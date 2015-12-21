#include <netcdf.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_NAME_LEN 128

/* Handle errors by printing an error message and exiting with a
 * non-zero status.
 */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

int main(int argc, char* argv[])
{
   char uFile[MAX_NAME_LEN], arcFile[MAX_NAME_LEN];
   size_t nx, ny, nz, num_arcx, num_arcy;
   double *u, *arcList, *arcDist;

   /* This will be the netCDF ID for the file, dimension, and data variable. */
   int ncid, dimid, varid;

   /* Loop indexes, and error handling. */
   int i, retval;

   uFile[0] = '\0';
   for (i=1; i < argc; i++) {
      /* file containing slowness data */
      if (strncmp("-u", argv[i], 3) == 0) {
         if (++i == argc) break;
         strncpy(uFile, argv[i], MAX_NAME_LEN);
      }
      /* file containing arcList and arcDist data */
      if (strncmp("-a", argv[i], 3) == 0) {
         if (++i == argc) break;
         strncpy(arcFile, argv[i], MAX_NAME_LEN);
      }
   }
   if (strlen(uFile) < 1 || strlen(arcFile) < 1) {
      printf("usage: dijkstra -a arc_file_name -u u_file_name\n");
      exit(1);
   }

   /* Open and process the slowness file.
    */
   if ((retval = nc_open(uFile, NC_NOWRITE, &ncid)))               ERR(retval);
   printf("opened file %s\n", uFile);

   /* Get the dimensions of the slowness data u
    */
   if ((retval = nc_inq_dimid (ncid, "x", &dimid)))                ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &nx )))                ERR(retval);
   printf("dimid and size for dimension x: %d %ld\n", dimid, nx);
   if ((retval = nc_inq_dimid (ncid, "y", &dimid)))                ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &ny )))                ERR(retval);
   printf("dimid and size for dimension y: %d %ld\n", dimid, ny);
   if ((retval = nc_inq_dimid (ncid, "z", &dimid)))                ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &nz )))                ERR(retval);
   printf("dimid and size for dimension z: %d %ld\n", dimid, nz);

   /* Get the varid of the data variable, based on its name.
    */
   if ((retval = nc_inq_varid(ncid, "u", &varid)))                 ERR(retval);

   /* Allocate and read the data
    */
   u = (double *) malloc(nx*ny*nz * sizeof(double));
   if (u == NULL) {
      printf("ERROR: failed to allocate slowness data\n");
      nc_close(ncid);
      exit(1);
   }
   if ((retval = nc_get_var_double(ncid, varid, u)))               ERR(retval);

   /* Close the file */
   if ((retval = nc_close(ncid)))                                  ERR(retval);

   printf("SUCCESS: finished reading file %s\n", uFile);

   /* Open and process file containing arcList and arcDist.
    */
   if ((retval = nc_open(arcFile, NC_NOWRITE, &ncid)))             ERR(retval);
   printf("opened file %s\n", arcFile);

   /* Get the dimensions of the arcList and arcDist data
    */
   if ((retval = nc_inq_dimid (ncid, "x",   &dimid)))              ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &num_arcx)))           ERR(retval);
   printf("dimid and size for dimension x: %d %ld\n", dimid, num_arcx);
   if ((retval = nc_inq_dimid (ncid, "y",   &dimid)))              ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &num_arcy)))           ERR(retval);
   printf("dimid and size for dimension y: %d %ld\n", dimid, num_arcy);

   /* Allocate and read arcList data
    */
   arcList = (double *) malloc(num_arcx*num_arcy * sizeof(double));
   if (arcList == NULL) {
      printf("ERROR: failed to allocate arcList data\n");
      nc_close(ncid);
      exit(1);
   }
   if ((retval = nc_inq_varid(ncid, "arcList", &varid)))           ERR(retval);
   if ((retval = nc_get_var_double(ncid, varid, arcList)))         ERR(retval);

   /* Allocate and read arcDist data
    */
   arcDist = (double *) malloc(num_arcx*num_arcy * sizeof(double));
   if (arcDist == NULL) {
      printf("ERROR: failed to allocate arcDist data\n");
      nc_close(ncid);
      exit(1);
   }
   if ((retval = nc_inq_varid(ncid, "arcDist", &varid)))           ERR(retval);
   if ((retval = nc_get_var_double(ncid, varid, arcDist)))         ERR(retval);

   /* Close the file */
   if ((retval = nc_close(ncid)))                                  ERR(retval);

   printf("SUCCESS: finished reading file %s\n", uFile);

   /*
    * Do dijkstra stuff
    */

   return 0;
}
