#include <netcdf.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Handle errors by printing an error message and exiting with a
 * non-zero status.
 */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

int main(int argc, char* argv[])
{
   char filename[128];
   size_t nx, ny, nz;
   double * u;

   /* This will be the netCDF ID for the file, dimension, and data variable. */
   int ncid, dimid, varid_u;

   /* Loop indexes, and error handling. */
   int i, retval;

   filename[0] = '\0';
   for (i=1; i < argc; i++) {
      if (strncmp("-i", argv[i], 3) == 0) {
         if (++i == argc) break;
         strncpy(filename, argv[i], 128);
      }
   }
   if (strlen(filename) < 1) {
      printf("usage: dijkstra -i filename\n");
      exit(1);
   }
   else {
      /* Open the file. */
      if ((retval = nc_open(filename, NC_NOWRITE, &ncid)))         ERR(retval);
      printf("opened file %s\n", filename);
   }

   /* Get the dimensions of the slowness data u
    */
   if ((retval = nc_inq_dimid (ncid, "x", &dimid)))                ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &nx )))                ERR(retval);
   printf("dimid and size for dimension x: %d %ld\n", dimid, nx);
   if ((retval = nc_inq_dimid(ncid, "y", &dimid)))                 ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &ny )))                ERR(retval);
   printf("dimid and size for dimension y: %d %ld\n", dimid, ny);
   if ((retval = nc_inq_dimid(ncid, "z", &dimid)))                 ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &nz )))                ERR(retval);
   printf("dimid and size for dimension z: %d %ld\n", dimid, nz);

   /* Get the varid of the data variable, based on its name.
    */
   if ((retval = nc_inq_varid(ncid, "u", &varid_u)))               ERR(retval);

   /* Allocate and read the data
    */
   u = (double *) malloc(nx*ny*nz * sizeof(double));
   if (u == NULL) {
      printf("ERROR: failed to allocate slowness data\n");
      nc_close(ncid);
      exit(1);
   }
   if ((retval = nc_get_var_double(ncid, varid_u, u)))             ERR(retval);

   /* Close the file, freeing all resources. */
   if ((retval = nc_close(ncid)))                                  ERR(retval);

   printf("*** SUCCESS reading file %s!\n", filename);

   /*
    * Do dijkstra stuff
    */

   return 0;
}
