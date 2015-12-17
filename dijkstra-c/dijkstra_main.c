#include <stdlib.h>
#include <stdio.h>
#include <netcdf.h>

#define FILE_NAME "srModel_forTomographyPaper.nc"

/* This really should be read from the netCDF file and memory dynamically allocated */
#define NX 241
#define NY 241
#define NZ 51

/* Handle errors by printing an error message and exiting with a
 * non-zero status.
 */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

int main(int argc, char* argv[])
{
   size_t nx, ny, nz;

   /* This will be the netCDF ID for the file, dimension, and data variable. */
   int ncid, dimid, varid_u;

   double * u;

   /* Loop indexes, and error handling. */
   int x, y, retval;

   /* Open the file. */
   if ((retval = nc_open(FILE_NAME, NC_NOWRITE, &ncid)))          ERR(retval);

   /* Get the dimensions of the slowness data u
    */
   if ((retval = nc_inq_dimid (ncid, "x", &dimid)))               ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &nx )))               ERR(retval);
   printf("dimid and size for dimension x: %d %ld\n", dimid, nx);
   if ((retval = nc_inq_dimid(ncid, "y", &dimid)))                ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &ny )))               ERR(retval);
   printf("dimid and size for dimension y: %d %ld\n", dimid, ny);
   if ((retval = nc_inq_dimid(ncid, "z", &dimid)))                ERR(retval);
   if ((retval = nc_inq_dimlen(ncid, dimid, &nz )))               ERR(retval);
   printf("dimid and size for dimension z: %d %ld\n", dimid, nz);

   /* Get the varid of the data variable, based on its name.
    */
   if ((retval = nc_inq_varid(ncid, "u", &varid_u)))              ERR(retval);

   /* Allocate and read the data
    */
   u = (double *) malloc(nx*ny*nz * sizeof(double));
   if (u == NULL) {
      printf("ERROR: failed to allocate slowness data\n");
      nc_close(ncid);
      exit(1);
   }
   if ((retval = nc_get_var_double(ncid, varid_u, u)))            ERR(retval);

   /* Close the file, freeing all resources. */
   if ((retval = nc_close(ncid)))                                 ERR(retval);

   printf("*** SUCCESS reading file %s!\n", FILE_NAME);

   /*
    * Do dijkstra stuff
    */

   return 0;
}
