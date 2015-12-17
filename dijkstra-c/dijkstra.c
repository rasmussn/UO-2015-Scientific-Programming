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
   /* This will be the netCDF ID for the file and data variable. */
   int ncid, varid;

   int u[NX][NY][NZ];

   /* Loop indexes, and error handling. */
   int x, y, retval;

   /* Open the file. NC_NOWRITE tells netCDF we want read-only access
    * to the file.
    */
   if ((retval = nc_open(FILE_NAME, NC_NOWRITE, &ncid))) {
      ERR(retval);
   }

   /* Get the varid of the data variable, based on its name. */
   if ((retval = nc_inq_varid(ncid, "u", &varid))) {
      ERR(retval);
   }

   /* Read the data. */
   if ((retval = nc_get_var_int(ncid, varid, &u[0][0][0]))) {
      ERR(retval);
   }

   /* Close the file, freeing all resources. */
   if ((retval = nc_close(ncid))) {
      ERR(retval);
   }

   /*
    * Do dijkstra stuff
    */

   printf("*** SUCCESS reading example file %s!\n", FILE_NAME);

   return 0;
}
