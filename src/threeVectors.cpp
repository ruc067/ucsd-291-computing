#include <Rcpp.h>
using namespace Rcpp;

void print3Values( int i, int ix, int iy, int iz, double xelt, double yelt, double zelt) {
  Rprintf("i = %d ix = %d x element = %f iy = %d y element = %f iz = %d z element = %f\n",
          i, ix, xelt, iy, yelt, iz, zelt);
}
// [[Rcpp::export]]
CharacterVector threeVectors(
    NumericVector x, NumericVector y, NumericVector z) {
  int xn = x.size();
  int yn = y.size();
  int zn = z.size();
  int n1 = xn > yn ? xn : yn;
  int n = n1 > zn ? n1 : zn;
  int ix=0, iy=0, iz=0;
  for (int i = 0; i<n; ++i) {
    // Do something here:
    print3Values( i, ix, iy, iz, x[ix], y[iy], z[iz]);
    // Update the indexes for the next pass:
    if (++ix == xn ) ix = 0 ;
    if (++iy == yn ) iy = 0;
    if (++iz == zn ) iz = 0;
  }
  return "Done";
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/
