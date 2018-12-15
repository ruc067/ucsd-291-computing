#include <Rcpp.h>
using namespace Rcpp;

double paretodens(double x, double alpha, double beta, bool logd = false) {
  if (alpha <= 0.0 | beta <= 0.0) return NAN;
  if (x < alpha){
    return 0;
  } 
  if (logd){
    return log(beta*pow(alpha,beta)/pow(x,(beta+1)));
  } else{
    return beta*pow(alpha,beta)/pow(x,(beta+1));
  }
}

// [[Rcpp::export(name = .dpareto)]]
NumericVector dpareto(NumericVector x, NumericVector y, NumericVector z, bool logd = false) {
  int xn = x.size();
  int yn = y.size();
  int zn = z.size();
  int n1 = xn > yn ? xn : yn;
  int n = n1 > zn ? n1 : zn;
  int ix=0, iy=0, iz=0;
  NumericVector paretodensity(n);
  for (int i = 0; i<n; ++i) {
    paretodensity[i] = paretodens(x[ix], y[iy], z[iz], logd);
    if (++ix == xn ) ix = 0 ;
    if (++iy == yn ) iy = 0;
    if (++iz == zn ) iz = 0;
  }
  return paretodensity;
}