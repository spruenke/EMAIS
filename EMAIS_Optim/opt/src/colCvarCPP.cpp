#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector colCVar(NumericMatrix a, int nr, int nc, double alpha){
   NumericVector cvar(nc);
  
  for(int i = 0; i < nc; i++){
    NumericVector x = a( _ , i);
    double ind = 1 + (nr - 1) * alpha;
    int lo = floor(ind);
    int hi = ceil(ind);
    x.sort();
    double qs = x[lo-1];
    double h = ind - lo;
    double colQ = (((1 - h) * qs) + (h * x[hi-1]));
Environment base("package:base");
  Function mean = base["mean"];
    cvar[i] = as<double>(mean(x[x >= colQ]));
  }
  
  return cvar;  
}
