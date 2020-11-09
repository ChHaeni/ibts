
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

IntegerVector findI_st(NumericVector x,NumericVector y1,NumericVector y2){
	// case: st_closed = TRUE
	int lenx = x.size();
	IntegerVector Out = rep(0,lenx);
	int leny = y1.size() - 1;
	int run = 0;

	for(int i = 0; (i < lenx) & (run <= leny); i++){
		if(x[i] >= y2[leny]){
			break;
		}
		while((y2[run] <= x[i]) & (run <= leny)){
			run += 1;
		}
		if(y1[run] <= x[i]){
			Out[i] = run + 1;
		}
	}
	return Out;
}
