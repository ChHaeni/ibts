
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

Rcpp::List mwIndices(int win_half, double delta_t,NumericVector x){
	Rcpp::List Out(x);
	int lenx = x.size() - 1;
	int win_size = win_half*2 + 1;
	double winh_time = win_half*delta_t;
	IntegerVector check_max = IntegerVector::create(0,-win_half);
	IntegerVector check_min = IntegerVector::create(lenx,win_half);

	// general
	for(int i = 0; i <= lenx; i++){		
		IntegerVector out(win_size);
		check_max[1] = i - win_half;
		check_min[1] = i + win_half;
		int lo = max(check_max);
		int hi = min(check_min);
		double lo_val = x[i] - winh_time;
		double hi_val = x[i] + winh_time;
		for(int run = lo; run <= hi; run++){
			if(x[run] > hi_val){
				break;
			}
			if(x[run] >= lo_val){
				out[run + win_half - i] = run + 1;
			}
		}
		Out[i] = out;
	}
	return Out;
}

