
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

Rcpp::List cutIntervals(NumericVector x1,NumericVector x2,NumericVector y1,NumericVector y2)
	{
	Rcpp::List Out(y1);
	IntegerVector Dummy1 = seq_len(x1.size());
	NumericVector Dummy2 = as<NumericVector>(Dummy1);
	NumericMatrix MatOut = cbind(Dummy2,x1);
	double dy12i;
	int lenx = x1.size() - 1;
	int leny = y1.size();
	int run = 0;
	int i1;

	for(int i = 0; i < leny; i++){
		dy12i = y2[i] - y1[i];
		if((x2[lenx] < y1[i]) || (x1[run] > y2[i])){
			Out[i] = R_NilValue;
		} else {
			while((x2[run] <= y1[i]) && (run < lenx)){
				run += 1;
			}
			i1 = run;
			if((run <= lenx) && (x1[run] < y1[i])){
				MatOut(run,1) = (min(NumericVector::create(x2[run],y2[i])) - y1[i])/dy12i;
				run += 1; 
			}
			while((run <= lenx) && (x2[run] < y2[i])){
				MatOut(run,1) = (x2[run] - x1[run])/dy12i;
				run += 1; 
			}
			if((run <= lenx) && (x1[run] < y2[i])){
				MatOut(run,1) = (y2[i] - x1[run])/dy12i;
				run += 1; 
			}
			run -= 1;
			if(i1 > run){
				Out[i] = R_NilValue;
			} else {
				Out[i] = MatOut(seq(i1, run),_);
			}
		}
	}
	return(Out);
}

