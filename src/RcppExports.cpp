// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cutIntervals
Rcpp::List cutIntervals(NumericVector x1, NumericVector x2, NumericVector y1, NumericVector y2);
RcppExport SEXP _ibts_cutIntervals(SEXP x1SEXP, SEXP x2SEXP, SEXP y1SEXP, SEXP y2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y2(y2SEXP);
    rcpp_result_gen = Rcpp::wrap(cutIntervals(x1, x2, y1, y2));
    return rcpp_result_gen;
END_RCPP
}
// findI_et
IntegerVector findI_et(NumericVector x, NumericVector y1, NumericVector y2);
RcppExport SEXP _ibts_findI_et(SEXP xSEXP, SEXP y1SEXP, SEXP y2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y2(y2SEXP);
    rcpp_result_gen = Rcpp::wrap(findI_et(x, y1, y2));
    return rcpp_result_gen;
END_RCPP
}
// findI_st
IntegerVector findI_st(NumericVector x, NumericVector y1, NumericVector y2);
RcppExport SEXP _ibts_findI_st(SEXP xSEXP, SEXP y1SEXP, SEXP y2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y2(y2SEXP);
    rcpp_result_gen = Rcpp::wrap(findI_st(x, y1, y2));
    return rcpp_result_gen;
END_RCPP
}
// mwIndices
Rcpp::List mwIndices(int win_half, double delta_t, NumericVector x);
RcppExport SEXP _ibts_mwIndices(SEXP win_halfSEXP, SEXP delta_tSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type win_half(win_halfSEXP);
    Rcpp::traits::input_parameter< double >::type delta_t(delta_tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(mwIndices(win_half, delta_t, x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ibts_cutIntervals", (DL_FUNC) &_ibts_cutIntervals, 4},
    {"_ibts_findI_et", (DL_FUNC) &_ibts_findI_et, 3},
    {"_ibts_findI_st", (DL_FUNC) &_ibts_findI_st, 3},
    {"_ibts_mwIndices", (DL_FUNC) &_ibts_mwIndices, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_ibts(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
