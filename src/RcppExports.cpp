// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// matInGeomC
NumericMatrix matInGeomC(NumericMatrix& mat, NumericMatrix& geom, bool negative);
RcppExport SEXP _geometr_matInGeomC(SEXP matSEXP, SEXP geomSEXP, SEXP negativeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type geom(geomSEXP);
    Rcpp::traits::input_parameter< bool >::type negative(negativeSEXP);
    rcpp_result_gen = Rcpp::wrap(matInGeomC(mat, geom, negative));
    return rcpp_result_gen;
END_RCPP
}
// pointInGeomC
IntegerVector pointInGeomC(NumericMatrix& vert, NumericMatrix& geom, bool invert);
RcppExport SEXP _geometr_pointInGeomC(SEXP vertSEXP, SEXP geomSEXP, SEXP invertSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type vert(vertSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type geom(geomSEXP);
    Rcpp::traits::input_parameter< bool >::type invert(invertSEXP);
    rcpp_result_gen = Rcpp::wrap(pointInGeomC(vert, geom, invert));
    return rcpp_result_gen;
END_RCPP
}
// getValuesMatC
NumericVector getValuesMatC(NumericMatrix& mat);
RcppExport SEXP _geometr_getValuesMatC(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(getValuesMatC(mat));
    return rcpp_result_gen;
END_RCPP
}
// subChrIntC
IntegerMatrix subChrIntC(CharacterMatrix& mat, CharacterVector& replace, IntegerVector& with);
RcppExport SEXP _geometr_subChrIntC(SEXP matSEXP, SEXP replaceSEXP, SEXP withSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterMatrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< CharacterVector& >::type replace(replaceSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type with(withSEXP);
    rcpp_result_gen = Rcpp::wrap(subChrIntC(mat, replace, with));
    return rcpp_result_gen;
END_RCPP
}
// sortUniqueC
NumericVector sortUniqueC(NumericVector x);
RcppExport SEXP _geometr_sortUniqueC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sortUniqueC(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_geometr_matInGeomC", (DL_FUNC) &_geometr_matInGeomC, 3},
    {"_geometr_pointInGeomC", (DL_FUNC) &_geometr_pointInGeomC, 3},
    {"_geometr_getValuesMatC", (DL_FUNC) &_geometr_getValuesMatC, 1},
    {"_geometr_subChrIntC", (DL_FUNC) &_geometr_subChrIntC, 3},
    {"_geometr_sortUniqueC", (DL_FUNC) &_geometr_sortUniqueC, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_geometr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
