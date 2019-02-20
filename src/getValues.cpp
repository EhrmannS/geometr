#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getValuesMatC(NumericMatrix &mat) {
  int mRows = mat.nrow(), mCols = mat.ncol();
  NumericVector values;
  
  for(int y = 0; y < mRows; y++){
    for(int x = 0; x < mCols; x++){
      values.push_back(mat(y, x));
    }
  }
  return(values);
}