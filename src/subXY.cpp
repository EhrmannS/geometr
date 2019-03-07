#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix subChrIntC(CharacterMatrix &mat, CharacterVector &replace, IntegerVector &with){
  int mRows = mat.nrow(), mCols = mat.ncol(), posFocal;
  CharacterVector theValue;
  IntegerMatrix out(mRows, mCols);
  theValue = as<CharacterVector>(mat(0, 0));

  for(int x = 0; x < mCols; x++){
    for(int y = 0; y < mRows; y++){

      theValue = as<CharacterVector>(mat(y, x));
      if(!CharacterVector::is_na(theValue[0])){
        posFocal = match(theValue, replace)[0];
        out(y, x) = with[posFocal-1];
      } else{
        out(y, x) = NA_INTEGER;
      }
    }
  }

  return(out);
}

// NumericMatrix subNumNumC(NumericMatrix &mat, NumericVector &replace, NumericVector with){
//   int mRows = mat.nrow(), mCols = mat.ncol(), posFocal;
//   NumericVector theValue, newValue;
//   NumericMatrix out = clone(mat);
//
//   for(int x = 0; x < mCols; x++){
//     for(int y = 0; y < mRows; y++){
//
//       if(any(mat(y, x) == replace).is_true()){
//         theValue = mat(y, x);
//         posFocal = match(theValue, replace)[0];
//         int newValue = with[posFocal-1];
//         out(y, x) = newValue;
//       }
//
//     }
//   }
//   return(out);
// }

// CharacterMatrix subNumChrC(NumericMatrix &mat, NumericVector &replace, CharacterVector with){
//   int mRows = mat.nrow(), mCols = mat.ncol(), posFocal;
//   NumericVector theValue;
//   CharacterMatrix out(mRows, mCols);
//
//   for(int x = 0; x < mCols; x++){
//     for(int y = 0; y < mRows; y++){
//
//       if(any(mat(y, x) == replace).is_true()){
//         theValue = mat(y, x);
//         posFocal = match(theValue, replace)[0];
//         out(y, x) = with[posFocal-1];
//       } else{
//         out(y, x) = NA_STRING;
//       }
//
//     }
//   }
//   return(out);
// }
