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
