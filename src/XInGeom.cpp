#include <Rcpp.h>
using namespace Rcpp;

// http://geomalgorithms.com/a03-_inclusion.html
// Copyright 2000 softSurfer, 2012 Dan Sunday
// This code may be freely used and modified for any purpose
// providing that this copyright notice is included with it.
// SoftSurfer makes no warranty for this code, and cannot be held
// liable for any real or imagined damage resulting from its use.
// Users of this code must verify correctness for their application.

// [[Rcpp::export]]
NumericMatrix matInGeomC(NumericMatrix &mat, NumericMatrix &geom, bool negative){
  int mRows = mat.nrow(), mCols = mat.ncol();
  int cRows = geom.nrow();
  int isLeft, inside, outside;
  NumericMatrix out = clone(mat);
  NumericMatrix vert = clone(geom);
  vert(_, 0) = vert(_, 0);
  vert(_, 1) = mRows - vert(_, 1);
  if(negative){
    inside = 0;
    outside = 1;
  } else{
    inside = 1;
    outside = 0;
  }

  // get bounding box of geom
  int xMin = min(vert(_, 0)), xMax = max(vert(_, 0));
  int yMin = min(vert(_, 1)), yMax = max(vert(_, 1));

  // warning if first and last coordinate are not the same
  if(any(vert(0, _) != vert(cRows-1, _)).is_true()){
    stop("first and last cooridnate are not the same!");
  }

  for(int x = 0; x < mCols; x++){
    for(int y = 0; y < mRows; y++){

      // if the coordinate is within the bounding box, proceed, otherwise value is definitely 0
      if((x < xMax) & (x >= xMin) & (y < yMax) & (y >= yMin)){
        int wn = 0;                            // the  winding number counter

        // loop through all edges of the polygon and find wn
        for (int i = 0; i < cRows-1; i++){

          if (y >= vert(i, 1)){
            if (y < vert(i+1, 1)){             // an upward crossing
              isLeft = (vert(i+1, 0) - vert(i, 0)) * (y - vert(i, 1)) - (x -  vert(i, 0)) * (vert(i+1, 1) - vert(i, 1));
              if(isLeft > 0){                  // P left of edge
                ++wn;                          // have  a valid up intersect
              }
            }
          } else {
            if (y >= vert(i+1, 1)){            // a downward crossing
              isLeft = (vert(i+1, 0) - vert(i, 0)) * (y - vert(i, 1)) - (x -  vert(i, 0)) * (vert(i+1, 1) - vert(i, 1));
              if(isLeft < 0){                  // P right of edge
                --wn;                          // have  a valid down intersect
              }
            }
          }

        }

        if(wn == 0){
          out(y, x) = outside;
        } else{
          out(y, x) = inside;
        }
      } else{
        out(y, x) = outside;
      }
    }
  }

  return(out);
}
