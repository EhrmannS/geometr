#' Transform a spatial object to class \code{ppp}
#'
#' @param input the object to transform to class \code{ppp}.
#' @return an object of class \code{ppp}
#' @family spatial classes
#' @examples
#' gc_ppp(input = gtGeoms$point)
#' @name gc_ppp
#' @rdname gc_ppp
NULL

# generic ----
#' @rdname gc_ppp
#' @name gc_ppp
#' @export
if(!isGeneric("gc_ppp")){
  setGeneric(name = "gc_ppp",
             def = function(input, ...){
               standardGeneric("gc_ppp")
             }
  )
}

# geom ----
#' @rdname gc_ppp
#' @importFrom spatstat ppp
#' @export
setMethod(f = "gc_ppp",
          signature = "geom",
          definition = function(input = NULL){

            theCoords <- getPoints(x = input)
            theData <- getFeatures(x = input)
            theData$fid <- NULL
            theData$gid <- NULL
            theWindow <- getWindow(x = input)
            theWindow <- owin(xrange = c(min(theWindow$x), max(theWindow$x)),
                              yrange = c(min(theWindow$y), max(theWindow$y)))

            out <- ppp(x = theCoords$x,
                       y = theCoords$y,
                       window = theWindow,
                       marks = theData)

            return(out)
         }
)
