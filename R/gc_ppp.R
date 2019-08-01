#' Transform a spatial object to class \code{ppp}
#'
#' @param input the object to transform to class \code{ppp}.
#' @return an object of class \code{ppp}
#' @family spatial classes
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
#' @examples
#' (pppPoints <- gc_ppp(input = gtGeoms$point))
#' @importFrom tibble tibble
#' @export
setMethod(f = "gc_ppp",
          signature = "geom",
          definition = function(input = NULL){

            theCoords <- getVertices(x = input)
            theData <- getTable(x = input, slot = "feat")
            theGroups <- getTable(x = input, slot = "group")
            theVertices <- getTable(x = input, slot = "vert")
            theCRS <- getCRS(x = input)
            bbox <- getExtent(x = input)
            theWindow = tibble(x = c(min(bbox$x), max(bbox$x), max(bbox$x), min(bbox$x), min(bbox$x)),
                               y = c(min(bbox$y), min(bbox$y), max(bbox$y), max(bbox$y), min(bbox$y)))

         }
)