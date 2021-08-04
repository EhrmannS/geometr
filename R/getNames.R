#' Get the name(s) of a spatial object.
#'
#' @param x the object from which to get the name.
#' @param ... other arguments.
#' @return A vector of the names of \code{x}.
#' @family getters
#' @name getNames
#' @rdname getNames
NULL

# generic ----
#' @rdname getNames
#' @name getNames
#' @export
setGeneric(name = "getNames",
           def = function(x, ...){
             standardGeneric("getNames")
           }
)

# any ----
#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getNames
#' @examples
#' getNames(x = gtGeoms$grid$continuous)
#' @export
setMethod(f = "getNames",
          signature = "geom",
          definition = function(x){

            theFeatures <- x@feature
            if(all(c("val", "len") %in% names(theFeatures))){
              out <- "values"
            } else {
              out <- names(theFeatures)
              out <- out[!out %in% c("fid", "gid")]

              if(length(out) == 0){
                out <- paste0(getType(x = x)[1], "_geom")
              }
            }

            return(out)
          }
)

# sf ----
#' @rdname getNames
#' @examples
#'
#' getNames(x = gtSF$polygon)
#' @importFrom sf st_drop_geometry
#' @export
setMethod(f = "getNames",
          signature = "sf",
          definition = function(x){
            allNames <- names(x)
            noGeom <- names(st_drop_geometry(x))
            out <- allNames[!allNames %in% noGeom]

            return(out)
          }
)

# RasterLayer ----
#' @rdname getNames
#' @examples
#'
#' getNames(x = gtRasters)
#' @importFrom checkmate testNumeric assertIntegerish testCharacter assertSubset
#' @export
setMethod(f = "getNames",
          signature = "Raster",
          definition = function(x){

            out <- names(x)

            return(out)
          }
)
