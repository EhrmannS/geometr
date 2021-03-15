#' Get the name(s) of a spatial object.
#'
#' @param x the object from which to get the name.
#' @return A vector of the requested names.
#' @family getters
#' @name getNames
#' @rdname getNames
NULL

# generic ----
#' @rdname getNames
#' @name getNames
#' @export
if(!isGeneric("getNames")){
  setGeneric(name = "getNames",
             def = function(x, ...){
               standardGeneric("getNames")
             }
  )
}

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
#' @export
setMethod(f = "getNames",
          signature = "geom",
          definition = function(x){

            out <- names(x@feature)

            return(out)
          }
)

# matrix ----
#' @rdname getNames
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
#' @importFrom checkmate testNumeric assertIntegerish testCharacter assertSubset
#' @export
setMethod(f = "getNames",
          signature = "Raster",
          definition = function(x){

            out <- names(x)

            return(out)
          }
)
