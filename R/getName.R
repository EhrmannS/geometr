#' Get the name(s) of a spatial object.
#'
#' @param x the object from which to get the name.
#' @return A vector of the requested names.
#' @family getters
#' @name getName
#' @rdname getName
NULL

# generic ----
#' @rdname getName
#' @name getName
#' @export
if(!isGeneric("getName")){
  setGeneric(name = "getName",
             def = function(x, ...){
               standardGeneric("getName")
             }
  )
}

# any ----
#' @rdname getName
#' @export
setMethod(f = "getName",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getName
#' @importFrom checkmate testNumeric assertIntegerish testCharacter assertSubset
#' @export
setMethod(f = "getName",
          signature = "geom",
          definition = function(x){

            out <- names(x@feature)

            return(out)
          }
)

# matrix ----
#' @rdname getName
#' @importFrom sf st_drop_geometry
#' @export
setMethod(f = "getName",
          signature = "sf",
          definition = function(x){
            allNames <- names(x)
            noGeom <- names(st_drop_geometry(x))
            out <- allNames[!allNames %in% noGeom]

            return(out)
          }
)

# RasterLayer ----
#' @rdname getName
#' @importFrom checkmate testNumeric assertIntegerish testCharacter assertSubset
#' @export
setMethod(f = "getName",
          signature = "Raster",
          definition = function(x){

            out <- names(x)

            return(out)
          }
)
