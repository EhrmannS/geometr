#' Get the spatial resolution of a spatial object.
#'
#' @param x the object from which to derive the resolution.
#' @return The resolution of \code{x} in x and y dimension.
#' @family getters
#' @name getRes
#' @rdname getRes
NULL

# generic ----
#' @rdname getRes
#' @name getRes
#' @export
if(!isGeneric("getRes")){
  setGeneric(name = "getRes",
             def = function(x, ...){
               standardGeneric("getRes")
             }
  )
}

# any ----
#' @rdname getRes
#' @export
setMethod(f = "getRes",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getRes
#' @importFrom tibble tibble
#' @export
setMethod(f = "getRes",
          signature = "geom",
          definition = function(x){

            temp <- x
            if(temp@type == "grid"){
              out <- tibble(x = c(temp@point$x[3]), y = c(temp@point$y[3]))
            } else {
              out <- NULL
            }

            return(out)
          }
)

# Raster* ----
#' @rdname getRes
#' @importFrom tibble tibble
#' @importFrom raster res
#' @export
setMethod(f = "getRes",
          signature = "Raster",
          definition = function(x){
            temp <- res(x)
            out <- tibble(x = temp[1], y = temp[2])
            return(out)
          }
)

# matrix ----
#' @rdname getRes
#' @importFrom tibble tibble
#' @export
setMethod(f = "getRes",
          signature = "matrix",
          definition = function(x){
            out <- tibble(x = 1, y = 1)
            return(out)
          }
)
