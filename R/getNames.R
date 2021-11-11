#' Get the name(s) of a spatial object.
#'
#' @param x the object from which to get the name.
#' @param ... other arguments.
#' @return A vector of the names of \code{x}.
#' @family getters
#' @examples
#'
#' getNames(gtGeoms$line)
#' getNames(gtGeoms$grid$categorical)
#'
#' gc_sf(gtGeoms$line) %>%
#'   getNames()
#'
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getNames()
#'
#' gc_terra(gtGeoms$grid$categorical) %>%
#'   getNames()
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
#' @export
setMethod(f = "getNames",
          signature = "geom",
          definition = function(x){

            x@name

          }
)

# sf ----
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

# sp ----
#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "Spatial",
          definition = function(x){
            allNames <- names(x)

            return(c("sp_obj"))
          }
)

# raster ----
#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "Raster",
          definition = function(x){

            names(x)

          }
)

# terra ----
#' @rdname getNames
#' @export
setMethod(f = "getNames",
          signature = "SpatRaster",
          definition = function(x){

            names(x)

          }
)
