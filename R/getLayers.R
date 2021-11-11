#' Get a specific layer of a spatial object.
#'
#' @param x the object from which to get the layer.
#' @param ... other arguments.
#' @return A list of the layers of \code{x}. Each list-item hast the result of
#'   getNames(x) as name.
#' @family getters
#' @examples
#'
#' getLayers(gtGeoms$line)
#'
#' gc_sp(gtGeoms$line) %>%
#'   getLayers()
#'
#' gc_sf(gtGeoms$line) %>%
#'   getLayers()
#'
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getLayers()
#'
#' gc_terra(gtGeoms$grid$categorical) %>%
#'   getLayers()
#'
#' getLayers(x = matrix(0, 3, 5))
#' @name getLayers
#' @rdname getLayers
NULL

# generic ----
#' @rdname getLayers
#' @name getLayers
#' @export
setGeneric(name = "getLayers",
           def = function(x, ...){
             standardGeneric("getLayers")
           }
)

# any ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "geom",
          definition = function(x){

            theType <- getType(x = x)[2]

            out <- NULL
            if(theType == "grid"){
              theFeatures <- getFeatures(x = x)
              theGroups <- getGroups(x = x)
              theNames <- getNames(x)

              if(is.data.frame(theGroups)){
                theGroups <- list(theGroups)
              }

              for(i in seq_along(theNames)){

                tempFeatures <- tibble(values = as.vector(theFeatures[["gid"]]))
                tempGroups <- theGroups[[i]]
                tempName <- theNames[i]

                temp <- new(Class = "geom",
                            type = x@type,
                            name = x@name,
                            point = x@point,
                            feature = tempFeatures,
                            group = tempGroups,
                            window = x@window,
                            crs = x@crs,
                            history = x@history)
                out <- c(out, list(temp))
              }

            } else {
              out <- list(x)
            }

            return(out)
          }
)

# matrix ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "Spatial",
          definition = function(x){
            list(x)
          }
)

# matrix ----
#' @rdname getLayers
#' @importFrom sf st_drop_geometry
#' @export
setMethod(f = "getLayers",
          signature = "sf",
          definition = function(x){
            allNames <- names(x)
            noGeom <- names(st_drop_geometry(x))
            geomName <- allNames[!allNames %in% noGeom]

            out <- setNames(list(x), geomName)
            out <- list(x)
            return(out)
          }
)

# raster ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "Raster",
          definition = function(x){

            # extract objects and assign history if that was set
            out <- lapply(1:dim(x)[3], function(y){
              t <- x[[y]]
              if(class(x) == "RasterBrick" & length(t@data@attributes) != 0){
                t@data@attributes <- t@data@attributes[[1]]
              }
              return(t)
            })
            # names(out) <- names(x)

            return(out)
          }
)

# terra ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "SpatRaster",
          definition = function(x){

            out <- lapply(1:dim(x)[3], function(y){
              t <- x[[y]]
              return(t)
            })
            # names(out) <- names(x)

            return(out)
          }
)

# matrix ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "matrix",
          definition = function(x){
            list(x)
          }
)
