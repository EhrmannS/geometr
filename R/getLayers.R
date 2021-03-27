#' Get a specific layer of a spatial object.
#'
#' @param x the object from which to get the layer.
#' @param layer [\code{character(.)} | \code{integerish(.)}]\cr the layer(s) to
#'   get. If left at \code{NULL}, all layers are pulled.
#' @return A list of the layers of \code{x}. Each list-item hast the result of
#'   getNames(x) as name.
#' @family getters
#' @name getLayers
#' @rdname getLayers
NULL

# generic ----
#' @rdname getLayers
#' @name getLayers
#' @export
if(!isGeneric("getLayers")){
  setGeneric(name = "getLayers",
             def = function(x, layer = NULL, ...){
               standardGeneric("getLayers")
             }
  )
}

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
#' @importFrom checkmate testNumeric assertIntegerish testCharacter assertSubset
#' @export
setMethod(f = "getLayers",
          signature = "geom",
          definition = function(x, layer = NULL){

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

                tempFeatures <- tibble(values = as.vector(theFeatures[[theNames[i]]]))
                tempGroups <- theGroups[[i]]
                tempName <- theNames[i]

                temp <- new(Class = "geom",
                            type = x@type,
                            point = x@point,
                            feature = tempFeatures,
                            group = tempGroups,
                            window = x@window,
                            crs = x@crs,
                            history = x@history)
                out <- c(out, setNames(list(temp), tempName))
              }

            } else {
              out <- setNames(list(x), paste0(getType(x)[1], "_geom"))
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
            out <-list(x)
            return(out)
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
            return(out)
          }
)

# RasterLayer ----
#' @rdname getLayers
#' @importFrom checkmate testNumeric assertIntegerish testCharacter assertSubset
#' @export
setMethod(f = "getLayers",
          signature = "Raster",
          definition = function(x){

            # extract objects and assign history if that was set
            out <- lapply(1:dim(x)[3], function(y){
              t <- x[[y]]
              if(length(x@history) != 0){
                t@history <- x@history
              }
              return(t)
            })
            names(out) <- names(x)

            return(out)
          }
)

# matrix ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "matrix",
          definition = function(x){
            out <- list(x)
            return(out)
          }
)
