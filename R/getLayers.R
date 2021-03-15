#' Get a specific layer of a spatial object.
#'
#' @param x the object from which to get the layer.
#' @param layer [\code{character(.)} | \code{integerish(.)}]\cr the layer(s) to get.
#' @return A list of the requested layers.
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

            if(is.null(layer)){
              layer <- seq_along(x@feature)
            }

            out <- NULL
            if(theType == "grid"){

              theFeatures <- getFeatures(x = x)
              if(!is.null(dim(theFeatures))){
                theFeatures <- setNames(list(theFeatures), names(x@feature))
              }
              theGroups <- getGroups(x = x)
              if(!is.null(dim(theGroups))){
                theGroups <- setNames(list(theGroups), names(x@feature))
              }
              theNames <- names(theFeatures)
              for(i in seq_along(layer)){

                if(testNumeric(x = layer)){
                  assertIntegerish(x = layer, lower = 1, upper = length(theFeatures))
                  tempFeatures <- setNames(list(tibble(values = as.vector(theFeatures[[layer[i]]]$values))), theNames[i])
                  tempGroups <- setNames(list(theGroups[[layer[i]]]), theNames[i])
                  tempName <- theNames[i]
                } else if(testCharacter(x = layer)){
                  assertSubset(x = layer, choices = theNames)
                  tempFeatures <- setNames(list(tibble(values = as.vector(theFeatures[[which(theNames %in% layer[i])]]$values))), theNames[i])
                  tempGroups <- setNames(list(theGroups[[which(theNames %in% layer[i])]]), theNames[i])
                  tempName <- theNames[which(theNames %in% layer[i])]
                }

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
              out <- c(out, setNames(list(x), "geometry"))
            }

            return(out)
          }
)

# matrix ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "Spatial",
          definition = function(x, layer = NULL){
            out <- setNames(list(x), "a spatial object")
            return(out)
          }
)

# matrix ----
#' @rdname getLayers
#' @importFrom sf st_drop_geometry
#' @export
setMethod(f = "getLayers",
          signature = "sf",
          definition = function(x, layer = NULL){
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
          definition = function(x, layer = NULL){

            if(is.null(layer)){
              layer <- 1:dim(x)[3]
            }

            # extract objects and assign history if that was set
            tempRas <- lapply(1:dim(x)[3], function(y){
              t <- x[[y]]
              if(length(x@history) != 0){
                t@history <- x@history
              }
              return(t)
            })
            theNames <- names(x)

            out <- NULL
            for(i in seq_along(layer)){

              if(testNumeric(x = layer)){
                assertIntegerish(x = layer, lower = 1, upper = length(tempRas))
                temp <- tempRas[[layer[i]]]
                tempName <- theNames[layer[i]]
              } else if(testCharacter(x = layer)){
                assertSubset(x = layer, choices = theNames)
                temp <- tempRas[[which(theNames %in% layer[i])]]
                tempName <- theNames[which(theNames %in% layer[i])]
              }
              out <- c(out, setNames(list(temp), tempName))

            }
            return(out)
          }
)

# matrix ----
#' @rdname getLayers
#' @export
setMethod(f = "getLayers",
          signature = "matrix",
          definition = function(x, layer = NULL){
            out <- setNames(list(x), "a matrix")
            return(out)
          }
)
