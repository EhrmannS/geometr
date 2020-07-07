#' Transform a spatial object to class \code{Raster*}
#'
#' @param input the object to transform to class \code{Raster*}.
#' @return an object of class \code{Raster*}
#' @family spatial classes
#' @examples
#' rasGeom <- gc_geom(input = gtRasters$categorical)
#'
#' gc_raster(input = rasGeom)
#' @name gc_raster
#' @rdname gc_raster
NULL

# generic ----
#' @rdname gc_raster
#' @name gc_raster
#' @export
if(!isGeneric("gc_raster")){
  setGeneric(name = "gc_raster",
             def = function(input){
               standardGeneric("gc_raster")
             }
  )
}

# geom ----
#' @rdname gc_raster
#' @importFrom checkmate assertClass
#' @importFrom raster raster ratify
#' @importFrom stats setNames
#' @export
setMethod(f = "gc_raster",
          signature = "geom",
          definition = function(input = NULL){

            featureType <- getType(input)

            if(!all(featureType %in% c("raster", "grid"))){
              stop("Only objects of class 'geom' and type 'grid' can be transformed to a Raster*")
            } else {

              theFeatures <- getFeatures(x = input)
              theGroups <- getGroups(x = input)
              theCRS <- getCRS(x = input)

              hist <- list()
              if(!is.data.frame(theFeatures)){

                theNames <- names(theFeatures)
                theRasters <- list()
                for(i in seq_along(theFeatures)){

                  mat <- matrix(data = theFeatures[[i]]$values,
                                nrow = input@point$y[2],
                                ncol = input@point$x[2], byrow = TRUE)
                  out <- raster(x = mat, crs = theCRS,
                                xmn = input@point$x[1], xmx = input@point$x[1] + input@point$x[2]*input@point$x[3],
                                ymn = input@point$y[1], ymx = input@point$y[1] + input@point$y[2]*input@point$y[3])

                  if(dim(theGroups)[1] != 0){
                    out <- ratify(out)
                    out@data@attributes <- list(as.data.frame(theGroups[[i]]))
                  }
                  out <- setHistory(x = out, history = paste0("raster was transformed from an object of class geom."))

                  theRasters <- c(theRasters, setNames(object = list(out), nm = theNames[i]))
                }
                out <- stack(theRasters)

              } else {
                mat <- matrix(data = theFeatures$values,
                              nrow = input@point$y[2],
                              ncol = input@point$x[2], byrow = TRUE)
                out <- raster(x = mat, crs = theCRS,
                              xmn = input@point$x[1], xmx = input@point$x[1] + input@point$x[2]*input@point$x[3],
                              ymn = input@point$y[1], ymx = input@point$y[1] + input@point$y[2]*input@point$y[3])

                if(dim(theGroups)[1] != 0){
                  out <- ratify(out)
                  out@data@attributes <- list(as.data.frame(theGroups))
                }
                out <- setHistory(x = out, history = paste0("raster was transformed from an object of class geom."))
              }

            }

            return(out)
          }
)

