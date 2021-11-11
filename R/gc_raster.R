#' Transform a spatial object to class \code{Raster*}
#'
#' @param input the object to transform to class \code{Raster*}.
#' @return an object of class \code{Raster*}
#' @family spatial classes
#' @examples
#' gc_raster(input = gtGeoms$grid$categorical)
#' @name gc_raster
#' @rdname gc_raster
NULL

# generic ----
#' @rdname gc_raster
#' @name gc_raster
#' @export
setGeneric(name = "gc_raster",
           def = function(input){
             standardGeneric("gc_raster")
           }
)


# geom ----
#' @rdname gc_raster
#' @importFrom raster raster ratify
#' @importFrom stats setNames
#' @export
setMethod(f = "gc_raster",
          signature = "geom",
          definition = function(input = NULL){

            featureType <- getType(input)

            if(!all(featureType %in% c("grid"))){
              stop("Only objects of type 'grid' can be transformed to a Raster*")
            } else {

              theFeatures <- getFeatures(x = input)
              theGroups <- getGroups(x = input)
              theNames <- getNames(input)
              theCRS <- getCRS(x = input)

              if(is.data.frame(theGroups)){
                theGroups <- list(theGroups)
              }
              theRasters <- list()
              for(i in seq_along(theNames)){

                mat <- matrix(data = unlist(theFeatures["gid"], use.names = F),
                              nrow = input@point$y[2],
                              ncol = input@point$x[2], byrow = TRUE)
                out <- raster(x = mat, crs = theCRS,
                              xmn = input@point$x[1], xmx = input@point$x[1] + input@point$x[2]*input@point$x[3],
                              ymn = input@point$y[1], ymx = input@point$y[1] + input@point$y[2]*input@point$y[3])

                if(any(names(theGroups[[i]]) != "gid")){
                  out <- ratify(out)
                  out@data@attributes <- list(as.data.frame(theGroups[[i]]))
                }
                out <- setHistory(x = out, history = paste0("raster '", theNames[i], "' was transformed from an object of class geom."))

                theRasters <- c(theRasters, stats::setNames(object = list(out), nm = theNames[i]))
              }

              if(length(theRasters) > 1){
                out <- stack(theRasters)
              } else {
                out <- theRasters[[1]]
              }


            }

            return(out)
          }
)

