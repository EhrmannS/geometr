#' Transform a spatial object to class \code{SpatRaster}
#'
#' @param input the object to transform to class \code{SpatRaster}.
#' @return an object of class \code{SpatRaster}
#' @family spatial classes
#' @examples
#' gc_terra(input = gtGeoms$grid$categorical)
#' @name gc_terra
#' @rdname gc_terra
NULL

# generic ----
#' @rdname gc_terra
#' @name gc_terra
#' @export
setGeneric(name = "gc_terra",
           def = function(input){
             standardGeneric("gc_terra")
           }
)


# geom ----
#' @rdname gc_terra
#' @importFrom terra rast levels
#' @export
setMethod(f = "gc_terra",
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

              out <- rast(ncols = input@point$x[2], nrows = input@point$y[2],
                          xmin = input@point$x[1], xmax = input@point$x[1] + input@point$x[2]*input@point$x[3],
                          ymin = input@point$y[1], ymax = input@point$y[1] + input@point$y[2]*input@point$y[3])
              out[] <- theFeatures[["gid"]]
              names(out) <- theNames

              if(any(names(theGroups) != "gid")){
                levels(out) <- as.data.frame(theGroups)
              }

            }

            return(out)
          }
)
