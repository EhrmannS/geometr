#' Set (or transform) the coordinate reference system of a spatial object.
#'
#' @details In case an object does not yet have a coordinate reference system
#'   assigned, this function simply assigns it. In case the object has already a
#'   valid crs, a transformation to the new crs will be carried out. The
#'   transformation is computed for all classes with the standard defined in the
#'   \code{rgdal} package.
#' @param x the object for which to set the coordinate reference system.
#' @param crs [\code{character(1)}]\cr the coordinate reference system to set
#'   for this object.
#' @return The object \code{x} with an assigned or transformed coordinate
#'   reference system.
#' @family setters
#' @name setCRS
#' @rdname setCRS
NULL

# generic ----
#' @rdname setCRS
#' @name setCRS
#' @docType methods
#' @export
if(!isGeneric("setCRS")){
  setGeneric(name = "setCRS",
             def = function(x, crs, ...){
               standardGeneric("setCRS")
             }
  )
}

# any ----
#' @rdname setCRS
#' @export
setMethod(f = "setCRS",
          signature = "ANY",
          definition = function(x){
            warning(paste0("I can't set a history to an object of class '", paste0(class(x), collapse = ", "), "'."))
          }
)

# geom ----
#' @rdname setCRS
#' @importFrom rgdal project
#' @export
setMethod(f = "setCRS",
          signature = "geom",
          definition = function(x, crs = NULL){
            if(is.na(x@crs)){
              x@crs <- crs
            } else{
              theCoords <- x@point[which(names(x@point) %in% c("x", "y"))]
              if(!all(c("+proj=longlat", "+ellps=WGS84") %in% strsplit(x@crs, " ")[[1]])){
                geographic <- project(as.matrix(theCoords), proj = as.character(x@crs), inv = TRUE)
              } else{
                geographic <- as.matrix(theCoords)
              }
              if(crs != "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"){
                projected <- project(geographic, proj = as.character(crs))
              } else{
                projected <- geographic
              }
              x@point <- data.frame(projected, x@point[which(!names(x@point) %in% c("x", "y"))])
              x@crs <- crs
              x <- setWindow(x = x, to = getExtent(x))
            }
            x@history <- c(getHistory(x = x), list(paste0("the crs was set to '", crs, "'.")))
            return(x)
          }
)

# Spatial ----
#' @rdname setCRS
#' @importFrom raster crs
#' @importFrom sp spTransform
#' @export
setMethod(f = "setCRS",
          signature = signature("Spatial"),
          definition = function(x, crs = NULL){
            if(is.na(x@proj4string)){
              x@proj4string <- crs(crs)
            } else{
              x <- spTransform(x, CRSobj = crs(crs))
            }
            return(x)
          }
)

# sf ----
#' @rdname setCRS
#' @importFrom sf st_set_crs st_transform
#' @export
setMethod(f = "setCRS",
          signature = "sf",
          definition = function(x, crs = NULL){
            if(is.na(st_crs(x = x)$proj4string)){
              x <- st_set_crs(x = x, value = crs)
            } else{
              x <- st_transform(x, crs = crs)
            }
            return(x)
          }
)

# Raster ----
#' @rdname setCRS
#' @importFrom raster crs projectRaster
#' @export
setMethod(f = "setCRS",
          signature = "Raster",
          definition = function(x, crs = NULL){
            if(is.na(x@crs)){
              x@crs <- crs(crs)
            } else{
              x <- projectRaster(from = x, crs = crs)
            }
            x@history <- c(getHistory(x = x), list(paste0("the crs was set to '", crs, "'.")))
            return(x)
          }
)