#' Set (or transform) the coordinate reference system of a spatial object.
#' @details In case an object has not yet assigned a coordinate reference
#'   system, this function simply assigns it. In case the object has already a
#'   valid crs, a transformation to the new crs will be carried out. The
#'   transformation is computed with the standard defined in the \code{rgdal}
#'   package.
#' @param x the object for which to set the coordinate reference system.
#' @param crs [\code{character(1)}]\cr the coordinate reference system to set
#'   for this object.
#' @name setCRS
#' @rdname setCRS
NULL

#' @rdname setCRS
#' @docType methods
#' @export
if(!isGeneric("setCRS")){
  setGeneric(name = "setCRS",
             def = function(x, crs, ...){
               standardGeneric("setCRS")
             }
  )
}

#' @rdname setCRS
#' @importFrom rgdal project
#' @export
setMethod(f = "setCRS",
          signature = "geom",
          definition = function(x, crs){
            if(is.na(x@crs)){
              x@crs <- crs
            } else{
              theCoords <- x@coords[which(names(x@coords) %in% c("x", "y"))]
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
              x@coords <- data.frame(projected, x@coords[which(!names(x@coords) %in% c("x", "y"))])
              x@crs <- crs
              x <- setWindow(x = x, to = getExtent(x))
            }
            return(x)
          }
)

#' @rdname setCRS
#' @importFrom raster crs
#' @importFrom sp spTransform
#' @export
setMethod(f = "setCRS",
          signature = signature("Spatial"),
          definition = function(x, crs){
            if(is.na(x@proj4string)){
              x@proj4string <- crs(crs)
            } else{
              x <- spTransform(x, CRSobj = crs(crs))
            }
            return(x)
          }
)

#' @rdname setCRS
#' @importFrom sf st_set_crs st_transform
#' @export
setMethod(f = "setCRS",
          signature = "sf",
          definition = function(x, crs){
            if(is.na(st_crs(x = x)$proj4string)){
              x <- st_set_crs(x = x, value = crs)
            } else{
              x <- st_transform(x, crs = crs)
            }
            return(x)
          }
)

#' @rdname setCRS
#' @importFrom raster crs projectRaster
#' @export
setMethod(f = "setCRS",
          signature = "Raster",
          definition = function(x, crs){
            if(is.na(x@crs)){
              x@crs <- crs(crs)
            } else{
              x <- projectRaster(from = x, crs = crs)
            }
            return(x)
          }
)