#' Set the attribute table of a spatial object.
#' @param x the object to which to assign \code{table}.
#' @param table [\code{data.frame(.)}]\cr the attribute table.
#' @name setTable
#' @rdname setTable
NULL

#' @rdname setTable
#' @export
if(!isGeneric("setTable")){
  setGeneric(name = "setTable",
             def = function(x, table, ...){
               standardGeneric("setTable")
             }
  )
}

#' @rdname setTable
#' @importFrom dplyr left_join
#' @export
setMethod(f = "setTable",
          signature = "geom",
          definition = function(x, table){
            assertDataFrame(table)
            stopifnot(any(names(table) %in% "fid"))
            nIDs <- length(x@attr$fid)
            x@attr <- left_join(x@attr, table)
            return(x)
          }
)

#' @rdname setTable
#' @importFrom dplyr left_join bind_cols
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom sp SpatialPointsDataFrame SpatialPixelsDataFrame
#'   SpatialMultiPointsDataFrame SpatialLinesDataFrame SpatialPolygonsDataFrame
#' @export
setMethod(f = "setTable",
          signature = "Spatial",
          definition = function(x, table){
            assertDataFrame(table)
            assertTRUE(length(x) == dim(table)[1])

            if(grepl("DataFrame", class(x))){
              if(any(colnames(table) %in% colnames(x@data))){
                x@data <- left_join(x@data, table)
              } else{
                x@data <- bind_cols(x@data, table)
              }
              out <- x
            } else{
              if(class(x) == "SpatialPoints"){
                out <- SpatialPointsDataFrame(coords = x, data = table)
              } else if(class(x) == "SpatialPixels"){
                out <- SpatialPixelsDataFrame(points = x, data = table)
              } else if(class(x) == "SpatialMultiPoints"){
                out <- SpatialMultiPointsDataFrame(coords = x, data = table)
              } else if(class(x) == "SpatialLines"){
                out <- SpatialLinesDataFrame(sl = x, data = table, match.ID = FALSE)
              } else if(class(x) == "SpatialPolygons"){
                out <- SpatialPolygonsDataFrame(Sr = x, data = table)
              }
            }

            return(out)
          }
)

#' @rdname setTable
#' @export
setMethod(f = "setTable",
          signature = "sf",
          definition = function(x, table){

          }
)


#' @rdname setTable
#' @importFrom raster ratify
#' @export
setMethod(f = "setTable",
          signature = "RasterLayer",
          definition = function(x, table){
            stopifnot(is.data.frame(table))
            temp <- ratify(x)
            nIDs <- length(temp@data@attributes[[1]][,1])
            stopifnot(dim(table)[1] == nIDs)
            temp@data@attributes <- list(table)
            return(temp)
          }
)