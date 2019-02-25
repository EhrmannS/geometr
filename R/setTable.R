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
            stopifnot(is.data.frame(table))
            stopifnot(any(names(table) %in% "fid"))
            nIDs <- length(x@attr$fid)
            x@attr <- left_join(x@attr, table)
            return(x)
          }
)

#' @rdname setTable
#' @export
setMethod(f = "setTable",
          signature = "Spatial",
          definition = function(x, table){

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