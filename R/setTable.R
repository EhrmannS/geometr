#' Set the attribute table
#'
#' Set the attribute table of a spatial object.
#' @name setTable
NULL

#' @rdname setTable
#' @param x the object to which to assign \code{table}.
#' @param table [\code{data.frame(.)}]\cr the attribute table.
#' @param ... other arguments.
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

          })

#' @rdname setTable
#' @export
setMethod(f = "setTable",
          signature = "sf",
          definition = function(x, table){

          })
