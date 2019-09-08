#' Set additional entries to the history of an object
#'
#' @details
#' @param x the object for which to set the coordinate reference system.
#' @param history [\code{character(1)}]\cr the coordinate reference system to set
#'   for this object.
#' @return bla
#' @name setHistory
#' @rdname setHistory
NULL

# generic ----
#' @rdname setHistory
#' @name setHistory
#' @docType methods
#' @export
if(!isGeneric("setHistory")){
  setGeneric(name = "setHistory",
             def = function(x, history, ...){
               standardGeneric("setHistory")
             }
  )
}

# geom ----
#' @rdname setHistory
#' @export
setMethod(f = "setHistory",
          signature = "geom",
          definition = function(x, history = NULL){

          }
)

# Raster ----
#' @rdname setHistory
#' @export
setMethod(f = "setHistory",
          signature = "Raster",
          definition = function(x, history = NULL){

          }
)
