#' Set additional entries to the history of an object
#'
#' @param x the object for which to set the coordinate reference system.
#' @param history [\code{list(1)}]\cr the history to set for this object.
#' @details Both, objects of class \code{geom} and \code{Raster*} have the slot
#'   \code{@history}, which contains the provenance of that object. With
#'   \code{setHistory}, that provenance can be updated, based on the
#'   modification the object has been exposed to. This happens automatically for
#'   all geometry operations that come with \code{geometr}.
#' @return The object \code{x} where the history slot has been updated.
#' @family setters
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

# any ----
#' @rdname setHistory
#' @export
setMethod(f = "setHistory",
          signature = "ANY",
          definition = function(x){
            warning(paste0("I can't set a history to an object of class '", paste0(class(x), collapse = ", "), "'."))
          }
)

# geom ----
#' @rdname setHistory
#' @export
setMethod(f = "setHistory",
          signature = "geom",
          definition = function(x, history = NULL){

            if(length(x@history) == 0){
              temp <- list(paste0("the object was loaded from memory"))
            } else {
              temp <- x@history
            }

            x@history <- c(temp, list(history))
            return(x)
          }
)

# Raster ----
#' @rdname setHistory
#' @export
setMethod(f = "setHistory",
          signature = "RasterLayer",
          definition = function(x, history = NULL){

            if(length(x@history) == 0){
              temp <- list(paste0("the object was loaded from memory"))
            } else {
              temp <- x@history
            }

            x@history <- c(temp, list(history))
            return(x)
          }
)
