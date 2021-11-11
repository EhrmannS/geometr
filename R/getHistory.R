#' Get the history of a spatial object.
#'
#' @param x the object from which to derive the history.
#' @param ... other arguments.
#' @return A list of the events that lead to \code{x}.
#' @family getters
#' @examples
#' library(tibble)
#'
#' tibble(x = c(40, 70, 70, 50),
#'        y = c(40, 40, 60, 70)) %>%
#'   gs_polygon() %>%
#'   gt_reflect(angle = 45) %>%
#'   getHistory()
#'
#' getHistory(gtGeoms$line)
#'
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getHistory()
#'
#' getWindow(x = matrix(0, 3, 5))
#' @name getHistory
#' @rdname getHistory
NULL

# generic ----
#' @rdname getHistory
#' @name getHistory
#' @export
setGeneric(name = "getHistory",
           def = function(x, ...){
             standardGeneric("getHistory")
           }
)


# any ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "geom",
          definition = function(x){
            hist <- x@history
            if(length(hist) == 0){
              hist <- list("the object was loaded from memory")
            }
            return(hist)
          }
)

# raster ----
#' @rdname getHistory
#' @export
setMethod(f = "getHistory",
          signature = "Raster",
          definition = function(x){
            if(inherits(x, "RasterStack")){
              hist <- list()
              for(i in 1:dim(x)[3]){
                hist <- c(hist, x@history)
              }
            } else {
              hist <- x@history
            }
            if(length(hist) == 0){
              hist <- list("the object was loaded from memory")
            }
            return(hist)
          }
)
