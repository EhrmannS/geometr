#' Set the names of a spatial object.
#'
#' @param x the object for which to set a new name.
#' @param to new name(s).
#' @param ... other arguments.
#' @return The object \code{x} with an update name.
#' @family setters
#' @examples
#' visualise(gtGeoms$grid$categorical)
#'
#' cover <- setNames(x = gtGeoms$grid$categorical, to = "cover")
#'
#' visualise(cover)
#' @name setNames
#' @rdname setNames
NULL

# generic ----
#' @rdname setNames
#' @name setNames
#' @export
setGeneric(name = "setNames",
           def = function(x, to, ...){
             standardGeneric("setNames")
           }
)

# any ----
#' @rdname setNames
#' @export
setMethod(f = "setNames",
          signature = "ANY",
          definition = function(x){
            warning(paste0("I can't set name(s) to an object of class '", paste0(class(x), collapse = ", "), "'."))
          }
)

# geom ----
#' @rdname setNames
#' @importFrom checkmate assertCharacter
#' @importFrom tibble tibble
#' @export
setMethod(f = "setNames",
          signature = "geom",
          definition = function(x, to = NULL){
            assertCharacter(x = to, any.missing = FALSE, min.len = 1)
            x@name <- to
            return(x)
          }
)
