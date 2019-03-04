#' Get the subset of a spatial object.
#' @param x object to \code{subset}.
#' @param slot [\code{character(1)}]\cr the slot in which to determine a subset,
#'   either \code{"table"} or \code{"coords"}.
#' @param ... Logical predicates defined in terms of the variables in \code{x}.
#'   Multiple conditions are combined with &. Only rows where the condition
#'   evaluates to TRUE are kept.
#' @name getSubset
#' @rdname getSubset
NULL

#' @rdname getSubset
#' @export
if(!isGeneric("getSubset")){
  setGeneric(name = "getSubset",
             def = function(x, ...){
               standardGeneric("getSubset")
             }
  )
}

#' @rdname getSubset
#' @examples
#' (obj <- gtGeoms$point)
#'
#' # get subset of features that have less than 3 vertices
#' (getSubset(x = gtGeoms$line, n < 5))
#'
#' @importFrom checkmate assertCharacter assertChoice
#' @importFrom rlang exprs
#' @export
setMethod(f = "getSubset",
          signature = signature("geom"),
          definition = function(x, ..., slot = "table"){
            assertCharacter(x = slot, len = 1, any.missing = FALSE)
            assertChoice(x = slot, choices = c("table", "coords"))
            subset <- exprs(...)
            if(slot == "table"){
              matches <- eval(parse(text = subset), envir = x@attr)
              x@attr <- x@attr[matches,]
              x@coords <- x@coords[x@coords$fid %in% x@attr$fid,]
            } else{
              matches <- eval(parse(text = subset), envir = x@coords)
              x@coords <- x@coords[matches,]
              x@attr <- x@attr[x@attr$fid %in% x@coords$fid,]
            }
            nVids <- sapply(unique(x@coords$fid), function(i){
              length(x@coords$vid[x@coords$fid == i])
            })
            x@attr$n <- nVids

            return(x)
          }
)

#' @rdname getSubset
#' @examples
#'
#' (obj <- gtSP$SpatialPolygonsDataFrame)
#' (getSubset(x = obj, a == 2))
#' @export
setMethod(f = "getSubset",
          signature = signature("Spatial"),
          definition = function(x, ...){
            subset <- exprs(...)
            matches <- eval(parse(text = subset), envir = x@data)
            x <- x[matches,]
            return(x)
          }
)

#' @rdname getSubset
#' @examples
#'
#' (obj <- gtSF$polygon)
#' (getSubset(x = obj, a == 1))
#' @export
setMethod(f = "getSubset",
          signature = signature("sf"),
          definition = function(x, ...){
            subset <- exprs(...)
            matches <- eval(parse(text = subset), envir = x)
            x <- x[matches,]
            return(x)
          }
)