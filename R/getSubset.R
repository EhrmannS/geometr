#' Get the subset of a spatial object.
#' @param x object to \code{subset}.
#' @param slot [\code{character(1)}]\cr the slot in which to determine a subset,
#'   either \code{"table"} or \code{"vert"}.
#' @param ... Logical predicates defined in terms of the variables in \code{x}.
#'   Multiple conditions are combined with &. Only rows where the condition
#'   evaluates to TRUE are kept.
#' @name getSubset
#' @rdname getSubset
NULL

#' @rdname getSubset
#' @name getSubset
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
#' # get feature 1
#' (getSubset(x = gtGeoms$line, fid == 1))
#'
#' @importFrom checkmate assertCharacter assertChoice
#' @importFrom rlang exprs
#' @export
setMethod(f = "getSubset",
          signature = signature("geom"),
          definition = function(x, ..., slot = "feat"){
            assertCharacter(x = slot, len = 1, any.missing = FALSE)
            assertChoice(x = slot, choices = c("vert", "feat", "group"))
            subset <- exprs(...)
            if(slot == "vert"){
              matches <- eval(parse(text = subset), envir = x@vert)
              x@vert <- x@vert[matches,]
              x@feat <- x@feat[x@feat$fid %in% x@vert$fid,]
            } else if(slot == "feat"){
              matches <- eval(parse(text = subset), envir = x@feat)
              x@feat <- x@feat[matches,]
              x@vert <- x@vert[x@vert$fid %in% x@feat$fid,]
            } else {

            }

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