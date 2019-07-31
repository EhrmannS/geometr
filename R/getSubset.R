#' Get the subset of a spatial object.
#'
#' @param x object to \code{subset}.
#' @param slot [\code{character(1)}]\cr the slot in which to determine a subset,
#'   either \code{"vert"} for vertices, \code{"feat"} for features or
#'   \code{"group"} for group tables.
#' @param ... Logical predicates defined in terms of the variables in \code{x}.
#'   Multiple conditions are combined with &. Only rows where the condition
#'   evaluates to TRUE are kept.
#' @return A subset of \code{x} in its original class.
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
#' # get the subset of a geom
#' (obj <- gtGeoms$point)
#'
#' # the first feature
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
              gids <- x@feat$gid
              x@group <- x@group[x@group$gid %in% gids,]
            } else if(slot == "feat"){
              matches <- eval(parse(text = subset), envir = x@feat)
              x@feat <- x@feat[matches,]
              x@vert <- x@vert[x@vert$fid %in% x@feat$fid,]
              gids <- x@feat$gid
              x@group <- x@group[x@group$gid %in% gids,]
            } else {
              matches <- eval(parse(text = subset), envir = x@group)
              x@group <- x@group[matches,]
              x@feat <- x@feat[x@feat$gid %in% x@group$gid,]
              x@vert <- x@vert[x@vert$fid %in% x@feat$fid,]
            }
            return(x)
          }
)

#' @rdname getSubset
#' @examples
#'
#' # get the subset of a Spatial object
#' (obj <- gtSP$SpatialPolygonsDataFrame)
#'
#' # the subset where the attribute a equals 2
#' (getSubset(x = obj, a == 2))
#'
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
#' # get the subset of an sf object
#' (obj <- gtSF$polygon)
#'
#' # the subset where the attribute a equals 1
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