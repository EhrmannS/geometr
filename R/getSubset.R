#' Get the subset of a spatial object.
#'
#' @param x object to \code{subset}.
#' @param slot [\code{character(1)}]\cr the slot in which to determine a subset,
#'   either \code{"point"} for vertices, \code{"feature"} for features or
#'   \code{"group"} for group tables.
#' @param ... Logical predicates defined in terms of the variables in \code{x}
#'   or a vector of booleans. Multiple conditions are combined with &. Only rows
#'   where the condition evaluates to TRUE are kept.
#' @return A subset of \code{x} in its original class.
#' @family getters
#' @name getSubset
#' @rdname getSubset
NULL

# generic ----
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

# any ----
#' @rdname getSubset
#' @export
setMethod(f = "getSubset",
          signature = "ANY",
          definition = function(x){
            NULL
          }
)

# geom ----
#' @rdname getSubset
#' @examples
#' # get the subset of a geom
#' (obj <- gtGeoms$point)
#'
#' # the first feature
#' (getSubset(x = gtGeoms$line, fid == 1))
#'
#' @importFrom checkmate assertCharacter assertChoice
#' @importFrom rlang enquos
#' @export
setMethod(f = "getSubset",
          signature = signature("geom"),
          definition = function(x, ..., slot = "feature"){
            assertCharacter(x = slot, len = 1, any.missing = FALSE)
            assertChoice(x = slot, choices = c("point", "feature", "group"))
            subset <- enquos(...)
            isLogical <- tryCatch(is.logical(eval_tidy(expr = subset[[1]])), error = function(e) FALSE)
            if(slot == "point"){
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = x@point)
              }
              x@point <- x@point[matches,]
              x@feature <- x@feature[x@feature$fid %in% x@point$fid,]
              gids <- x@feature$gid
              x@group <- x@group[x@group$gid %in% gids,]
            } else if(slot == "feature"){
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = x@feature)
              }
              x@feature <- x@feature[matches,]
              x@point <- x@point[x@point$fid %in% x@feature$fid,]
              gids <- x@feature$gid
              x@group <- x@group[x@group$gid %in% gids,]
            } else {
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = x@group)
              }
              x@group <- x@group[matches,]
              x@feature <- x@feature[x@feature$gid %in% x@group$gid,]
              x@point <- x@point[x@point$fid %in% x@feature$fid,]
            }
            return(x)
          }
)

# Spatial ----
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
            if(is.logical(subset)){
              matches <- subset
            } else {
              matches <- eval(parse(text = subset), envir = x@data)
            }
            x <- x[matches,]
            return(x)
          }
)

# sf ----
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
            if(is.logical(subset)){
              matches <- subset
            } else {
              matches <- eval(parse(text = subset), envir = x)
            }
            x <- x[matches,]
            return(x)
          }
)

# ppp ----
#' @rdname getSubset
#' @examples
#'
#' # getSubset(gtPPP$...)
#' @export
setMethod(f = "getSubset",
          signature = "ppp",
          definition = function(x){

          }
)