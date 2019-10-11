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

            thePoints <- getTable(x = x, slot = "point")
            theFeatures <- getTable(x = x, slot = "feature")
            theGroups <- getTable(x = x, slot = "group")

            if(slot == "point"){
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = thePoints)
              }
              thePoints <- thePoints[matches,]
              theFeatures <- theFeatures[theFeatures$fid %in% thePoints$fid,]
              theGroups <- theGroups[theGroups$gid %in% theFeatures$gid,]
            } else if(slot == "feature"){
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = theFeatures)
              }
              theFeatures <- theFeatures[matches,]
              thePoints <- thePoints[thePoints$fid %in% theFeatures$fid,]
              theGroups <- theGroups[theGroups$gid %in% theFeatures$gid,]
            } else {
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = theGroups)
              }
              theGroups <- theGroups[matches,]
              theFeatures <- theFeatures[theFeatures$gid %in% theGroups$gid,]
              thePoints <- thePoints[thePoints$fid %in% theFeatures$fid,]
            }

            # actually not using setTable() here, because this is not a
            # merge/cbind operation, but setting a totally new table
            x@point <- thePoints
            x@feature <- theFeatures
            x@group <- as.list(theGroups)

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
            subset <- enquos(...)
            isLogical <- tryCatch(is.logical(eval_tidy(expr = subset[[1]])), error = function(e) FALSE)
            if(isLogical){
              matches <- eval_tidy(expr = subset[[1]])
            } else {
              subset <- exprs(...)
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
            subset <- enquos(...)
            isLogical <- tryCatch(is.logical(eval_tidy(expr = subset[[1]])), error = function(e) FALSE)
            if(isLogical){
              matches <- eval_tidy(expr = subset[[1]])
            } else {
              subset <- exprs(...)
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