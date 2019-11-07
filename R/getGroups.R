#' Get the table of group attributes
#'
#' Get tabular information of the attributes of groups of features.
#' @param x the object from which to derive the attribute table.
#' @param ... subset based on logical predicates defined in terms of the
#'   variables in \code{x} or a vector of booleans. Multiple conditions are
#'   combined with &. Only rows where the condition evaluates to TRUE are kept.
#' @return A table of the group attributes of \code{x} or an object where the
#'   groups table has been subsetted.
#' @family getters
#' @name getGroups
#' @rdname getGroups
NULL

# generic ----
#' @rdname getGroups
#' @name getGroups
#' @export
if(!isGeneric("getGroups")){
  setGeneric(name = "getGroups",
             def = function(x, ...){
               standardGeneric("getGroups")
             }
  )
}

# any ----
#' @rdname getGroups
#' @export
setMethod(f = "getGroups",
          signature = "ANY",
          definition = function(x, ...){
            NULL
          }
)

# geom ----
#' @rdname getGroups
#' @examples
#' getGroups(x = gtGeoms$polygon)
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getGroups",
          signature = "geom",
          definition = function(x, ...){

            theType <- getType(x = x)[2]

            if(length(exprs(...)) > 0){
              out <- x
              subset <- enquos(...)
              isLogical <- tryCatch(is.logical(eval_tidy(expr = subset[[1]])), error = function(e) FALSE)
              thePoints <- getPoints(x = x)
              theFeatures <- getFeatures(x = x)
              theGroups <- getGroups(x = x)
              if(isLogical){
                matches <- eval_tidy(expr = subset[[1]])
              } else {
                subset <- exprs(...)
                matches <- eval(parse(text = subset), envir = theGroups)
              }
              theGroups <- theGroups[matches,]
              theFeatures <- theFeatures[theFeatures$gid %in% theGroups$gid,]
              thePoints <- thePoints[thePoints$fid %in% theFeatures$fid,]

              out@point <- thePoints
              out@feature <- list(geometry = theFeatures)
              out@group <- list(geometry = theGroups)
            } else {

              if(theType == "grid"){
                theGroups <- x@group
                out <- list()
                for(i in seq_along(theGroups)){
                  theInput <- theGroups[[i]]
                  theName <- names(theGroups)[i]
                  if(length(theGroups) > 1){
                    out <- c(out, setNames(list(theInput), theName))
                  } else {
                    out <- theInput
                  }
                }
              } else {
                out <- x@group$geometry
              }
            }

            return(out)
          }
)

# Raster ----
#' @rdname getGroups
#' @examples
#'
#' getGroups(gtRasters$categorical)
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getGroups",
          signature = "Raster",
          definition = function(x){
            if(length(x@data@attributes) == 0){
              vals <- sort(unique(getValues(x = x)))
              tibble(fid = seq_along(vals), values = vals)
            } else{
              names <- names(x@data@attributes[[1]])
              names[which(names == "id")] <- "fid"
              out <- as_tibble(x@data@attributes[[1]])
              names(out) <- names
              return(out)
            }
          }
)

# master ----
#' @rdname getGroups
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getGroups",
          signature = "matrix",
          definition = function(x){
            vals <- sort(unique(as.vector(x = x)))
            tibble(fid = seq_along(vals), values = vals)
          }
)