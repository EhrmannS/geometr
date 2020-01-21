#' Get the table of group attributes
#'
#' Get tabular information of the attributes of groups of features.
#' @param x the object from which to derive the attribute table.
#' @param ... subset based on logical predicates defined in terms of the
#'   columns in \code{x} or a vector of booleans. Multiple conditions are
#'   combined with \code{&}. Only rows where the condition evaluates to TRUE are kept.
#' @return A table of the group attributes of \code{x} or an object where the
#'   groups table has been subsetted.
#' @family getters
#' @examples
#' getGroups(x = gtGeoms$polygon)
#'
#' # for raster objects, groups are pixels with the same value and their
#' # attributes are in the raster attribute table (RAT)
#' getGroups(x = gtRasters$categorical)
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

              out <- new(Class = "geom",
                         type = theType,
                         point = thePoints,
                         feature = list(geometry = theFeatures),
                         group = list(geometry = theGroups),
                         window = getWindow(x = x),
                         scale = "absolute",
                         crs = getCRS(x = x),
                         history = getHistory(x = x))
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
                    if(dim(theInput)[1] == 0){
                      out <- theGroups[[1]]
                    } else {
                      out <- theInput
                    }
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
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getGroups",
          signature = "Raster",
          definition = function(x){
            if(length(x@data@attributes) == 0){
              out <- tibble(gid = integer())
            } else{
              names <- names(x@data@attributes[[1]])
              names[which(names == "id")] <- "gid"
              out <- as_tibble(x@data@attributes[[1]])
              names(out) <- names
            }
            return(out)
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
            tibble(gid = seq_along(vals), values = vals)
          }
)