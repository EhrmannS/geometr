#' Get the table of group attributes
#'
#' @param x the object from which to derive the attribute table.
#' @param ... additional arguments.
#' @details When this function is called on "ANY" object, it is first tested
#'   whether that object has features (\code{\link{getFeatures}}), from which
#'   the groups can be reconstructed. If this is not the case, \code{NULL} is
#'   returned.
#' @return A table of the group attributes of \code{x}.
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
            theFeatures <- getFeatures(x = x)
            if(!is.null(theFeatures)){
              tibble(gid = unique(theFeatures$gid))
            } else {
              theFeatures
            }
          }
)

# geom ----
#' @rdname getGroups
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getGroups",
          signature = "geom",
          definition = function(x){

            theType <- getType(x = x)[1]

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
            if(class(x) == "RasterBrick"){
              out <- tibble(gid = 1)
            } else {
              out <- NULL
              for(i in 1:dim(x)[3]){
                temp <- x[[i]]@data@attributes
                if(length(temp) != 0){
                  names <- names(temp[[1]])
                  names[which(names == "id")] <- "gid"
                  tab <- as_tibble(temp[[1]])
                  names(tab) <- names
                } else {
                  tab <- tibble(gid = sortUniqueC(getValues(x)))
                }
                if(dim(x)[3] == 1){
                  out <- tab
                } else {
                  out <- c(out, setNames(list(tab), names(x)[i]))
                }
              }
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
            tibble(gid = sortUniqueC(as.vector(x = x)))
          }
)