#' Get the table of group attributes
#'
#' @param x the object from which to derive the attribute table.
#' @details This table contains at least the column 'gid'.
#'
#'   When this function is called on "ANY" object, it is first tested whether
#'   that object has features (\code{\link{getFeatures}}), from which the groups
#'   can be reconstructed. If this is not the case, \code{NULL} is returned.
#' @return A tibble (or a list of tibbles per layer) of the group attributes of
#'   \code{x}.
#' @family getters
#' @examples
#' getGroups(x = gtGeoms$polygon)
#'
#' # for raster objects, groups are pixels with the same value and their
#' # attributes are in the raster attribute table (RAT)
#' getGroups(x = gtRasters)
#' @name getGroups
#' @rdname getGroups
NULL

# generic ----
#' @rdname getGroups
#' @name getGroups
#' @export
if(!isGeneric("getGroups")){
  setGeneric(name = "getGroups",
             def = function(x){
               standardGeneric("getGroups")
             }
  )
}

# any ----
#' @rdname getGroups
#' @export
setMethod(f = "getGroups",
          signature = "ANY",
          definition = function(x){
            theFeatures <- getFeatures(x = x)
            if(!is.null(theFeatures)){
              tibble(gid = sortUniqueC(theFeatures$gid))
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
              theNames <- getNames(x = x)
              theFeatures <- getFeatures(x)
              theGroups <- x@group

              out <- NULL
              for(i in seq_along(theNames)){
                vals <- unlist(theFeatures[theNames[i]], use.names=F)
                if(is.numeric(vals)){
                  sbs <- sortUniqueC(vals)

                  if(all(sbs %in% theGroups$value)){
                    tab <- theGroups[theGroups$value %in% sbs,]
                  } else {
                    tab <- tibble(value = sbs)
                  }
                } else {
                  tab <- tibble(value = integer())
                }

                if(length(theNames) > 1){
                  out <- c(out, setNames(list(tab), theNames[i]))
                } else {
                  out <- tab
                }

              }

            } else {
              out <- x@group
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
              out <- tibble(value = integer())
            } else {
              out <- NULL
              for(i in 1:dim(x)[3]){
                temp <- x[[i]]@data@attributes
                if(length(temp) != 0){
                  names <- names(temp[[1]])
                  names[which(names == "id")] <- "value"
                  tab <- as_tibble(temp[[1]])
                  names(tab) <- names
                } else {
                  tab <- tibble(value = integer())
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