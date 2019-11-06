#' Set the table of group attributes
#'
#' @param x the object to which to assign an attribute table.
#' @param table [\code{data.frame(.)}]\cr the attribute table.
#' @return The object \code{x} with an updated attribute table.
#' @family setters
#' @name setGroups
#' @rdname setGroups
NULL

# generic ----
#' @rdname setGroups
#' @name setGroups
#' @export
if(!isGeneric("setGroups")){
  setGeneric(name = "setGroups",
             def = function(x, table, ...){
               standardGeneric("setGroups")
             }
  )
}

# any ----
#' @rdname setGroups
#' @export
setMethod(f = "setGroups",
          signature = "ANY",
          definition = function(x){
            warning(paste0("I can't set group attributes to an object of class '", paste0(class(x), collapse = ", "), "'."))
          }
)

# geom ----
#' @rdname setGroups
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "setGroups",
          signature = "geom",
          definition = function(x, table = NULL){
            if(!any(names(table) %in% c("gid"))){
              if(dim(table)[1] != dim(x@point)[1]){
                stop("'table' must either contain the column 'gid' or be of the same length as 'x'.")
              }
            }
            if(x@type == "grid"){

            } else {
              theGroups <- getGroups(x = x)

              if(any(colnames(table) %in% colnames(theGroups))){
                temp <- merge(theGroups, table, all.x = TRUE)
                temp <- .updateOrder(input = temp)
              } else{
                temp <- cbind(theGroups, table)
              }
              x@group <- list(geometry = as_tibble(temp))
            }

            cln <- colnames(table)
            if(length(cln) > 1){
              hist <- paste0("the 'group' attribute table was joined with the variables (", paste(cln, collapse = ", "), ")")
            } else {
              hist <- paste0("the 'group' attribute table was joined with the variable ", cln)
            }
            x@history <- c(getHistory(x = x), list())

            return(x)
          }
)

# RasterLayer ----
#' @rdname setGroups
#' @importFrom raster ratify
#' @importFrom checkmate assertDataFrame
#' @export
setMethod(f = "setGroups",
          signature = "RasterLayer",
          definition = function(x, table = NULL){
            assertDataFrame(x = table)
            temp <- ratify(x)
            nIDs <- length(temp@data@attributes[[1]][,1])
            stopifnot(dim(table)[1] == nIDs)
            temp@data@attributes <- list(table)
            return(temp)
          }
)
