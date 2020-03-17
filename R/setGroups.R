#' Set a table of group attributes
#'
#' @param x the object to which to assign a new attribute table.
#' @param table [\code{data.frame(.)}]\cr the new attribute table.
#' @return The object \code{x} with an updated group attribute table.
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
              stop("'table' must contain the column 'gid'.")
            }
            if(x@type == "grid"){

            } else {
              theGroups <- getGroups(x = x)
              theGroups <- theGroups[c("gid")]

              outGroups <- merge(theGroups, table, all.y = TRUE)
              outGroups <- .updateOrder(input = outGroups)
              if(any(colnames(outGroups) == "fid")){
                outGroups <- outGroups[,-which(colnames(outGroups) == "fid")]
              }

              x@group <- list(geometry = as_tibble(outGroups))
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
