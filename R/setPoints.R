#' Set a table of point attributes
#'
#' @param x the object to which to assign a new point attribute table.
#' @param table [\code{data.frame(.)}]\cr the new attribute table.
#' @return The object \code{x} with an updated point attribute table.
#' @family setters
#' @name setPoints
#' @rdname setPoints
NULL

# generic ----
#' @rdname setPoints
#' @name setPoints
#' @export
if(!isGeneric("setPoints")){
  setGeneric(name = "setPoints",
             def = function(x, table, ...){
               standardGeneric("setPoints")
             }
  )
}

# any ----
#' @rdname setPoints
#' @export
setMethod(f = "setPoints",
          signature = "ANY",
          definition = function(x, table = NULL){
            warning(paste0("I can't set point attributes to an object of class '", paste0(class(x), collapse = ", "), "'."))
          }
)

# geom ----
#' @rdname setPoints
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "setPoints",
          signature = "geom",
          definition = function(x, table = NULL){
            if(!any(names(table) %in% "fid")){
                stop("'table' must contain the column 'fid'.")
            }
            if(any(c("x", "y") %in% names(table))){
              stop("recently new points can't be defined with this function.")
            }
            if(x@type == "grid"){

            } else {
              thePoints <- getPoints(x = x)
              theFeatures <- getFeatures(x = x)

              outPoints <- merge(thePoints, table, all.x = TRUE)
              outPoints <- .updateOrder(input = outPoints)
              if(any(colnames(table) %in% "gid")){
                outFeatures <- tibble(fid = outPoints$fid,
                                      gid = outPoints$gid)
              } else {
                outFeatures <- tibble(fid = outPoints$fid,
                                      gid = seq_along(outPoints$fid))
              }
              if(any(colnames(outPoints) == "gid")){
                outPoints <- outPoints[,-which(colnames(outPoints) == "gid")]
              }

              x@point <- as_tibble(outPoints)
              x@feature <- list(geometry = outFeatures)
            }

            cln <- colnames(table)
            if(length(cln) > 1){
              hist <- paste0("the 'point' attribute table was joined with the variables (", paste(cln, collapse = ", "), ")")
            } else {
              hist <- paste0("the 'point' attribute table was joined with the variable ", cln)
            }
            x@history <- c(getHistory(x = x), list())

            return(x)
          }
)