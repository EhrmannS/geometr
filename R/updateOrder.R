#' Update column order
#'
#' Set the order of the table columns to \code{c("fid", "gid", rest)}
#' @param input [\code{data.frame(1)}]\cr a table that contains at least the
#'   columns \code{fid} and \code{gid}.
#' @return A new table where the columns have the correct order.
#' @importFrom checkmate assertNames

.updateOrder <- function(input = NULL){

  # check arguments
  targetCols <- c("fid", "gid")
  targetCols <- targetCols[targetCols %in% names(input)]

  out <- input[c(targetCols, names(input)[!names(input) %in% targetCols])]

  return(out)
}
