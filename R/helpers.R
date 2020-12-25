#' Convert degree to radians
#' @param degree [\code{numeric(1)}]\cr a degree value to convert to radians.
#' @importFrom checkmate assertNumeric

.rad <- function(degree){

  assertNumeric(x = degree)

  (degree * pi)/180
}

#' Get the number of decimal places
#' @param x [\code{numeric(1)}]\cr the number for which to derive decimal
#'   places.
#' @importFrom checkmate assertNumeric

.getDecimals <- function(x) {
  # https://stackoverflow.com/a/5173906/4506642

  assertNumeric(x = x)

  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }

}


