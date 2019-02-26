#' @useDynLib geometr
globalVariables(c("gtTheme"))

dots <- function(...){
  eval(substitute(alist(...)))
}