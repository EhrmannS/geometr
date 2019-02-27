#' @useDynLib geometr
globalVariables(c("gtTheme"))

listArgs <- function (){
  as.list(
    match.call(
      definition = sys.function( -1 ),
      call = sys.call( -1 )
    )
  )[-1]
}

dots <- function(...){
  eval(substitute(alist(...)))
}

rad <- function(degree){
  assertNumeric(degree)
  (degree * pi)/180
}