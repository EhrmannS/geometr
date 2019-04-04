#' @useDynLib geometr
globalVariables(c("gtTheme", "fid", "gid", "targetGrob"))

listArgs <- function (){
  as.list(
    match.call(
      definition = sys.function( -1 ),
      call = sys.call( -1 )
    )
  )[-1]
}

rad <- function(degree){
  assertNumeric(degree)
  (degree * pi)/180
}
