#' @useDynLib geometr
globalVariables(c("gtTheme", "fid", "gid", "targetGrob", "include"))

listArgs <- function (){
  as.list(
    match.call(
      definition = sys.function( -1 ),
      call = sys.call( -1 )
    )
  )[-1]
}

#' Convert degree to radians
#' @param degree [\code{numeric(1)}]\cr a degree value to convert to radians.
#' @importFrom checkmate assertNumeric
#' @export

rad <- function(degree){
  assertNumeric(degree)
  (degree * pi)/180
}
