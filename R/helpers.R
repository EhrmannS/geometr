#' List arguments of the parent function
#'
#' Determine the name and value of the arguments called in a function.
#' @return The output of \code{\link{match.call}} for the parent frame.
#' @examples
#'
#' testFun <- function(x = "character", y = NULL, z = c(1, 2, 3)){
#'   g <- listArgs()
#'   return(g)
#' }
#' testFun(x = "hello world")
#' @export

listArgs <- function (){
  as.list(
    match.call(
      definition = sys.function( -1 ),
      call = sys.call( -1 )
    )
  )[-1]
}

#' Determine depth of a list
#'
#' @param list [\code{list(.)}] list to test.
#' @return an integer value of the depth of a list.
#' @examples
#' x <- list(int = c(1:5),
#'           char = list(lower = c(letters[1:5]),
#'                       upper = c(LETTERS[1:5])))
#' depthList(x)
#' depthList(x[[1]])
#' @export

depthList <- function(list) {
  ifelse(is.list(list), 1L + max(sapply(list, depthList)), 0L)
}

#' Transform degree to radians
#' @param degree [\code{numeric(1)}]\cr the degree value to transform.
#' @return a radians value
#' @export

rad <- function(degree){
  assertNumeric(degree)
  (degree * pi)/180
}