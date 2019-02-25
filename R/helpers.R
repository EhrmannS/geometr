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

listArgs <- function (){
  as.list(
    match.call(
      definition = sys.function( -1 ),
      call = sys.call( -1 )
    )
  )[-1]
}

#' Transform degree to radians
#' @param degree [\code{numeric(1)}]\cr the degree value to transform.
#' @return a radians value

rad <- function(degree){
  assertNumeric(degree)
  (degree * pi)/180
}


#' Set the scale of a plot
#'
#' @param attr [\code{data.frame(1)}]\cr the attribute table from which to
#'   derive values.
#' @param params [\code{list(7)}]\cr the parameters of the geom that shall be
#'   scaled.
#' @return a list of parameters to a grob.
#' @importFrom checkmate assertCharacter assertList assertTRUE
#' @importFrom grDevices colorRampPalette

scaleParameters <- function(attr = NULL, params = NULL){

  # check arguments
  assertCharacter(params$scale$x, any.missing = FALSE)
  vals <- as.numeric(as.factor(eval(parse(text = paste0(params$scale$to)), envir = attr)))
  uniqueVals <- sort(unique(vals))
  if(dim(attr)[1] > 1){
    assertTRUE(length(vals) > 1)
  }
  assertList(params, len = 7, any.missing = FALSE)
  pos <- which(names(params) == params$scale$x)
  notPos <- which(names(params) != params$scale$x)
  out <- params

  if(params$scale$x %in% c("line", "fill")){
    if(length(uniqueVals) > 1){
      if(length(params[[pos]]) < 2){
        stop(paste0("the parameter ", params$scale$x, " must contain more than 1 value."))
      }
    }
    uniqueColours <- colorRampPalette(colors = params[[pos]])(length(uniqueVals))
    breaks <- c(uniqueVals[1]-1, uniqueVals)
    breaks <- c(0, uniqueVals)
    valCuts <- cut(vals, breaks = breaks, include.lowest = TRUE)
    out[[pos]] <- uniqueColours[valCuts]

    for(i in notPos[-1]){
      out[[i]] <- rep(out[[i]][[1]], times = length(vals))
    }

  }
  return(out)
}
