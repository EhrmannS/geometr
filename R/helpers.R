#' Set the scale of a plot
#'
#' @param attr [\code{data.frame(1)}]\cr the attribute table from which to
#'   derive values.
#' @param params [\code{list(7)}]\cr the parameters of the geom that shall be
#'   scaled.
#' @return a list of parameters to a grob.
#' @importFrom checkmate assertCharacter assertList assertTRUE
#' @importFrom grDevices colorRampPalette
#' @export

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
