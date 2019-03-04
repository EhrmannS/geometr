#' Determine plot parameters
#'
#' Parameter values are determined based on the columns of an attribute table.
#' @param attr [\code{data.frame(1)}]\cr the attribute table from which to
#'   derive values.
#' @param params [\code{list(7)}]\cr the parameters of the geom that shall be
#'   scaled; see Details.
#' @param ... graphical parameters in the form of \code{parameter = column},
#'   where \code{parameter} would be scaled (colours) or repeated along (other
#'   parameters) \code{column}.
#' @details  This function serves merely to determine parameter values from a
#'   given theme based on a given attribute table, it does not set the
#'   parameters in a plot. The provided value range thus depends on the values
#'   provided in the theme. Use \code{\link{setTheme}} to set the values to a
#'   modified range.
#'
#'   The paramaters that can be scaled can be found in \code{gtTheme@geom}. They
#'   are by default: \itemize{ \item \code{linecol = c("#00204DFF",
#'   "#FFEA46FF")} \item \code{fillcol = NA}, \item
#'   \code{linetype = "solid"}, \item \code{linewidth = 1}, \item
#'   \code{pointsize = 0.5}, \item \code{pointsymbol = 20}.}
#' @return a list of parameters to a grob.
#' @importFrom checkmate assertCharacter assertList assertTRUE
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang exprs rep_along
#' @importFrom stats setNames
#' @export

scaleParameters <- function(attr = NULL, params = NULL, ...){

  # check arguments
  assertDataFrame(x = attr)
  assertList(params, len = 7, any.missing = FALSE)

  displayArgs <- exprs(..., .named = TRUE)
  out <- params

  if(length(displayArgs) != 0){
    tempArgs <- displayArgs
  } else{
    tempArgs <- setNames(list(params$scale$to), params$scale$x)
  }

  defaultArgs <- params[!names(params) %in% names(tempArgs)]

  for(i in seq_along(tempArgs)){

    thisArg <- tempArgs[[i]]
    thisArgName <- names(tempArgs)[i]
    pos <- which(names(params) %in% thisArgName)

    if(as.character(thisArg) %in% colnames(attr)){

      vals <- eval(parse(text = paste0(thisArg)), envir = attr)
      uniqueVals <- sort(unique(vals))

      if(thisArgName %in% c("linecol", "fillcol")){

        procVals <- as.numeric(as.factor(uniqueVals))
        if(length(procVals) > 1){
          if(length(params[[pos]]) < 2){
            stop(paste0("the parameter ", params$scale$x, " must contain more than 1 value."))
          }
        }
        uniqueColours <- colorRampPalette(colors = params[[pos]])(length(procVals))
        breaks <- c(procVals[1]-1, procVals)
        breaks <- c(0, procVals)
        valCuts <- cut(procVals, breaks = breaks, include.lowest = TRUE)
        tempOut <- uniqueColours[valCuts]

      } else{
        tempOut <- rep_along(vals, params[thisArgName][[1]])
      }

    } else{
      tempOut <- thisArg
    }

    out[[pos]] <- tempOut

  }

  for(i in seq_along(defaultArgs)){
    if(i == 1) next

    thisArg <- defaultArgs[[i]][[1]]
    thisArgName <- names(defaultArgs)[i]
    pos <- which(names(params) %in% thisArgName)

    out[[pos]] <- rep(thisArg, dim(attr)[1])

  }

  return(out)
}
