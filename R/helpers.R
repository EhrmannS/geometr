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
#' @importFrom dplyr left_join
#' @export

scaleParameters <- function(attr = NULL, params = NULL, ...){

  # check arguments
  assertDataFrame(x = attr)
  assertList(params, len = 7, any.missing = FALSE)

  # capture display arguments
  displayArgs <- exprs(..., .named = TRUE)
  out <- params

  # when there are display arguments, take them, otherwise take the theme datault
  if(length(displayArgs) != 0){
    tempArgs <- displayArgs
  } else{
    tempArgs <- setNames(list(params$scale$to), params$scale$x)
  }
  if(!any(names(tempArgs) == "fillcol")){
    tempArgs <- c(tempArgs, setNames(list(NA_character_), "fillcol"))
  }

  defaultArgs <- params[!names(params) %in% names(tempArgs)]

  for(i in seq_along(tempArgs)){

    # determine value and name of the i-th display argument
    thisArg <- tempArgs[[i]]
    thisArgName <- names(tempArgs)[i]
    pos <- which(names(params) %in% thisArgName)

    # check whether the parameter value if a column in 'attr'
    if(as.character(thisArg) %in% colnames(attr)){

      vals <- eval(parse(text = paste0(thisArg)), envir = attr)
      vals <- as.numeric(as.factor(vals))
      uniqueVals <- unique(vals)

      # if the argument is a colour argument, construct a color ramp from two or more values
      if(thisArgName %in% c("linecol", "fillcol")){
        out$scale$x <- thisArgName
        out$scale$to <- thisArg

        procVals <- seq_along(uniqueVals)
        # test that there is in fact more than one value
        if(length(procVals) > 1){
          if(length(params[[pos]]) < 2){
            stop(paste0("the parameter '", thisArgName, "' must contain more than 1 value."))
          }
        }
        uniqueColours <- colorRampPalette(colors = params[[pos]])(length(procVals))
        breaks <- c(0, procVals)
        valCuts <- cut(vals, breaks = breaks, include.lowest = FALSE)
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
