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

#' Gather colours for the plot
#'
#' @param input []
#' @param theme []
#'

makeColours <- function(input = NULL, theme = NULL){

  isRaster <- testClass(x = input, classes = "Raster")
  isGeom <- testClass(x = input, classes = "geom")

  attr <- getTable(input)

  if(isRaster){
    # get some meta of input
    hasColourTable <- as.logical(length(input@legend@colortable))
    isFactor <- input@data@isfactor
    vals <- input@data@values
    uniqueVals <- sortUniqueC(input[!is.na(input)])
    nrVals <- length(uniqueVals)
    targetColours <- theme@raster$colours

    # limit values to 256, this is the number of distinct colours that can be
    # represented
    if(nrVals < 256){
      nrVals <- nrVals
    } else{
      nrVals <- 256
    }

    # # values must start at 1
    # if(uniqueVals[1] == 0){
    #   uniqueVals <- uniqueVals+1
    # }

  } else if(isGeom){


  }

  if(hasColourTable){
    uniqueColours <- input@legend@colortable[uniqueVals]
    breaksTemp <- c(uniqueVals[1]-1, uniqueVals)
  } else if(isFactor){
    uniqueColours <- colorRampPalette(colors = targetColours)(nrVals)
    idPos <- grep("id", colnames(attr), ignore.case = TRUE)
    breaksTemp <- c(uniqueVals[1]-1, attr[[idPos]])
  } else {
    uniqueColours <- colorRampPalette(colors = targetColours)(nrVals)
    breaksTemp <- c(uniqueVals[1]-1, seq(uniqueVals[1], uniqueVals[[length(uniqueVals)]], length.out = nrVals))
  }

  valCuts <- cut(vals, breaks = breaksTemp, include.lowest = TRUE)
  out.cols <- uniqueColours[valCuts]

  # determine the tick values ...
  if(length(uniqueVals) > theme@legend$bins){
    tickValues <- quantile(uniqueVals, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
  } else{
    tickValues <- uniqueVals
  }

  # ... and their labels
  legend.labels <- round(tickValues, 1)

  if(theme@legend$ascending){
    legend.array <- matrix(data = rev(uniqueColours), ncol = 1, nrow = length(uniqueColours))
    legend.values <- rev(uniqueVals)
    legend.labels.pos <- unit(which(uniqueVals %in% tickValues), "native")
  } else{
    legend.array <- matrix(data = uniqueColours[[i]], ncol = 1, nrow = length(uniqueColours[[i]]))
    legend.values <- uniqueVals[[i]]
    legend.labels.pos <- rev(unit(which(uniqueVals[[i]] %in% tickValues[[i]]), "native"))
  }

  out <- list(out.cols = out.cols,
              legend.array = legend.array,
              legend.values = legend.values,
              legend.labels = legend.labels,
              legend.labels.pos = legend.labels.pos)
}
