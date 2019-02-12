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

#' Translate 'day of the year' to a date
#'
#' @param year [\code{integerish(1)}]\cr a year value as part of the date.
#' @param doy [\code{integerish(1)}]\cr the day of the year for which you want
#'   the date
#' @return An object of class \code{\link{POSIXct}}.
#' @examples
#' aDate <- doyToDate(year = 2000, doy = 055)
#' @export

doyToDate <- function(year, doy){

  if(!is.numeric(year)) year <- as.numeric(year)
  if(!is.numeric(doy)) doy <- as.numeric(doy)
  if(length(year) != length(doy)) year <- rep_len(year, length(doy))

  leap_year <- seq(from = 1904, to = 2196, by = 4)
  theMonths <- NULL
  theDays <- NULL

  for(i in seq_along(doy)){
    if(year[i] %in% leap_year){
      daysPerMonth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    } else{
      daysPerMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }
    days <- c(0, cumsum(daysPerMonth))

    month <- max(which(doy[i] > days))
    theMonths <- c(theMonths, month)

    day <- doy[i] - days[month]
    theDays <- c(theDays, day)
  }

  return(ISOdate(year, theMonths, theDays, hour = 0, tz = "GMT"))
}

#' Scale values
#'
#' Scale the values of a matrix to a specific range
#' @param mat [\code{matrix(1)}]\cr the matrix, in which values shall be scaled
#'   to \code{range}.
#' @param range [\code{integerish(2)}]\cr the range, to which values in
#'   \code{mat} should be scaled to.
#' @return a matrix of scaled values
#' @export

scaleMat <- function(mat, range = NULL){
  
  assertMatrix(mat)
  assertNumeric(range, any.missing = FALSE, len = 2)
  
  # scale to range
  out <- scaleMatrixC(mat, range)

  return(out)
}

#' Set the scale of a plot
#'
#' @param attr [\code{data.frame(1)}]\cr the attribute table from which to
#'   derive values.
#' @param params [\code{list(7)}]\cr the parameters of the geom that shall be
#'   scaled.
#' @return a list of parameters to a grob.
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
      assertTRUE(length(params[[pos]]) > 1)
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

#' Print messages
#'
#' This is a wrapper around \code{\link{message}}.
#' @param msg [\code{character(1)}]\cr the message.
#' @param silent [\code{logical(1)}]\cr should all messages except errors be
#'   suppressed (\code{TRUE}), or should all messages be printed (\code{FALSE},
#'   default)?
#' @export

blablabla <- function(msg, silent = FALSE){
  if(missing(msg)) msg <- ""
  if(!silent){
    message(msg)
  }
}

#' Transform degree to radians
#' @param degree [\code{numeric(1)}]\cr the degree value to transform.
#' @return a radians value
#' @export

rad <- function(degree){
  assertNumeric(degree)
  (degree * pi)/180
}

#' Create non-linear palettes
#'
#' A wrapper around \code{colorRampPalette} to create palettes where the number
#' of interpolated colours is not linear, i.e. there may be a different number
#' of colour values between the colors that shall be interpolated.
#' @param colors [\code{character(.)}]\cr colours to interpolate. must be a valid
#'   argument to \code{\link[grDevices]{col2rgb}}.
#' @param steps [\code{integerish(.)}]\cr the number of colours between each of
#'   \code{'colors'}.
#' @param ... arguments to pass to \code{colorRampPalette}.
#' @examples
#' myColours <- terrain.colors(5)
#'
#' linearPalette <- rtPalette(colors = myColours)
#' pie(x = rep(1, 50), col = linearPalette(50), labels = NA)
#'
#' nonlinearPalette <- rtPalette(colors = myColours, steps = c(20, 20, 2, 2))
#' pie(x = rep(1, 50), col = nonlinearPalette(50), labels = NA)
#' @importFrom grDevices colorRampPalette col2rgb
#' @export

rtPalette <- function(colors, steps = NULL, ...){
  # found at https://menugget.blogspot.com/2011/11/define-color-steps-for-colorramppalette.html
  
  if(is.null(steps)) steps <- rep(0, (length(colors)-1))
  if(length(steps) != length(colors)-1) stop("Must have one less 'steps' value than 'colors'")
  
  fillValues <- cumsum(rep(1, length(colors)) + c(0, steps))
  RGB <- matrix(NA, nrow = 3, ncol = fillValues[length(fillValues)])
  RGB[,fillValues] <- col2rgb(colors)
  
  for(i in which(steps > 0)){
    col.start = RGB[,fillValues[i]]
    col.end = RGB[,fillValues[i+1]]
    for(j in seq(3)){
      vals <- seq(col.start[j], col.end[j], length.out = steps[i] + 2)[2:(2+steps[i] - 1)]  
      RGB[j,(fillValues[i] + 1):(fillValues[i + 1] - 1)] <- vals
    }
  }
  
  newValues <- rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal <- colorRampPalette(newValues, ...)
  
  return(pal)
}