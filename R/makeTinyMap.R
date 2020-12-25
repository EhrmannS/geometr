#' Make a tiny map
#'
#' A tiny map is used via the show method of a geom.
#' @param geom [\code{geom}]\cr the geom from which to create a tiny map.

.makeTinyMap <- function(geom = NULL){

  assertClass(x = geom, classes = "geom")
  theWindow <- getWindow(x = geom)
  thePoints <- getPoints(x = geom)

  # get the window labels
  xmin <- round(min(theWindow$x), 2)
  xminFill <- paste0(rep(" ", nchar(xmin)), collapse = "")
  xmax <- round(max(theWindow$x), 2)
  xmaxFill <- paste0(rep(" ", nchar(xmax)), collapse = "")
  ymin <- round(min(theWindow$y), 2)
  ymax <- round(max(theWindow$y), 2)

  # define symbols
  full <- '\u25C9'
  half <- '\u25CE'
  quarter <- '\u25CB'
  empty <- '\u25CC'

  # create vector of symbols
  filled <- NULL
  nrPoints <- dim(thePoints)[1]
  for(i in 1:4){
    for(j in 1:4){
      x <- xmin + c(((xmax-xmin)/4 * j) - (xmax-xmin)/4, (xmax-xmin)/4 * j)
      y <- ymin + c(((ymax-ymin)/4 * i) - (ymax-ymin)/4, (ymax-ymin)/4 * i)
      target <- data.frame(x = c(x[1], x[2], x[2], x[1], x[1]),
                           y = c(y[1], y[1], y[2], y[2], y[1]))

      inside <- pointInGeomC(vert = as.matrix(thePoints[c("x", "y")]),
                             geom = as.matrix(target),
                             invert = FALSE)
      pointsInside <- sum(inside != 0)
      ratio <- pointsInside/nrPoints
      if(ratio <= 1/16){
        recent <- empty
      } else if(ratio > 1/16 & ratio <= 1/8){
        recent <- quarter
      } else if(ratio > 1/8 & ratio <= 1/4){
        recent <- half
      } else if(ratio > 1/4){
        recent <- full
      }

      filled <- c(filled, recent)

    }
  }

  # populate the tiny map
  out <- paste0(c("", xminFill, ymax, "\n",
                  "          ", xminFill, filled[13], filled[14], filled[15], filled[16], xmaxFill, "\n",
                  "          ", xminFill, filled[9], filled[10], filled[11], filled[12], xmaxFill, "\n",
                  "          ", xminFill, filled[5], filled[6], filled[7], filled[8], xmaxFill, "\n",
                  "          ", xmin, filled[1], filled[2], filled[3], filled[4], xmax, "\n",
                  "          ", xminFill, ymin, ""))

  return(out)

}