#' Update the theme parameters
#'
#' @param x [\code{list(1)}]\cr any spatial object to plot.
#' @param theme [\code{gtTheme(1)}]\cr the theme that shall be updated.
#' @importFrom dplyr left_join

.updateTheme <- function(x, theme = gtTheme){

  featureType <- getType(x = x)
  thePoints <- getPoints(x = x)

  if(featureType[1] == "grid"){

    theme@scale$param <- "fillcol"
    theme@scale$to <- "gid"

    # if the values are colours, don't plot the legend
    vals <- sample(getFeatures(x = x)$values, 10)
    if(any(as.character(vals) %in% colors()) | any(grepl(pattern = "\\#(.{6,8})", x = vals))){
      theme@legend$plot <- FALSE
    }
  }

  if(dim(thePoints)[1] != 0){
    # determine the items for which a scale should be built
    items <- suppressMessages(sort(gt_pull(obj = x, var = theme@scale$to)))

    # only use automatic range, if it haven't been set before
    if(is.null(theme@scale$range)){
      if(!is.null(items)){
        theme@scale$range <- c(head(items, 1), tail(items, 1))
      }
    }

    # only use automatic bins, if they haven't been set before
    if(is.null(theme@scale$bins)){
      if(!is.null(items)){
        theme@scale$bins <- length(items)
      }
    }
  } else {
    theme@title$plot <- FALSE
    theme@legend$plot <- FALSE
    theme@box$plot <- FALSE
  }


  return(theme)
}