#' Make the grob of a plot
#'
#' @param x the object to transform to class \code{grob}.
#' @param featureType the type of feature to make a grob from.
#' @param plotValues the plot values.
#' @param scaleValues the scale values.
#' @param rows in case it's a grid, the number of rows.
#' @param cols in case it's a grid, the number of cols.
#' @param plotParams new plotting parameters specified
#'   via the quick options in \code{\link{visualise}}.
#' @param theme the theme from which to take parameters.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#'   a \code{\link{polylineGrob}}, a \code{\link{pathGrob}} or a
#'   \code{\link{rasterGrob}}.
#' @importFrom rlang exprs rep_along
#' @importFrom grDevices colorRampPalette colors rgb
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertNames assertSubset assertList
#' @importFrom dplyr left_join group_by mutate
#' @importFrom grid gpar unit pointsGrob gList pathGrob polylineGrob clipGrob
#'   rasterGrob

.makeGrob <- function(x, featureType, plotValues, scaleValues, plotParams, rows = rows, cols = cols, theme = gtTheme){

  if(theme@box$plot){

    # featureType <- getType(x = x)
    if(featureType[1] != "grid") {

      params <- list(linecol = "black",
                     fillcol = NA,
                     linetype = "solid",
                     linewidth = 1,
                     pointsize = 0.5,
                     pointsymbol = 20)

      # # select only 'plotParams' that are part of the valid parameters.
      tempArgs <- plotParams[names(plotParams) %in% names(params)]

      # if the parameter to scale has not beend defined as quick parameter, add it to 'tempArgs'
      if(!theme@scale$param %in% names(plotParams) & !is.na(theme@scale$param)){
        tempArgs <- c(tempArgs, setNames(list(theme@scale$to), theme@scale$param))
      }

      # process parameters that shall be changed
      for(i in seq_along(tempArgs)){

        # determine value and name of the i-th display argument
        theVar <- tempArgs[[i]]
        theParam <- names(tempArgs)[i]
        pos <- which(names(params) %in% theParam)

        if(i == 1){
          items <- plotValues
        } else {
          items <- suppressMessages(gt_pull(obj = x, var = theVar))
        }
        num <- suppressWarnings(as.numeric(as.character(theVar)))

        # if the argument is a colour argument, construct a color ramp from two or more values
        if(theParam %in% c("linecol", "fillcol")){

          if(!is.null(theme@scale$bins)){
            thebins <- theme@scale$bins
          } else {
            thebins <- length(items)
          }

          if(is.null(items)){
            cols <- theVar
            if(!any(as.character(cols) %in% colors()) & !any(grepl(pattern = "\\#(.{6,8})", x = cols))){
              stop(paste0("'", cols, "' was neither found as column in the object to plot, nor is it a valid colour."))
            }
            tempOut <- colorRampPalette(colors = cols)(length(theVar))
          } else {
            cols <- theme@parameters$colours
            allColours <- colorRampPalette(colors = cols)(thebins)

            valCuts <- rank(items, ties.method = "min")
            tempOut <- allColours[valCuts]
          }

          if(!is.null(theme@parameters$missingcol)){
            tempOut[is.na(tempOut)] <- theme@parameters$missingcol
          }

        } else if(theParam %in% c("linewidth", "pointsize")){

          if(!is.null(theme@scale$bins)){
            thebins <- theme@scale$bins
          } else {
            thebins <- length(items)
          }

          if(is.null(items)){
            if(is.na(num)){
              stop(paste0("'", theVar, "' was neither found as column in the object to plot, nor is it a valid ", theParam, "."))
            }
            tempOut <- num
          } else {
            allSizes <- seq(from = min(theme@parameters[[theParam]], na.rm = TRUE),
                            to = max(theme@parameters[[theParam]], na.rm = TRUE),
                            length.out = thebins)

            if(is.null(items)){
              tempOut <- rep(num, length(allSizes))
            } else {
              valCuts <- rank(items, ties.method = "min")
              tempOut <- allSizes[valCuts]
            }
          }

        } else if(theParam %in% c("pointsymbol", "linetype")){

          if(!is.null(theme@scale$bins)){
            thebins <- theme@scale$bins
          } else {
            thebins <- length(items)
          }

          if(is.null(items)){
            if(is.na(num)){
              stop(paste0("'", theVar, "' was neither found as column in the object to plot, nor is it a valid ", theParam, "."))
            }
            tempOut <- num
          } else {
            allSymbols <- theme@parameters[[theParam]]

            if(is.null(items)){
              tempOut <- rep(num, length(allSymbols))
            } else {
              valCuts <- rank(items, ties.method = "min")
              tempOut <- allSymbols[valCuts]
            }
          }

        }

        params[[pos]] <- tempOut
      }

      # rescale values between 0 and 1
      x <- gt_scale(obj = x, range = tibble(x = c(0, 1), y = c(0, 1)))

      point <- getPoints(x = x)
      ids <- unique(point$fid)

      if(featureType[1] %in% "point"){

        out <- pointsGrob(x = unit(point$x, "npc"),
                          y = unit(point$y, "npc"),
                          pch = params$pointsymbol,
                          name = ids,
                          size = unit(params$pointsize, "char"),
                          gp = gpar(
                            col = params$linecol,
                            fill = params$fillcol))

      } else if(featureType[1] %in% "line"){

        out <- polylineGrob(x = unit(point$x, "npc"),
                            y = unit(point$y, "npc"),
                            id = as.numeric(as.factor(point$fid)),
                            name = ids,
                            gp = gpar(col = params$linecol,
                                      lty = params$linetype,
                                      lwd = params$linewidth))

      } else if(featureType[1] %in% "polygon"){

        dups <- group_by(.data = point, fid, x, y)
        dups <- mutate(.data = dups,
                       is_dup = duplicated(x) & duplicated(y),
                       is_odd = seq_along(fid) %% 2 == 0,
                       dup = as.integer(is_dup & is_odd))
        dups <- dups[["dup"]]
        dups <- c(0, dups[-length(dups)])
        vids <- 1 + cumsum(dups)

        out <- pathGrob(x = point$x,
                        y = point$y,
                        id = vids,
                        pathId = point$fid,
                        rule = "evenodd",
                        name = ids,
                        gp = gpar(
                          col = params$linecol,
                          fill = params$fillcol,
                          lty = params$linetype,
                          lwd = params$linewidth))
      }

    } else {

      # vals <- getFeatures(x = x)$values

      if(testCharacter(x = plotValues, pattern = "\\#(.{6,8})")){
        theColours <- as.vector(plotValues)
      } else {
        # items <- sort(gt_pull(obj = x, var = theme@scale$to))

        scaleBreaks <- c(scaleValues[1]-1, scaleValues)
        valCuts <- cut(plotValues, breaks = scaleBreaks, include.lowest = TRUE)

        colours <- theme@parameters$colours
        allColours <- colorRampPalette(colors = colours)(theme@scale$bins)

        theColours <- allColours[valCuts]
      }

      out <- rasterGrob(x = unit(0, "npc"),
                        y = unit(0, "npc"),
                        width = unit(1, "npc"),
                        height = unit(1, "npc"),
                        hjust = 0,
                        vjust = 0,
                        image = matrix(data = theColours, nrow = rows, ncol = cols, byrow = TRUE),
                        name = "theRaster",
                        interpolate = FALSE)
    }

    if(is(out) != "gList"){
      out <- gList(out)
    }

  } else {
    out <- NULL
  }

  return(out)

}

