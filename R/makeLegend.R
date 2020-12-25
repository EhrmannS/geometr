#' Make the legend of a plot
#'
#' @param x [\code{list(.)}]\cr the theme from which to build the legend.
#' @param type [\code{character(1)}]\cr whether it is a \code{"raster"} or a
#'   \code{"vector"} legend.
#' @importFrom checkmate assertChoice
#' @importFrom grid textGrob rasterGrob rectGrob gpar gTree gList unit

.makeLegend <- function(x = NULL, theme = NULL){

  featureType <- getType(x = x)
  theParam <- theme@scale$param
  theName <- theme@scale$to
  out <- list(obj = NULL, maxVal = NULL, posX = NULL, posY = NULL)

  if(theme@legend$plot){
    if(featureType[1] == "grid"){
      cols <- theme@parameters$fillcol
      ids <- getFeatures(x = x)$gid

      cols <- unique(cols[order(ids)])
      ids <- unique(sort(ids))

    } else{
      cols <- theme@parameters[[theme@scale$param]]
      ids <- getFeatures(x = x)$gid

    }

    tickValues <- seq_along(cols)

    # determine the tick values and labels
    if(length(tickValues) > theme@legend$bins){
      tickValues <- quantile(tickValues, probs = seq(0, 1, length.out = theme@legend$bins+1), type = 1, names = FALSE)
    }

    legendLabels <- ids[tickValues]
    legendCols <- cols
    if(theme@legend$ascending){
      legendPos <- unit(tickValues, "native")
    } else{
      legendPos <- rev(unit(tickValues, "native"))
    }



    # if(featureType[1] == "vector"){
    #   out <- list("out" = NULL, "hasLegend" = NULL, "params" = NULL, "legend" = NULL)
    #
    #   thePoints <- getPoints(x = x)
    #   theFeatures <- getFeatures(x = x)
    #   theGroups <- getGroups(x = x)
    #
    #   tempArgs <- displayArgs[names(displayArgs) %in% names(theme@vector)]
    #   if(length(tempArgs) == 0){
    #     tempArgs <- setNames(list(theme@scale$to), theme@scale$param)
    #   }
    #
    #   # make a table of relevant features
    #   attr <- thePoints
    #   if(!is.null(theFeatures)){
    #     attr <- left_join(x = attr, y = theFeatures, by = "fid")
    #   }
    #   if(!is.null(theGroups)){
    #     attr <- left_join(x = attr, y = theGroups, by = "gid")
    #   }
    #
    #   # test whether vertices are outside of 'window'
    #   inWindow <- pointInGeomC(vert = as.matrix(thePoints[c("x", "y")]),
    #                            geom = as.matrix(theWindow[c("x", "y")]),
    #                            invert = FALSE)
    #   inWindow <- inWindow[-5] != 0
    #
    #   if(!any(inWindow)){
    #     warning("no vertices are within the plotting window.", immediate. = TRUE)
    #   } else if(!all(inWindow)){
    #     warning("some vertices are not within the plotting window.", immediate. = TRUE)
    #   }
    #
    #   if(!class(x)[1] %in% "geom"){
    #     x <- gc_geom(input = x)
    #   }
    #
    #
    #   allValues <- sapply(aGrob, function(x){
    #     if(suppressWarnings(all(!is.na(as.numeric(as.character(x$name)))))){
    #       as.numeric(as.character(x$name))
    #     } else {
    #       x$name
    #     }
    #   })
    #   allColours <- sapply(aGrob, function(x){
    #     x$gp$col
    #   })
    #   allFill <- sapply(aGrob, function(x){
    #     if(!is.null(x$gp$fill)){
    #       x$gp$fill
    #     } else {
    #       NA_integer_
    #     }
    #   })
    #   allPch <- sapply(aGrob, function(x){
    #     if(!is.null(x$pch)){
    #       x$pch
    #     } else {
    #       NA_integer_
    #     }
    #   })
    #   allSize <- sapply(aGrob, function(x){
    #     if(!is.null(x$size)){
    #       x$size
    #     } else {
    #       NA_real_
    #     }
    #   })
    #   allLty <- sapply(aGrob, function(x){
    #     if(!is.null(x$gp$lty)){
    #       x$gp$lty
    #     } else {
    #       NA_character_
    #     }
    #   })
    #   allLwd <- sapply(aGrob, function(x){
    #     if(!is.null(x$gp$lwd)){
    #       x$gp$lwd
    #     } else {
    #       NA_integer_
    #     }
    #   })
    #
    #   # make an overall table of parameters
    #   params <- tibble(fid = rev(allValues),
    #                    fillcol = rev(allFill),
    #                    pointsymbol = rev(allPch),
    #                    pointsize = rev(allSize),
    #                    linecol = rev(allColours),
    #                    linetype = rev(allLty),
    #                    linewidth = rev(allLwd))
    #   params <- left_join(x = params, y = attr, by = "fid")
    #
    #   # go through the defined display arguments ...
    #   legends <- list()
    #   for(i in seq_along(tempArgs)){
    #
    #     theArg <- names(tempArgs)[i]
    #     theVal <- as.character(tempArgs[[i]])
    #
    #     # ... construct the indices for selecting attributes
    #     if(is.null(theme@scale$range)){
    #       if(theVal %in% colnames(params)){
    #         uniqueVal <- params[[as.character(theVal)]][!duplicated(params[[as.character(theVal)]])]
    #         if(any(is.na(params[[theArg]]))){
    #           uniqueArg <- params[[theArg]][!duplicated(params[[as.character(theVal)]])][order(uniqueVal)][-length(uniqueVal)]
    #         } else {
    #           uniqueArg <- params[[theArg]][!duplicated(params[[as.character(theVal)]])][order(uniqueVal)]
    #         }
    #       } else {
    #         next
    #         # uniqueVal <- NA_character_#params[[as.character(theVal)]][!duplicated(params[[as.character(theVal)]])]
    #         # uniqueArg <- theVal
    #       }
    #       uniqueVal <- sort(uniqueVal)
    #       if(length(uniqueArg) > theme@legend$bins){
    #         tempTicks <- quantile(seq_along(uniqueArg), probs = seq(0, 1, length.out = theme@legend$bins), type = 1, names = FALSE)
    #       } else {
    #         tempTicks <- seq_along(uniqueArg)
    #       }
    #       legendVals <- uniqueVal[tempTicks]
    #     } else {
    #       uniqueVal <- seq(theme@scale$range[1], theme@scale$range[2])
    #       tempTicks <- quantile(uniqueVal, probs = seq(0, 1, length.out = theme@legend$bins), type = 1, names = FALSE)
    #       legendVals <- uniqueVal[tempTicks+1]
    #     }
    #
    #     tempLegend <- tibble(labels = legendVals,
    #                          pos = as.numeric(unit(tempTicks, "native")))
    #     names(tempLegend)[1] <- as.character(theVal)
    #
    #     legends <- c(legends, setNames(object = list(tempLegend), nm = theArg))
    #   }
    #
    #   # revert order if given
    #   if(!theme@legend$ascending){
    #     params <- params[dim(params)[1]:1,]
    #     legendNames <- names(legends)
    #     legends <- lapply(seq_along(legends), function(x){
    #       legends[[x]][dim(legends[[x]])[1]:1,]
    #     })
    #     names(legends) <- legendNames
    #   }
    #
    #   out$hasLegend <- TRUE
    #   out$params <- params
    #   out$legend <- legends
    #
    #
    # } else {
    #
    # }




    # cols <- getFeatures(x = x)$values
    # allValues <- sortUniqueC(cols[!is.na(cols)])
    # tickValues <- seq_along(cols)
    # targetColours <- theme@raster$fillcol
    # allColours <- colorRampPalette(colors = cols)(length(cols))


    # theParam <- names(obj$legend)[j]
    # theLegend <- obj$legend[[j]]
    # legendName <- names(theLegend[,1])

    # if(length(legendPos) == 1){
    #   maxYScale <- unit(as.numeric(legendPos[length(legendPos)]) + 1, "native")
    # } else {
    #   maxYScale <- unit(as.numeric(legendPos[which.max(legendPos)]) + 1, "native")
    # }
    # pushViewport(viewport(height = unit(1, "npc") * theme@legend$yRatio,
    #                       yscale = c(1, maxYScale),
    #                       name = theName))

    # this is a little hack to get all the values that are contained in the
    # object "into" the plotted object for later use (e.g. by gt_locate())
    legend_values <- textGrob(label = legendLabels,
                              name = "legend_values",
                              gp = gpar(col = NA))


    if(theParam %in% c("linecol", "fillcol")){

      # make sure that the NA colour is always at the bottom (this
      # seems to be not the case when $range has a value)

      legend_obj <- rasterGrob(x = unit(0, "npc") + unit(5, "points"),
                               width = unit(10, "points"),
                               height = unit(1, "npc"),
                               just = c("left"),
                               name = "legend_items",
                               image = rev(legendCols),
                               interpolate = FALSE)

      if(theme@legend$box$plot){
        legend_obj <- gList(
          legend_obj,
          rectGrob(x = unit(0, "npc") + unit(5, "points"),
                   width = unit(10, "points"),
                   just = c("left"),
                   name = "legend_box",
                   gp = gpar(col = theme@legend$box$colour,
                             fill = NA,
                             lty = theme@legend$box$linetype,
                             lwd = theme@legend$box$linewidth)))
      }

    } else if(theParam %in% "pointsize"){

      # theSizes <- sort(unique(unlist(obj$params[theParam], use.names = FALSE)))[theLegend$pos]
      # legend_obj <- pointsGrob(x = rep(unit(1, "npc") + theLayout$legendX[j], times = length(theLegend$pos)),
      #                   y = unit(theLegend$pos, "native") - unit(0.5, "native"),
      #                   pch = theme@vector$pointsymbol[1],
      #                   size = unit(theSizes, "char"),
      #                   name = "legend_items")

    } else if(theParam %in% "pointsymbol"){

      # theSymbols <- sort(unique(unlist(obj$params[theParam], use.names = FALSE)))[theLegend$pos]
      # legend_obj <- pointsGrob(x = rep(unit(1, "npc") + theLayout$legendX[j], length(theSymbols)),
      #                   y = unit(theLegend$pos, "native") - unit(0.5, "native"),
      #                   pch = theSymbols,
      #                   size = unit(max(theme@vector$pointsize), "char"),
      #                   name = "legend_items")

    } else if(theParam %in% c("linewidth")){

      # theWidths <- sort(unique(unlist(obj$params[theParam], use.names = FALSE)))[theLegend$pos]
      # legend_obj <- polylineGrob(x = rep(unit(c(1, 1), "npc") + unit.c(theLayout$legendX[j], theLayout$legendX[j] + unit(10, "points")), times = length(theLegend$pos)),
      #                     y = unit(rep(theLegend$pos, each = 2), "native") - unit(0.5, "native"),
      #                     id = rep(theLegend$pos, each = 2),
      #                     name = "legend_items",
      #                     gp = gpar(col = theme@vector$linecol[1],
      #                               lwd = theWidths,
      #                               lty = theme@vector$linetype[1]))

    } else if(theParam %in% c("linetype")){

      # theTypes <- sort(unique(unlist(obj$params[theParam], use.names = FALSE)))[theLegend$pos]
      # legend_obj <- polylineGrob(x = rep(unit(c(1, 1), "npc") + unit.c(theLayout$legendX[j], theLayout$legendX[j] + unit(10, "points")), times = length(theLegend$pos)),
      #                     y = unit(rep(theLegend$pos, each = 2), "native") - unit(0.5, "native"),
      #                     id = rep(theLegend$pos, each = 2),
      #                     name = "legend_items",
      #                     gp = gpar(col = theme@vector$linecol[1],
      #                               lwd = max(theme@vector$linewidth),
      #                               lty = theTypes))

    }

    if(theme@legend$label$plot){
      legend_labels <- textGrob(label = unlist(legendLabels, use.names = FALSE),
                                x = unit(0, "npc") + unit(20, "points"),
                                y = unit(legendPos, "native") - unit(0.5, "native"),
                                name = "legend_labels",
                                just = c("left"),
                                gp = gpar(fontsize = theme@legend$label$fontsize,
                                          col = theme@legend$label$colour))
    }


    out$obj <- gTree(children = gList(legend_values, legend_obj, legend_labels))
    out$maxVal <- unit(tail(tickValues, 1) + 1, "native")
  } else {
    out$obj <- NULL
    out$maxVal <- 0
  }

  if(theme@legend$position == "right"){
    out$posX <- 3
    out$posY <- 2
  } else {
    # out$posX <- 2
    # out$posY <- 4
  }

  return(out)
}

