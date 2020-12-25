#' Update the theme according to disply arguments
#'
#' @param x [\code{list(1)}]\cr any spatial object to plot.
#' @param newParams [\code{named list(.)}]\cr new plotting parameters specified
#'   via the quick options in \code{\link{visualise}}.
#' @param theme [\code{gtTheme(1)}]\cr the theme that shall be updated.
#' @importFrom dplyr left_join
#' @importFrom checkmate testCharacter testNames
#' @importFrom grDevices colorRampPalette rgb

.updateTheme <- function(x = NULL,
                         newParams = NULL,
                         theme = gtTheme){

  featureType <- getType(x = x)

  if(featureType[1] == "grid"){
    vals <- getFeatures(x = x)$values

    if(testNames(getName(x), permutation.of = c("red", "green", "blue"))) {

      red <- getFeatures(x = getLayer(x = x, layer = "red")[[1]])$values
      red[is.na(red)] <- 255L

      green <- getFeatures(x = getLayer(x = x, layer = "green")[[1]])$values
      green[is.na(green)] <- 255L

      blue <- getFeatures(x = getLayer(x = x, layer = "blue")[[1]])$values
      blue[is.na(blue)] <- 255L

      alpha <- rep(255, length(blue))
      alpha[is.na(red)] <- 0L
      alpha[is.na(green)] <- 0L
      alpha[is.na(blue)] <- 0L

      theColours <- rgb(red = red, green = green, blue = blue, alpha = alpha, maxColorValue = 255)
      theme@legend$plot <- FALSE

    } else if(testCharacter(x = vals, pattern = "\\#(.{6,8})")){

      theColours <- as.vector(vals)

    } else {

      allValues <- sortUniqueC(vals[!is.na(vals)])
      nrVals <- length(allValues)
      targetColours <- theme@parameters$fillcol

      # make palette of all values in the theme, determine breaking points as
      # values of the raster and "intersect" the palette with them
      allColours <- colorRampPalette(colors = targetColours)(length(allValues))
      breaksTemp <- c(allValues[1]-1, allValues)
      valCuts <- cut(vals, breaks = breaksTemp, include.lowest = TRUE)
      theColours <- allColours[valCuts]
    }

    theme@scale$param <- "fillcol"
    theme@scale$rows <- getRows(x = x)
    theme@scale$cols <- getCols(x = x)
    theme@parameters$fillcol <- theColours

  } else {

    thePoints <- getPoints(x = x)
    theFeatures <- getFeatures(x = x)
    theGroups <- getGroups(x = x)
    params <- theme@parameters

    if(featureType[1] == "point"){
      attr <- left_join(x = thePoints, y = theFeatures, by = "fid")
      attr <- left_join(x = attr, y = theGroups, by = "gid")
    } else {
      attr <- left_join(x = theFeatures, y = theGroups, by = "gid")
    }

    # select only 'newParams' that are part of the valid parameters.
    tempArgs <- newParams[names(newParams) %in% names(params)]
    if(length(tempArgs) == 0){
      tempArgs <- setNames(list(theme@scale$to), theme@scale$param)
    }
    if(!any(names(tempArgs) == "fillcol")){
      tempArgs <- c(tempArgs, setNames(list(NA_character_), "fillcol"))
    }

    if(all(c("linecol", "fillcol") %in% names(tempArgs))){
      if(all(is.na(c(tempArgs[[1]], tempArgs[[2]])))){
        stop("Either 'linecol' or 'fillcol' must contain a value other than 'NA'")
      }
    }

    defaultArgs <- params[!names(params) %in% names(tempArgs)]

    # # process parameters that shall be changed
    for(i in seq_along(tempArgs)){

      # determine value and name of the i-th display argument
      thisArg <- tempArgs[[i]]
      thisArgName <- names(tempArgs)[i]
      assertChoice(x = thisArgName, choices = names(params))
      pos <- which(names(params) %in% thisArgName)

      # check whether the parameter value is a column in 'attr', otherwise take
      # the default scale$to parameter
      if(!is.na(as.character(thisArg))){
        if(as.character(thisArg) %in% colnames(attr)){
          toEval <- as.character(thisArg)
          toRamp <- params[[which(names(params) %in% thisArgName)]]
          makeWarning <- TRUE
        } else {
          toEval <- as.symbol(theme@scale$to)
          toRamp <- thisArg
          makeWarning <- FALSE
        }

        vals <- attr[[toEval]]
        theme@scale$to <- toEval

        # figure out numeric representations of 'vals'
        temp <- suppressWarnings(as.numeric(as.character(vals)))
        if(!all(is.na(temp))){
          valsNum <- temp
          uniqueValsNum <- sort(unique(temp[!is.na(temp)]))
        } else {
          valsNum <- as.numeric(as.factor(vals))
          uniqueValsNum <- sort(as.numeric(as.factor(unique(vals))))
        }

        # if the argument is a colour argument, construct a color ramp from two or more values
        if(thisArgName %in% c("linecol", "fillcol")){

          # test whether 'toRamp' is a colour
          if(!any(as.character(toRamp) %in% colors()) & !any(grepl(pattern = "\\#(.{6,8})", x = toRamp))){
            stop(paste0(toRamp, " was neither found as column in the object to plot, nor is it a valid colour."))
          }

          if(makeWarning){
            if(length(uniqueValsNum) > 1){
              if(length(toRamp) <= 1){
                warning(paste0("please provide a theme with at least two values for '", thisArgName, "' to make a color gradient between."))
              }
            }
          }

          if(theme@scale$identity){
            theme@scale$bins <- NULL
          }

          if(is.null(theme@scale$range)){
            if(is.null(theme@scale$bins)){
              nColours <- length(uniqueValsNum)
              breaks <- c(min(uniqueValsNum, na.rm = T)-1, uniqueValsNum)
            } else {
              nColours <- theme@scale$bins
              breaks <- seq(from = min(uniqueValsNum), to = max(uniqueValsNum), length.out = theme@scale$bins+1)
            }
          } else {
            if(is.null(theme@scale$bins)){
              nColours <- length(uniqueValsNum)
              breaks <- seq(from = theme@scale$range[1], to = theme@scale$range[2], length.out = length(uniqueValsNum)+1)
            } else {
              nColours <- theme@scale$bins
              breaks <- seq(from = theme@scale$range[1], to = theme@scale$range[2], length.out = theme@scale$bins+1)
            }
          }
          plotColours <- colorRampPalette(colors = toRamp)(nColours)

          if(theme@scale$identity){
            if(length(unique(plotColours)) < length(unique(uniqueValsNum))){
              stop(paste0("the property '", thisArgName, "' (", paste0(toRamp, collapse = ", "), ") does not allow a palette of ", length(unique(uniqueValsNum)), " unique colours."))
            }
          } else {

          }

          valCuts <- cut(valsNum, breaks = breaks, include.lowest = FALSE, labels = FALSE)
          tempOut <- plotColours[valCuts]
          if(!is.null(theme@parameters$missingcol)){
            tempOut[is.na(tempOut)] <- theme@parameters$missingcol
          }

        } else if(thisArgName %in% c("linewidth", "pointsize")){

          if(makeWarning){
            if(length(uniqueValsNum) > 1){
              if(length(toRamp) <= 1){
                warning(paste0("please provide a theme with at least two values for '", thisArgName, "' to scale between."))
              }
            }
          }

          uniquItems <- seq(from = min(toRamp, na.rm = TRUE), to = max(toRamp, na.rm = TRUE), length.out = length(uniqueValsNum))
          breaks <- c(min(uniqueValsNum)-1, uniqueValsNum)
          valCuts <- cut(valsNum, breaks = breaks, include.lowest = FALSE)
          tempOut <- uniquItems[valCuts]

        } else if(thisArgName %in% c("pointsymbol", "linetype")){

          # perhaps a warning/stop if there are more than 12 (or so...) values?
          if(makeWarning){
            if(length(uniqueValsNum) > 1){
              if(length(toRamp) < length(uniqueValsNum)){
                toRamp <- rep(toRamp, length.out = length(uniqueValsNum))
                warning(paste0("please provide a theme with ", length(uniqueValsNum)," values for the unique values of '", thisArgName, "'."))
              }
            }
          }

          uniquItems <- rep(toRamp, times = length(uniqueValsNum))
          breaks <- c(0, uniqueValsNum[order(uniqueValsNum)])
          valCuts <- cut(valsNum, breaks = breaks, include.lowest = FALSE)
          tempOut <- uniquItems[valCuts]

        }

        params[[pos]] <- tempOut

      } else{
        params[[pos]] <- thisArg
      }
    }


    # process parameters that are default
    for(i in seq_along(defaultArgs)){

      # determine value and name of the i-th display argument
      thisArg <- defaultArgs[[i]][[1]]
      thisArgName <- names(defaultArgs)[i]
      pos <- which(names(params) %in% thisArgName)

      # repeat the default args as many times as there are features
      params[[pos]] <- rep(thisArg, dim(attr)[1])

    }

    theme@parameters <- params

  }

  return(theme)
}