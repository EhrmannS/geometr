#' Transform a spatial object to a grob
#'
#' @param input the object to transform to class \code{grob}.
#' @param theme [\code{gtTheme(1)}]\cr the theme from which to take parameters.
#' @param ... instead of providing a modified \code{theme}, you can also
#'   determine specific graphic parameters (see \code{\link{gpar}}) separately;
#'   see \code{\link{setTheme}} for details.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#' @family spatial classes
#' @name gc_grob
#' @rdname gc_grob
NULL

# generic ----
#' @rdname gc_grob
#' @name gc_grob
#' @export
if(!isGeneric("gc_grob")){
  setGeneric(name = "gc_grob",
             def = function(input, theme, ...){
               standardGeneric("gc_grob")
             }
  )
}

# geom ----
#' @rdname gc_grob
#' @importFrom rlang exprs rep_along
#' @importFrom grDevices colorRampPalette colors
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertNames assertSubset assertList
#' @importFrom dplyr group_by mutate
#' @importFrom grid gpar unit pointsGrob gList pathGrob polylineGrob clipGrob
#' @export
setMethod(f = "gc_grob",
          signature = "geom",
          definition = function(input = NULL, theme = gtTheme, ...){

            # capture display arguments
            displayArgs <- exprs(...)
            featureType <- getType(x = input)[2]
            thePoints <- getPoints(x = input)
            theFeatures <- getFeatures(x = input)
            theGroups <- getGroups(x = input)

            if(featureType == "point"){
              attr <- left_join(x = thePoints, y = theFeatures, by = "fid")
              attr <- left_join(x = attr, y = theGroups, by = "gid")
            } else {
              attr <- left_join(x = theFeatures, y = theGroups, by = "gid")
            }

            # scale input to relative, if it's not
            outGeom <- input
            if(input@scale == "absolute"){
              outGeom <- gt_scale(geom = outGeom, to = "relative")
            }

            point <- getPoints(x = outGeom)
            params <- theme@vector
            scale <- theme@scale

            # select only displayArgs that are part of the valid parameters.
            tempArgs <- displayArgs[names(displayArgs) %in% names(params)]
            if(length(tempArgs) == 0){
              tempArgs <- setNames(list(scale$to), scale$param)
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

            # process parameters that shall be changed
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
                  toEval <- as.symbol(scale$to)
                  toRamp <- thisArg
                  makeWarning <- FALSE
                }

                vals <- attr[[toEval]]
                scale$to <- thisArgName
                scale$to <- toEval

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

                  if(scale$identity){
                    scale$bins <- NULL
                  }

                  if(is.null(scale$range)){
                    if(is.null(scale$bins)){
                      nColours <- length(uniqueValsNum)
                      breaks <- c(min(uniqueValsNum, na.rm = T)-1, uniqueValsNum)
                    } else {
                      nColours <- scale$bins
                      breaks <- seq(from = min(uniqueValsNum), to = max(uniqueValsNum), length.out = scale$bins+1)
                    }
                  } else {
                    if(is.null(scale$bins)){
                      nColours <- length(uniqueValsNum)
                      breaks <- seq(from = scale$range[1], to = scale$range[2], length.out = length(uniqueValsNum)+1)
                    } else {
                      nColours <- scale$bins
                      breaks <- seq(from = scale$range[1], to = scale$range[2], length.out = scale$bins+1)
                    }
                  }
                  plotColours <- colorRampPalette(colors = toRamp)(nColours)

                  if(scale$identity){
                    if(length(unique(plotColours)) < length(unique(uniqueValsNum))){
                      stop(paste0("the property '", thisArgName, "' (", paste0(toRamp, collapse = ", "), ") does not allow a palette of ", length(unique(uniqueValsNum)), " unique colours."))
                    }
                  } else {

                  }

                  valCuts <- cut(valsNum, breaks = breaks, include.lowest = FALSE, labels = FALSE)
                  tempOut <- plotColours[valCuts]
                  if(!is.null(theme@vector$missingcol)){
                    tempOut[is.na(tempOut)] <- theme@vector$missingcol
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

            ids <- attr[["fid"]]

            if(featureType %in% "point"){

              out <- pointsGrob(x = unit(point$x, "npc"),
                                y = unit(point$y, "npc"),
                                pch = params$pointsymbol,
                                name = ids,
                                size = unit(params$pointsize, "char"),
                                gp = gpar(
                                  col = params$linecol,
                                  fill = params$fillcol))

            } else if(featureType %in% "line"){

              out <- polylineGrob(x = unit(point$x, "npc"),
                                  y = unit(point$y, "npc"),
                                  id = as.numeric(as.factor(point$fid)),
                                  name = ids,
                                  gp = gpar(col = params$linecol,
                                            lty = params$linetype,
                                            lwd = params$linewidth))

            } else if(featureType %in% "polygon"){

              # start_time <- Sys.time()
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

            return(out)
          }
)
