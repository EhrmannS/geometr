#' Transform a spatial object to a grob
#'
#' @param input the object to transform to class \code{grob}.
#' @param theme [\code{gtTheme(1)}]\cr the theme from which to take parameters.
#' @param ... instead of providing a modified \code{theme}, you can also
#'   determine specific graphic parameters (see \code{\link{gpar}}) separately;
#'   see \code{\link{setTheme}} for details.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#'   \code{\link{polylineGrob}} or a \code{\link{pathGrob}}.
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
#' @importFrom grDevices colorRampPalette
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertNames assertSubset assertList
#' @importFrom grid gpar unit pointsGrob gList pathGrob polylineGrob clipGrob
#' @export
setMethod(f = "gc_grob",
          signature = "geom",
          definition = function(input = NULL, theme = gtTheme, ...){

            # capture display arguments
            displayArgs <- exprs(...)
            featureType <- getType(x = input)[2]
            thePoints <- getPoints(x = input)
            theFeatures <- getTable(x = input, slot = "feature")
            theGroups <- getTable(x = input, slot = "group")

            if(featureType == "point"){
              attr <- left_join(x = thePoints, y = theFeatures, by = "fid")
              attr <- left_join(x = attr, y = theGroups, by = "gid")
            } else {
              attr <- left_join(x = theFeatures, y = theGroups, by = "gid")
            }

            # scale input to relative, if it's not
            if(input@scale == "absolute"){
              outGeom <- gt_scale(geom = input, to = "relative")
            } else{
              outGeom <- input
            }

            point <- getPoints(x = outGeom)
            params <- theme@vector

            # select only displayArgs that are part of the valid parameters.
            displayArgs <- displayArgs[names(displayArgs) %in% names(params)]

            if(length(displayArgs) != 0){
              tempArgs <- displayArgs
            } else{
              tempArgs <- setNames(list(params$scale$to), params$scale$x)
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
                  toEval <- thisArg
                  toRamp <- params[[which(names(params) %in% thisArgName)]]
                  makeWarning <- TRUE
                } else{
                  toEval <- as.symbol(params$scale$to)
                  toRamp <- thisArg
                  makeWarning <- FALSE
                }

                vals <- eval(parse(text = paste0(toEval)), envir = attr)
                params$scale$x <- thisArgName
                params$scale$to <- toEval

                # figure out numeric representations of 'vals'
                temp <- suppressWarnings(as.numeric(as.character(vals)))
                if(!all(is.na(temp))){
                  valsNum <- temp
                  uniqueValsNum <- unique(temp)
                } else {
                  valsNum <- as.numeric(as.factor(vals))
                  uniqueValsNum <- as.numeric(as.factor(unique(vals)))
                }

                # if the argument is a colour argument, construct a color ramp from two or more values
                if(thisArgName %in% c("linecol", "fillcol")){

                  if(makeWarning){
                    if(length(uniqueValsNum) > 1){
                      if(length(toRamp) <= 1){
                        warning(paste0("please provide a theme with at least two values for '", thisArgName, "' to make a color gradient between."))
                      }
                    }
                  }

                  uniqueColours <- colorRampPalette(colors = toRamp)(length(uniqueValsNum))
                  breaks <- c(min(uniqueValsNum)-1, uniqueValsNum)
                  valCuts <- cut(valsNum, breaks = breaks, include.lowest = FALSE)
                  tempOut <- uniqueColours[valCuts]

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

                  uniquItems <- toRamp
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
              if(i == 1) next

              # determine value and name of the i-th display argument
              thisArg <- defaultArgs[[i]][[1]]
              thisArgName <- names(defaultArgs)[i]
              pos <- which(names(params) %in% thisArgName)

              # repeat the default args as many times as there are features
              params[[pos]] <- rep(thisArg, dim(attr)[1])

            }

            ids <- eval(parse(text = "fid"), envir = attr)

            if(input@type %in% "point"){

              out <- pointsGrob(x = unit(point$x, "npc"),
                                y = unit(point$y, "npc"),
                                pch = params$pointsymbol,
                                name = ids,
                                size = unit(params$pointsize, "char"),
                                gp = gpar(
                                  col = params$linecol,
                                  fill = params$fillcol))

            } else if(input@type %in% "line"){

              out <- polylineGrob(x = unit(point$x, "npc"),
                                  y = unit(point$y, "npc"),
                                  id = as.numeric(as.factor(point$fid)),
                                  name = ids,
                                  gp = gpar(col = params$linecol,
                                            lty = params$linetype,
                                            lwd = params$linewidth))

            } else if(input@type %in% "polygon"){

              out <- NULL
              for(i in seq_along(unique(attr$fid))){

                theID <- unique(attr$fid)[i]
                tempIDs <- attr[attr$fid == theID, ]
                tempCoords <- point[point$fid %in% tempIDs$fid, ]

                # determine subpaths by searching for duplicates. Whenever there is a
                # duplicate in the vertices, the next vertex is part of the next subpaths
                dups <- as.numeric(duplicated(tempCoords[c("x", "y")]))
                dups <- c(0, dups[-length(dups)])
                tempCoords$vid <- 1 + cumsum(dups)
                if(i == 1){
                  out <- pathGrob(x = tempCoords$x,
                                  y = tempCoords$y,
                                  id = as.numeric(as.factor(tempCoords$vid)),
                                  rule = "evenodd",
                                  name = ids[i],
                                  gp = gpar(
                                    col = params$linecol[i],
                                    fill = params$fillcol[i],
                                    lty = params$linetype[i],
                                    lwd = params$linewidth[i]))
                } else{
                  out <- gList(out,
                               pathGrob(x = tempCoords$x,
                                        y = tempCoords$y,
                                        id = as.numeric(as.factor(tempCoords$vid)),
                                        rule = "evenodd",
                                        name = ids[i],
                                        gp = gpar(
                                          col = params$linecol[i],
                                          fill = params$fillcol[i],
                                          lty = params$linetype[i],
                                          lwd = params$linewidth[i])))
                }
              }
            }

            return(out)
          }
)
