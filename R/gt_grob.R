#' Transform a \code{geom} to and from a \code{grob} object
#'
#' An object of class \code{\link{grob}} is the grid-package representation of a
#' \code{geom} and is used for plotting.
#' @param input [\code{geom}]\cr Object to transform.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{input} (minimum and maximum values),
#'   specify this here.
#' @param theme [\code{list(7)}]\cr visualising options; see
#' @param ... instead of providing a \code{gtTheme}, you can also determine
#'   specific graphic parameters (see \code{\link{gpar}}) separately; see
#'   \code{\link{setTheme}} for details.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#'   \code{\link{polylineGrob}} or a \code{\link{pathGrob}}.
#' @family spatial classes
#' @importFrom checkmate assertNames assertSubset assertList
#' @importFrom grid gpar unit pointsGrob gList pathGrob polylineGrob clipGrob
#' @export

gt_grob <- function(input = NULL, window = NULL, theme = gtTheme, ...){

  isGeom <- testClass(x = input, classes = "geom")
  isGrob <- testClass(x = input, classes = "grob")
  assert(isGeom, isGrob, .var.name = "input")
  assertClass(x = theme, classes = "gtTheme", null.ok = TRUE)

  if(isGrob){
    assertClass(x = window, classes = "grob", null.ok = TRUE)

    sourceClass <- class(input)[1]
    if(sourceClass == "pointsGrob"){
      type <- "point"
    } else if(sourceClass == "polylineGrob"){
      type <- "line"
    } else if(sourceClass == "pathgrob"){
      type <- "polygon"
    }

    theCoords <- tibble(fid = 1, vid = seq_along(input$x), x = as.numeric(input$x), y = as.numeric(input$y))
    if(is.null(window)){
      theWindow <- tibble(x = rep(c(min(theCoords$x), max(theCoords$x)), each = 2),
                          y = c(min(theCoords$y), max(theCoords$y), max(theCoords$y), min(theCoords$y)))
      theScale <- "absolute"
    } else {
      theWindow <- tibble(x = rep(c(as.numeric(window$x), as.numeric(window$x) + as.numeric(window$width)), each = 2),
                          y = c(as.numeric(window$y), as.numeric(window$y) + as.numeric(window$height), as.numeric(window$y) + as.numeric(window$height), as.numeric(window$y)))
      theScale <- "relative"
    }
    theData <- tibble(fid = 1, gid = 1)
    history <- paste0("geometry was created with gt_grob() from an object of class '", sourceClass, "'")

    out <- new(Class = "geom",
               type = type,
               vert = theCoords,
               attr = theData,
               window = theWindow,
               scale = theScale,
               crs = NA_character_,
               history = list(history))
    if(theScale == "relative"){
      out <- gt_scale(geom = out, to = "absolute")
    }

  } else {

    # capture display arguments
    displayArgs <- exprs(..., .named = TRUE)

    # scale input to relative, if it's not
    if(input@scale == "absolute"){
      outGeom <- gt_scale(geom = input, to = "relative")
    } else{
      outGeom <- input
    }

    vert <- getVertices(x = outGeom)
    attr <- getTable(x = input)

    params <- theme@geom

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

    # process parameters that shall be changed ----
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
        } else{
          toEval <- as.symbol("fid")
          toRamp <- thisArg
        }

        vals <- eval(parse(text = paste0(toEval)), envir = attr)
        valsNum <- as.numeric(vals)
        uniqueVals <- unique(vals)
        uniqueValsNum <- as.numeric(uniqueVals)

        # if the argument is a colour argument, construct a color ramp from two or more values
        if(thisArgName %in% c("linecol", "fillcol")){
          params$scale$x <- thisArgName
          params$scale$cls <- thisArg

          uniqueColours <- colorRampPalette(colors = toRamp)(length(uniqueValsNum))
          breaks <- c(min(uniqueValsNum)-1, uniqueValsNum)
          valCuts <- cut(valsNum, breaks = breaks, include.lowest = FALSE)
          tempOut <- uniqueColours[valCuts]

        } else{
          tempOut <- rep_along(valsNum, thisArg)
        }

        params[[pos]] <- tempOut

      } else{
        params[[pos]] <- thisArg
      }
    }

    # process parameters that are default ----
    for(i in seq_along(defaultArgs)){
      if(i == 1) next

      # determine value and name of the i-th display argument
      thisArg <- defaultArgs[[i]][[1]]
      thisArgName <- names(defaultArgs)[i]
      pos <- which(names(params) %in% thisArgName)

      # repeat the default args as many times as there are features
      params[[pos]] <- rep(thisArg, dim(attr)[1])

    }

    ids <- eval(parse(text = params$scale$to), envir = attr)
    if(is.factor(ids)) ids <- as.character(ids)

    if(input@type %in% "point"){

      out <- pointsGrob(x = unit(vert$x, "npc"),
                             y = unit(vert$y, "npc"),
                             pch = params$pointsymbol,
                             size = unit(params$pointsize, "char"),
                             gp = gpar(
                               col = params$linecol,
                               fill = params$fillcol))

    } else if(input@type %in% "line"){

      out <- polylineGrob(x = unit(vert$x, "npc"),
                               y = unit(vert$y, "npc"),
                               id = as.numeric(as.factor(vert$fid)),
                               name = ids,
                               gp = gpar(col = params$linecol,
                                         lty = params$linetype,
                                         lwd = params$linewidth))

    } else if(input@type %in% "polygon"){

      out <- NULL
      for(i in seq_along(unique(attr$fid))){

        theID <- unique(attr$fid)[i]
        tempIDs <- attr[attr$fid == theID, ]
        tempCoords <- vert[vert$fid %in% tempIDs$fid, ]

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
  }

  return(out)

}