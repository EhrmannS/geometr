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
#' @family geometry tools
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

    theCoords <- tibble(fid = 1, vid = seq_along(targetGrob$x), x = as.numeric(targetGrob$x), y = as.numeric(targetGrob$y))
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

    # scale it to relative, if it's not
    if(input@scale == "absolute"){
      outGeom <- gt_scale(geom = input, to = "relative")
    } else{
      outGeom <- input
    }

    featureType <- input@type
    vert <- getVertices(x = outGeom)

    attr <- getTable(x = input)
    pars <- makeColours(input = input, theme = theme, ...)
    pars <- pars$params

    ids <- eval(parse(text = pars$scale$to), envir = attr)
    if(is.factor(ids)) ids <- as.character(ids)

    if(featureType %in% "point"){

      out <- pointsGrob(x = unit(vert$x, "npc"),
                             y = unit(vert$y, "npc"),
                             pch = pars$pointsymbol,
                             size = unit(pars$pointsize, "char"),
                             gp = gpar(
                               col = pars$linecol,
                               fill = pars$fillcol))

    } else if(featureType %in% "line"){

      out <- polylineGrob(x = unit(vert$x, "npc"),
                               y = unit(vert$y, "npc"),
                               id = as.numeric(as.factor(vert$fid)),
                               name = ids,
                               gp = gpar(col = pars$linecol,
                                         lty = pars$linetype,
                                         lwd = pars$linewidth))

    } else if(featureType %in% "polygon"){

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
                            col = pars$linecol[i],
                            fill = pars$fillcol[i],
                            lty = pars$linetype[i],
                            lwd = pars$linewidth[i]))
        } else{
          out <- gList(out,
                       pathGrob(x = tempCoords$x,
                                y = tempCoords$y,
                                id = as.numeric(as.factor(tempCoords$vid)),
                                rule = "evenodd",
                                name = ids[i],
                                gp = gpar(
                                  col = pars$linecol[i],
                                  fill = pars$fillcol[i],
                                  lty = pars$linetype[i],
                                  lwd = pars$linewidth[i])))
        }

      }

    }
  }

  return(out)

}