#' Transform a \code{geom} to a grob object
#'
#' An object of class \code{\link{grob}} is the grid-package representation of a
#' \code{geom} and is used for plotting.
#' @param geom [\code{geom}]\cr Object of class \code{\link{geom}}.
#' @param theme [\code{list(7)}]\cr Visualising options; see
#' @param ... instead of providing a \code{gtTheme}, you can also determine
#'   specific graphic parameters (see \code{\link{gpar}}) separately; see
#'   \code{\link{setTheme}} for details.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#'   \code{\link{polylineGrob}} or a \code{\link{pathGrob}}.
#' @importFrom checkmate assertNames assertSubset assertList
#' @importFrom grid gpar unit pointsGrob gList pathGrob polylineGrob clipGrob
#' @export

gt_as_grob <- function(geom = NULL, theme = gtTheme, ...){

  assertClass(geom, classes = "geom")
  assertClass(x = theme, classes = "gtTheme", null.ok = TRUE)

  # scale it to relative, if it's not
  if(geom@scale == "absolute"){
    outGeom <- gt_scale(geom = geom, to = "relative")
  } else{
    outGeom <- geom
  }

  featureType <- geom@type
  vert <- getVertices(x = outGeom)

  attr <- getTable(x = geom)
  pars <- makeColours(input = geom, theme = theme, ...)
  pars <- pars$params

  ids <- eval(parse(text = pars$scale$to), envir = attr)
  if(is.factor(ids)) ids <- as.character(ids)

  if(featureType %in% "point"){

    geomGrob <- pointsGrob(x = unit(vert$x, "npc"),
                           y = unit(vert$y, "npc"),
                           pch = pars$pointsymbol,
                           size = unit(pars$pointsize, "char"),
                           gp = gpar(
                             col = pars$linecol,
                             fill = pars$fillcol))

  } else if(featureType %in% "line"){

    geomGrob <- polylineGrob(x = unit(vert$x, "npc"),
                             y = unit(vert$y, "npc"),
                             id = as.numeric(as.factor(vert$fid)),
                             name = ids,
                             gp = gpar(col = pars$linecol,
                                       lty = pars$linetype,
                                       lwd = pars$linewidth))

  } else if(featureType %in% "polygon"){

    geomGrob <- NULL
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
        geomGrob <- pathGrob(x = tempCoords$x,
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
        geomGrob <- gList(geomGrob,
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
  return(geomGrob)

}