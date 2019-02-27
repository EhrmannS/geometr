#' Transform geometry to grob
#'
#' A \code{\link{grob}} (graphical object) is the grid-package representation of
#' a \code{geom} and is used for plotting.
#' @param geom [\code{geom}]\cr Object of class \code{\link{geom}}.
#' @param theme [\code{list(7)}]\cr Visualising options; see
#' @param ... instead of providing a \code{gtTheme}, you can also determine
#'   specific graphic parameters (see \code{\link{gpar}}) separately; see
#'   \code{\link{setTheme}} for details.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#'   \code{\link{linesGrob}}, \code{\link{polylineGrob}} or a
#'   \code{\link{polygonGrob}}.
#' @examples
#' coords <- data.frame(x = c(40, 70, 70, 50, 40, 60, 70, 40, 60,
#'                            40, 10, 20, 30, 30, 20, 50, 40, 10, 20),
#'                      y = c(40, 40, 60, 70, 40, 20, 40, 10, 20,
#'                            40, 20, 20, 50, 40, 40, 70, 40, 20, 60),
#'                      fid = c(1, 1, 1, 1, 2, 2, 2, 3, 3,
#'                              3, 3, 3, 4, 4, 4, 5, 5, 5, 5))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#'
#' aGrob <- gt_as_grob(geom = aGeom)
#' str(aGrob)
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
  coords <- outGeom@coords

  attr <- getTable(x = geom)
  # if a "hole" (in an fid) has been defined, assign a common id
  if(any(names(attr) == "in_fid")){
    c1 <- ifelse(is.na(attr$in_fid), attr$fid,  attr$in_fid)
    c2 <- attr$fid
    attr$fid <- c1
    attr$in_fid <- c2
  } else{
    attr$in_fid <- attr$fid
  }
  pars <- scaleParameters(attr = attr, params = theme@geom)
  # https://github.com/ropensci/rgbif/blob/d7fdbdc4fcba8015f5e1aeadfc8cc789cf3ed155/R/occ_issues.r

  if(featureType %in% c("point")){

    geomGrob <- pointsGrob(x = unit(coords$x, "npc"),
                           y = unit(coords$y, "npc"),
                           pch = theme@geom$pointsymbol,
                           size = unit(theme@geom$pointsize, "char"),
                           gp = gpar(
                             col = pars$line,
                             fill = pars$fill,
                             ...))

  } else if(featureType %in% "line"){

    geomGrob <- polylineGrob(x = coords$x,
                             y = coords$y,
                             id = as.numeric(as.factor(tempCoords$fid)),
                             gp = gpar(col = pars$line,
                                       lty = pars$linetype,
                                       lwd = pars$linewidth,
                                       ...))

  } else if(featureType %in% c("polygon")){

    geomGrob <- NULL
    for(i in seq_along(unique(attr$fid))){

      theID <- unique(attr$fid)[i]
      tempIDs <- attr[attr$fid == theID, ]
      tempCoords <- coords[coords$fid %in% tempIDs$in_fid, ]
      if(i == 1){
        geomGrob <- pathGrob(x = tempCoords$x,
                             y = tempCoords$y,
                             id = as.numeric(as.factor(tempCoords$fid)),
                             rule = "evenodd",
                             gp = gpar(
                               col = pars$line[i],
                               fill = pars$fill[i],
                               lty = pars$linetype[i],
                               lwd = pars$linewidth[i],
                               ...))
      } else{
        geomGrob <- gList(geomGrob,
                          pathGrob(x = tempCoords$x,
                                   y = tempCoords$y,
                                   id = as.numeric(as.factor(tempCoords$fid)),
                                   rule = "evenodd",
                                   gp = gpar(
                                     col = pars$line[i],
                                     fill = pars$fill[i],
                                     lty = pars$linetype[i],
                                     lwd = pars$linewidth[i],
                                     ...)))
      }

    }

  }
  return(geomGrob)

}