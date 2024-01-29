#' Create a \code{geom} randomly
#'
#' This function creates a random geometry
#' @param type [character(1)][character]\cr Either one of the three main feature
#'   types \code{"point"}, \code{"line"} or \code{"polygon"}, \code{"random"},
#'   or more specifically one of their subtypes, e.g. \code{"hexagon"} (subtypes
#'   currently not yet supported).
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @param vertices [integerish(1)][integer]\cr the number of vertices the geometry
#'   should have; only meaningful if \code{type} does not indicate the number of
#'   vertices already. If left at \code{NULL} the minimum number of vertices for
#'   the \code{geom} type, i.e. 1 for \code{point}, 2 for \code{line} and 3 for
#'   \code{polygon}.
#' @return A \code{geom}.
#' @family geometry shapes
#' @examples
#' # create a random polygon with five vertices
#' set.seed(1)
#' someGeom <- geo_random(type = "polygon", vertices = 5)
#' geo_vis(geom = someGeom, linecol = "#FFB000")
#'
#' # in case template is given, this serves as source for the window extent
#' geo_vis(geom = geo_random(), linecol = "#B24223", pointsymbol = 22, new = FALSE)
#' @importFrom stats runif
#' @export

geo_random <- function(type = "point", window = NULL, vertices = NULL){

  theCoices <- c("point", "line", "polygon")
  assertSubset(x = type, choices = c(theCoices, "random"))
  if(type == "random"){
    type <- sample(x = theCoices, size = 1)
  }
  theWindow <- .testWindow(x = window)
  assertIntegerish(vertices, any.missing = FALSE, len = 1, null.ok = TRUE)

  if(type == "point"){
    if(is.null(vertices)){
      vertices <- 1
    }
    outType  <- type
    anchor <- tibble(x = runif(vertices),
                     y = runif(vertices),
                     fid = 1:vertices)
  } else if(type == "line"){
    if(is.null(vertices)){
        vertices <- 2
      } else {
        if(vertices < 2){
          stop("I can't create a line with less than two vertices.")
        }
      }
      outType <- type
      anchor <- tibble(x = runif(vertices),
                       y = runif(vertices),
                       fid = 1)
  } else if(type == "polygon"){
    if(is.null(vertices)){
      vertices <- 3
    } else {
      if(vertices < 3){
        stop("I can't create a polygon with less than three vertices.")
      }
    }
    outType <- type
    xVerts <- runif(vertices)
    yVerts <- runif(vertices)
    anchor <- tibble(x = c(xVerts, xVerts[1]),
                     y = c(yVerts, yVerts[1]),
                     fid = 1)
  }

  if(is.null(theWindow)){
    theWindow = tibble(x = c(0, 1),
                       y = c(0, 1))
  }

  theFeatures <- tibble(fid = unique(anchor$fid), gid = unique(anchor$fid))
  theGroups <- tibble(gid = unique(anchor$fid))

  theData <- list(features = theFeatures, groups = theGroups)

  theGeom <- new(Class = "geom",
                 type = outType,
                 label = paste0(type, "_geom"),
                 geometry = anchor,
                 data = theData,
                 window = theWindow,
                 crs = as.character(NA),
                 provenance = list(paste0("geometry was created randomly")))

  invisible(theGeom)
}