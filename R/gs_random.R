#' Create a \code{geom} randomly
#'
#' This function creates a random geometry
#' @param type [\code{character(1)}]\cr Either one of the three main feature
#'   types \code{"point"}, \code{"line"} or \code{"polygon"}, or more
#'   specifically one of their subtypes, e.g. \code{"hexagon"}.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box [0, 1] (minimum and maximum values), specify this
#'   here.
#' @param vertices [\code{integerish(1)}]\cr the number of vertices the geometry
#'   should have; only meaningful if \code{type} does not indicate the number of
#'   vertices already. If left at \code{NULL} the minimum number of vertices for
#'   the \code{geom} type, i.e. 1 for \code{point}, 2 for \code{line} and 3 for
#'   \code{polygon}.
#' @param ... [various]\cr additional arguments.
#' @family geometry shapes
#' @examples
#' input <- matrix(nrow = 100, ncol = 100, data = 0)
#'
#' # create a random polygon with five vertices
#' set.seed(1)
#' someGeom <- gs_random(type = "polygon", vertices = 5)
#' visualise(geom = someGeom)
#'
#' # in case template is given, this serves as source for the window extent
#' library(magrittr)
#' gs_random(template = input) %>%
#'   visualise(geom = ., new = FALSE, linecol = "red")
#' @importFrom stats runif
#' @export

gs_random <- function(type = "point", window = NULL, vertices = NULL, ...){

  theCoices <- c("point", "line", "polygon", "triangle", "rectangle", "square", "hexagon", "random")
  assertSubset(x = type, choices = theCoices)
  theWindow <- .testWindow(x = window, ...)
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
    theWindow = tibble(x = c(0, 1, 1, 0, 0),
                       y = c(0, 0, 1, 1, 0))
  }

  theFeatures <- tibble(fid = unique(anchor$fid), gid = unique(anchor$fid))
  theGroups <- tibble(gid = unique(anchor$fid))

  theGeom <- new(Class = "geom",
                 type = outType,
                 point = anchor,
                 feature = list(geometry = theFeatures),
                 group = list(geometry = theGroups),
                 window = theWindow,
                 scale = "relative",
                 crs = as.character(NA),
                 history = list(paste0("geometry was created randomly")))

  invisible(theGeom)
}