#' Create a geometry randomly
#'
#' This function creates a random geometry
#' @param type [\code{character(1)}]\cr Either one of the three main feature
#'   types \code{"point"}, \code{"line"} or \code{"polygon"}, or more
#'   specifically one of their subtypes, e.g. \code{"hexagon"}.
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the geometry.
#' @param vertices [\code{integerish(1)}]\cr the number of vertices the geometry
#'   should have; only meaningful if \code{type} does not indicate the number of
#'   vertices already. If left at \code{NULL} the minimum number of vertices for
#'   the \code{geom} type, i.e. 1 for \code{point}, 2 for \code{line} and 3 for
#'   \code{polygon}.
#' @family shapes
#' @examples
#' library(magrittr)
#' input <- matrix(nrow = 100, ncol = 100, data = 0)
#'
#' # create a random geometry with five vertices
#' set.seed(1)
#' someGeom <- gs_random(type = "polygon", vertices = 5)
#' visualise(geom = someGeom)
#'
#' # in case template is given, this serves as source for the window extent
#' someGeom <- gs_random(template = input) %>%
#'   visualise(geom = ., new = FALSE, linecol = "red")
#' @importFrom stats runif
#' @export

gs_random <- function(type = "point", template = NULL, vertices = NULL){

  assertSubset(type, choices = c("point", "line", "rectangle", "square", "polygon", "spline", "ellipse", "circle", "triangle", "hexagon"))
  templateExists <- !testNull(template)
  if(templateExists){
    isRaster <- testClass(template, "RasterLayer")
    isMatrix <- testClass(template, "matrix")
    if(!isRaster & !isMatrix){
      stop("please provide either a RasterLayer or a matrix as 'template'.")
    }
  }
  assertIntegerish(vertices, any.missing = FALSE, len = 1, null.ok = TRUE)

  if(type %in% "point"){
    if(is.null(vertices)){
      vertices <- 1
    }
    outType  <- type
    anchor <- tibble(fid = 1:vertices,
                     vid = 1:vertices,
                     x = runif(vertices),
                     y = runif(vertices))
    # } else if(type %in% c("line", "spline")){
    #   if(is.null(vertices)){
    #     vertices <- 2
    #   }
    #   outType <- "line"
    #
  } else{
    if(is.null(vertices)){
      vertices <- 3
    }
    outType <- "polygon"
    anchor <- tibble(fid = 1,
                     vid = 1,
                     x = runif(vertices),
                     y = runif(vertices))
  }

  if(templateExists){
    window <- getExtent(template)
  } else{
    window <- tibble(x = c(0, 1),
                     y = c(0, 1))
  }

  theGeom <- new(Class = "geom",
                 type = outType,
                 coords = anchor,
                 attr = tibble(fid = unique(anchor$fid), n = 1),
                 window = window,
                 scale = "relative",
                 crs = as.character(NA),
                 history = list(paste0("geometry was created randomly")))

  if(templateExists){
    theGeom <- gt_scale(theGeom, to = "absolute")
  }

  invisible(theGeom)
}