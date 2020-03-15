#' Create a point \code{geom}
#'
#' Create a point geometry (of class \code{\link{geom}}) either by specifying
#' anchor values or by sketching it.
#' @param anchor [\code{geom(1)}|\code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. It must include column names \code{x}, \code{y} and
#'   optionally a custom \code{fid}.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param vertices [\code{integer(1)}]\cr number of vertices.
#' @param sketch [\code{raster(1)}]\cr raster object that serves as template to
#'   sketch polygons.
#' @param ... [various]\cr graphical parameters to \code{\link{gt_locate}}, in
#'   case points are sketched; see \code{\link[grid]{gpar}}
#' @return An invisible \code{geom}.
#' @family geometry shapes
#' @details The arguments \code{anchor} and \code{sketch} indicate how the line
#'   is created: \itemize{ \item if \code{anchor} is set, the line is created
#'   parametrically from the given objects' points, \item if an object is set in
#'   \code{sketch}, this is used to create the \code{geom} interactively, by
#'   clicking into the plot.}
#' @examples
#' # 1. create points programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#'
#' # if no window is set, the bounding box will be set as window
#' (aGeom <- gs_point(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' library(magrittr)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' gs_point(anchor = coords, window = window) %>%
#'   visualise(linecol = "green")
#'
#' # when a geom is used in 'anchor', its properties are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' gs_point(anchor = aGeom) %>%
#'   visualise(geom = .)
#' \donttest{
#' # 2. sketch two points by clicking into a template
#' gs_point(sketch = gtRasters$continuous, vertices = 2) %>%
#'   visualise(geom = ., linecol = "green", pointsymbol = 5, new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testNull assert testClass
#'   assertLogical assertIntegerish
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom methods new
#' @export

gs_point <- function(anchor = NULL, window = NULL, vertices = 1, sketch = NULL,
                     ...){

  # check arguments
  anchor <- .testAnchor(x = anchor)
  theWindow <- .testWindow(x = window)
  assertIntegerish(vertices, min.len = 1, lower = 1, any.missing = FALSE)

  # sketch the geometry
  if(!is.null(sketch)){
    hist <- paste0("object was sketched as 'point' geom.")

    template <- .testTemplate(x = sketch, ...)
    theGeom <- gt_sketch(template = template$obj,
                         shape = "point",
                         features = vertices,
                         ...)

  } else {

    if(anchor$type == "geom"){
      hist <- paste0("object was cast to 'point' geom.")

      if(is.null(theWindow)){
        theWindow <- anchor$obj@window
      }
      theVertices <- anchor$obj@point
      theFeatures <- anchor$obj@feature
      theGroups <- anchor$obj@group
      projection <- getCRS(x = anchor$obj)

    } else if(anchor$type == "df"){
      hist <- paste0("object was created as 'point' geom.")

      if(is.null(theWindow)){
        theWindow = tibble(x = c(min(anchor$obj$x), max(anchor$obj$x), max(anchor$obj$x), min(anchor$obj$x), min(anchor$obj$x)),
                           y = c(min(anchor$obj$y), min(anchor$obj$y), max(anchor$obj$y), max(anchor$obj$y), min(anchor$obj$y)))
      }
      theVertices <- bind_cols(anchor$obj)
      if(!"fid" %in% names(theVertices)){
        theVertices <- bind_cols(theVertices, fid = seq_along(theVertices$x))
        vertices <- dim(theVertices)[1]
        theFeatures <- tibble(fid = 1:vertices, gid = 1:vertices)
        theGroups <- tibble(gid = 1:vertices)
      } else {
        vertices <- unique(theVertices$fid)
        theFeatures <- tibble(fid = vertices, gid = seq_along(vertices))
        theGroups <- tibble(gid = seq_along(vertices))
      }
      theFeatures <- list(geometry = theFeatures)
      theGroups <- list(geometry = theGroups)
      projection <- NA

    }

    theGeom <- new(Class = "geom",
                   type = "point",
                   point = theVertices,
                   feature = theFeatures,
                   group = theGroups,
                   window = theWindow,
                   scale = "absolute",
                   crs = as.character(projection),
                   history = c(getHistory(x = anchor$obj), list(hist)))
  }

  invisible(theGeom)
}
