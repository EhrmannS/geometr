#' Create a point geometry
#'
#' Create a point geometry (of class \code{\link{geom}}) either by specifying
#' its parameters or by sketching it.
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the geometry.
#' @param vertices [\code{integer(1)}]\cr number of vertices.
#' @param ... [various]\cr additional arguments; see Details.
#' @return An invisible \code{geom}.
#' @family geometry shapes
#' @details  The arguments \code{anchor} and \code{template} indicate how the
#'   geometry is created: \itemize{ \item \code{anchor}: if set, the geometry is
#'   created parametrically, the input provided is used to parameterise the
#'   geometry. \item \code{template}: if set, the geometry is created
#'   interactively, by clicking into the plot.}
#'
#'   Possible additional arguments are: \itemize{ \item verbose = TRUE/FALSE
#'   \item graphical parameters to \code{\link{gt_locate}}, in case points are
#'   sketched; see \code{\link{gpar}}}.
#' @examples
#' library(magrittr)
#'
#' # create a points programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#'
#' # if no window is set, the bounding box (i.e. min/max values) will be set as window
#' (aGeom <- gs_point(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' visualise(geom = gs_point(anchor = coords))
#' gs_point(anchor = coords, window = window) %>%
#'   visualise(geom = ., linecol = "green", new = FALSE)
#'
#' # when a geom is used in 'anchor', its properties (e.g. 'window') are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' gs_point(anchor = aGeom) %>%
#'   visualise(geom = .)
#' @importFrom checkmate testDataFrame assertNames testNull assert testClass
#'   assertLogical assertIntegerish
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom methods new
#' @export

gs_point <- function(anchor = NULL, window = NULL, template = NULL,
                     vertices = NULL, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor, ...)
  theWindow <- .testWindow(x = window, ...)
  template <- .testTemplate(x = template, ...)

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  if(is.null(anchor)){
    assertIntegerish(vertices, min.len = 1, lower = 1, any.missing = FALSE)
  } else{
    assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)
  }

  # get some raster properties
  if(!is.null(template)){
    if(template$type == "RasterLayer"){
      tempName <- names(template$obj)
      dims <- dim(template$obj)
      projection <- getCRS(x = template$obj)
    } else{
      tempName <- "layer"
      dims <- dim(template$obj)
      projection <- NA
    }
  } else{
    tempName <- "layer"
    projection <- NA
  }

  # if anchor does not exists, make it
  if(is.null(anchor)){
    message("please click the ", vertices, " vertices.")
    visualise(raster = template$obj)
    coords <- gt_locate(samples = vertices, panel = tempName, silent = TRUE, ...)
    theWindow <- tibble(x = c(0, dims[2], dims[2], 0, 0),
                        y = c(0, 0, dims[1], dims[1], 0))
    theVertices <- tibble(x = coords$x,
                          y = coords$y,
                          fid = 1:vertices)
    theFeatures <- tibble(fid = 1:vertices, gid = 1:vertices)
    theGroups <- tibble(gid = 1:vertices)
  } else if(anchor$type == "geom"){
    if(is.null(theWindow)){
      theWindow <- anchor$obj@window
    }
    theVertices <- anchor$obj@vert
    theFeatures <- anchor$obj@feat
    theGroups <- anchor$obj@group
  } else if(anchor$type == "df"){
    if(is.null(theWindow)){
      theWindow = tibble(x = c(min(anchor$obj$x), max(anchor$obj$x), max(anchor$obj$x), min(anchor$obj$x), min(anchor$obj$x)),
                         y = c(min(anchor$obj$y), min(anchor$obj$y), max(anchor$obj$y), max(anchor$obj$y), min(anchor$obj$y)))
    }
    theVertices <- bind_cols(anchor$obj)
    if(!"fid" %in% names(theVertices)){
      theVertices <- bind_cols(theVertices, fid = seq_along(theVertices$x))
    }
    vertices <- dim(theVertices)[1]
    theFeatures = tibble(fid = 1:vertices, gid = 1:vertices)
    theGroups = tibble(gid = 1:vertices)
  }

  theGeom <- new(Class = "geom",
                 type = "point",
                 vert = theVertices,
                 feat = theFeatures,
                 group = theGroups,
                 window = theWindow,
                 scale = "absolute",
                 crs = as.character(projection),
                 history = list(paste0("geometry was created as 'point'.")))

  invisible(theGeom)
}
