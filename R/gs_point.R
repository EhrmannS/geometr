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
#'
#' geom <- gs_point(anchor = data.frame(x = 40, y = 40), window = data.frame(x = c(40, 70), y = c(40, 70)))
#'
#' \dontrun{
#'
#' input <- gtRasters$continuous
#'
#' # create points interactively
#' myPoints <- gs_point(template = input, vertices = 5)
#' visualise(geom = myPoints, linecol = "deeppink", new = FALSE)
#'
#' anExtent <- gs_rectangle(myPoints)
#' visualise(geom = anExtent, linecol = "green", new = FALSE)
#' }
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
  window <- .testWindow(x = window, ...)
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
    if(i == 1){
      visualise(raster = template$obj)
    }
    coords <- gt_locate(samples = vertices, panel = tempName, silent = TRUE, ...)
    window <- tibble(x = c(0, dims[2]),
                     y = c(0, dims[1]))
    anchor <- tibble(x = coords$x,
                     y = coords$y,
                     fid = 1:vertices)
  } else if(anchor$type == "geom"){
    if(is.null(window)){
      window <- tibble(x = c(min(anchor$obj@window$x),
                             max(anchor$obj@window$x)),
                       y = c(min(anchor$obj@window$y),
                             max(anchor$obj@window$y)))
    }
    anchor <- anchor$obj@vert
  } else if(anchor$type == "df"){
    if(is.null(window)){
      window <- tibble(x = c(min(anchor$obj$x),
                             max(anchor$obj$x)),
                       y = c(min(anchor$obj$y),
                             max(anchor$obj$y)))
    }
    anchor <- anchor$obj
    if(!"fid" %in% names(anchor)){
      anchor <- bind_cols(anchor, fid = seq_along(anchor$x))
    }
  }

  theGeom <- new(Class = "geom",
                 type = "point",
                 vert = anchor,
                 feat = tibble(fid = unique(anchor$fid), gid = unique(anchor$fid)),
                 group = tibble(gid = unique(anchor$fid)),
                 window = tibble(x = c(min(window$x), max(window$x), max(window$x), min(window$x), min(window$x)),
                                 y = c(min(window$y), min(window$y), max(window$y), max(window$y), min(window$y))),
                 scale = "absolute",
                 crs = as.character(projection),
                 history = list(paste0("geometry was created as 'point'.")))

  invisible(theGeom)
}
