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
#' @param ... [various]\cr graphical parameters to \code{\link{locate}}, in case
#'   points are sketched; see \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @family shapes
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
#'   visualise(geom = ., linecol = "deeppink")
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
  anchorIsDF <- testDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
  if(anchorIsDF){
    colnames(anchor) <- tolower(colnames(anchor))
    assertNames(names(anchor), must.include = c("x", "y"), subset.of = c( "fid", "vid", "x", "y"))
    if(!"vid" %in% names(anchor)){
      anchor <- bind_cols(vid = rep(1, times = length(anchor$x)), anchor)
    }
    if(!"fid" %in% names(anchor)){
      anchor <- bind_cols(fid = seq_along(anchor$x), anchor)
    }
    features <- length(unique(anchor$fid))
  }
  anchorIsGeom <- testClass(anchor, classes = "geom")
  if(anchorIsGeom){
    # fid and vid values need to be transformed
    if(anchor@type != "point"){
      anchor@coords$fid <- seq_along(anchor@coords$fid)
      anchor@coords$vid <- rep(1, times = length(anchor@coords$vid))
    }
    features <- length(unique(anchor@coords$fid))
  }
  windowExists <- !testNull(window)
  if(windowExists){
    assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
    colnames(window) <- tolower(colnames(window))
    assertNames(names(window), must.include = c("x", "y"))
  }
  templateExists <- !testNull(template)
  if(templateExists){
    assert(
      testClass(template, "RasterLayer"),
      testClass(template, "matrix")
    )
  }
  if(!anchorIsDF & !anchorIsGeom & !templateExists){
    stop("please provide either 'anchor' or 'template'.")
  }
  if(!anchorIsDF & !anchorIsGeom){
    assertIntegerish(vertices, min.len = 1, lower = 1, any.missing = FALSE)
  } else{
    assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)
  }

  # get some raster properties
  if(templateExists){
    if(testClass(template, "RasterLayer")){
      tempName <- names(template)
      dims <- dim(template)
      projection <- crs(template, asText = TRUE)
    } else{
      tempName <- "layer"
      dims <- dim(template)
      projection <- NA
    }
  } else{
    tempName <- "layer"
    projection <- NA
  }

  # if anchor does not exists, make it
  if(!anchorIsDF & !anchorIsGeom){
    message("please click the ", vertices, " vertices.")
    visualise(raster = template)
    coords <- locate(samples = vertices, panel = tempName, silent = TRUE, ...)
    window <- tibble(x = c(0, dims[2]),
                     y = c(0, dims[1]))
    anchor <- tibble(fid = 1:vertices,
                     vid = 1,
                     x = coords$x,
                     y = coords$y)
  } else if(anchorIsGeom){
    if(!windowExists){
      window <- anchor@window
    }
    anchor <- anchor@coords
  } else if(anchorIsDF){
    if(!windowExists){
      window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                       y = c(min(anchor$y), max(anchor$y)))
    }
  }

  theGeom <- new(Class = "geom",
                 type = "point",
                 coords = anchor,
                 attr = tibble(fid = unique(anchor$fid), n = as.data.frame(table(anchor$fid))$Freq),
                 window = tibble(x = rep(window$x, each = 2), y = c(window$y, rev(window$y))),
                 scale = "absolute",
                 crs = as.character(projection),
                 history = list(paste0("geometry was created as 'point'")))

  invisible(theGeom)
}
