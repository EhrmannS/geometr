#' Create a line geometry
#'
#' Create a line geometry (of class \code{\link{geom}}) either by specifying its
#' parameters or by sketching it.
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the geometry.
#' @param features [\code{integerish(1)}]\cr number of geometries to create.
#' @param vertices [\code{integerish(.)}]\cr number of vertices per geometry;
#'   will be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param ... [various]\cr graphical parameters to \code{\link{locate}}, in case
#'   points are sketched; see \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @family shapes
#' @details The arguments \code{anchor} and \code{template} indicate how the
#'   geometry is created: \itemize{ \item \code{anchor}: if set, the geometry is
#'   created parametrically, the input provided is used to parameterise the
#'   geometry. \item \code{template}: if set, the geometry is created
#'   interactively, by clicking into the plot.}
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
#' (aGeom <- gs_line(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' visualise(geom = gs_line(anchor = coords))
#' gs_line(anchor = coords, window = window) %>%
#'   visualise(geom = ., linecol = "green", new = FALSE)
#'
#' # when a geom is used in 'anchor', its properties (e.g. 'window') are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' gs_line(anchor = aGeom) %>%
#'   visualise(geom = ., linecol = "deeppink")
#'
#' \dontrun{
#'
#' input <- gtRasters$continuous
#'
#' # create points interactively
#' myLine <- gs_line(template = input, vertices = 5)
#' visualise(geom = myLine, linecol = "deeppink", new = FALSE)
#'
#' anExtent <- gs_rectangle(myLine)
#' visualise(geom = anExtent, linecol = "green", new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testClass testNull assertDataFrame assert assertIntegerish
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @export

gs_line <- function(anchor = NULL, window = NULL, template = NULL, features = 1,
                    vertices = NULL, ...){

  # check arguments
  anchorIsDF <- testDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
  if(anchorIsDF){
    colnames(anchor) <- tolower(colnames(anchor))
    assertNames(names(anchor), must.include = c("x", "y"), subset.of = c( "fid", "vid", "x", "y"))
    if(!"vid" %in% names(anchor)){
      anchor <- bind_cols(vid = seq_along(anchor$x), anchor)
    }
    if(!"fid" %in% names(anchor)){
      anchor <- bind_cols(fid = rep(1, times = length(anchor$x)), anchor)
    }
    features <- length(unique(anchor$fid))
  }
  anchorIsGeom <- testClass(anchor, classes = "geom")
  if(anchorIsGeom){
    # fid and vid values need to be transformed
    if(anchor@type == "point"){
      anchor@vert$fid <- rep(1, length(anchor@vert$fid))
      anchor@vert$vid <- seq_along(anchor@vert$vid)
    }
    features <- length(unique(anchor@vert$fid))
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
  assertIntegerish(features, len = 1, lower = 1)
  if(!anchorIsDF & !anchorIsGeom){
    assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE)
    if(length(vertices) != features){
      vertices <- rep(vertices, length.out = features)
    }
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

  nodes <- fids <- NULL
  for(i in 1:features){

    # if anchor does not exists, make it
    if(!anchorIsDF & !anchorIsGeom){

      message(paste0("please click the ", vertices[i], " vertices."))
      visualise(raster = template)
      theClicks <- locate(samples = vertices[i], panel = tempName, silent = TRUE, ...)
      window <- tibble(x = c(0, dims[2]),
                       y = c(0, dims[1]))
      tempAnchor <- tibble(fid = i,
                           vid = 1:vertices[i],
                           x = theClicks$x,
                           y = theClicks$y)

    } else if(anchorIsGeom){
      if(!windowExists){
        window <- anchor@window
      }
      tempAnchor <- anchor@vert[anchor@vert$fid == i,]
    } else if(anchorIsDF){
      if(!windowExists){
        window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                         y = c(min(anchor$y), max(anchor$y)))
      }
      tempAnchor <- anchor[anchor$fid == i, ]
    }

    theNodes <- tempAnchor[c("fid", "vid", "x", "y")]

    nodes <- bind_rows(nodes, theNodes)
    fids <- c(fids, length(unique(theNodes$vid)))
  }

  out <- new(Class = "geom",
             type = "line",
             vert = nodes,
             attr = tibble(fid = unique(nodes$fid), gid = unique(nodes$fid)),
             window = tibble(x = rep(c(min(window$x), max(window$x)), each = 2), y = c(min(window$y), max(window$y), max(window$y), min(window$y))),
             scale = "absolute",
             crs = as.character(projection),
             history = list(paste0("geometry was created as 'line'")))

  invisible(out)
}


# gs_curve <- function(){
#
# }


# gs_circle <- function(){
#
# }
# gs_ellipse <- function(){
#
# }