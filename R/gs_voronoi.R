#' Create a voronoi tiling
#'
#' @param anchor [\code{geom} | \code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. In case of \code{data.frame}, it must include column
#'   names \code{x}, \code{y}, \code{fid} and optinal attributes; see Examples.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the tiling.
#' @param features [\code{integerish(1)}]\cr number of tiles to create.
#' @param ... [various]\cr graphical parameters to \code{\link{gt_locate}}, in case
#'   the tiling is sketched; see \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @family tilings
#' @examples
#' library(magrittr)
#'
#' # create voronoi polygons programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_point(anchor = coords, window = window)
#' visualise(geom = aGeom)
#'
#' tiles <- gs_voronoi(anchor = aGeom) %>%
#'   visualise(geom = ., new = FALSE)
#'
#' \dontrun{
#'
#' input <- gtRasters$continuous
#'
#' # create voronoi polygons interactively
#' tiles <- gs_voronoi(template = input)
#' visualise(geom = tiles, linecol = "orange", new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testClass testNull
#'   assertDataFrame assert assertIntegerish
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @importFrom deldir deldir tile.list
#' @export

gs_voronoi <- function(anchor = NULL, window = NULL, template = NULL, features = 3,
                       ...){

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
    features <- length(unique(anchor@vert$fid))
  }
  windowExists <- !testNull(window)
  if(windowExists){
    assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
    colnames(window) <- tolower(colnames(window))
    assertNames(names(window), must.include = c("x", "y"))
    window <- c(min(window$x), max(window$x), min(window$y), max(window$y))
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

  # get some raster properties
  if(templateExists){
    if(testClass(template, "RasterLayer")){
      tempName <- names(template)
      dims <- dim(template)
      projection <- getCRS(x = template)
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

    message(paste0("please click the ", features, " vertices."))

    visualise(raster = template)
    theClicks <- gt_locate(samples = features, panel = tempName, silent = TRUE, ...)
    window <- c(0, dims[2], 0, dims[1])
    tempAnchor <- tibble(fid = 1:features,
                         vid = 1,
                         x = theClicks$x,
                         y = theClicks$y)

  } else if(anchorIsGeom){
    if(!windowExists){
      window <- c(min(anchor@window$x), max(anchor@window$x), min(anchor@window$y), max(anchor@window$y))
    }
    tempAnchor <- anchor@vert
  } else if(anchorIsDF){
    if(!windowExists){
      window <- c(min(anchor$x), max(anchor$x), min(anchor$y), max(anchor$y))
    }
    tempAnchor <- anchor
  }

  temp <- deldir(as.data.frame(tempAnchor), rw = window, suppressMsge = TRUE)
  tempTiles <- tile.list(temp)

  for(i in seq_along(tempTiles)){

    temp <- tibble(fid = i, vid = 1:length(tempTiles[[i]]$x), x = tempTiles[[i]]$x, y = tempTiles[[i]]$y)
    if(i == 1){
      nodes <- temp
    } else{
      nodes <- bind_rows(nodes, temp)
    }
  }

  out <- new(Class = "geom",
             type = "polygon",
             vert = nodes,
             attr = tibble(fid = unique(nodes$fid), gid = unique(nodes$fid)),
             window = tibble(x = rep(c(window[1], window[2]), each = 2), y = c(window[3], window[4], window[4], window[3])),
             scale = "absolute",
             crs = as.character(projection),
             history = list(paste0("geometry was created as voronoi 'polygon'")))

  invisible(out)
}