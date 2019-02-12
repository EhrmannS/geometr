#' Create a regular tiling geometry
#'
#' @param window [\code{data.frame(1)}]\cr the origin (lower left corner) and
#'   the maximum value (upper right corner) of the tiling.
#' @param cells [\code{integerish(2)}]\cr number of tiles in \code{x} and
#'   \code{y} dimension.
#' @param crs [\code{character(1)}]\cr corrdinate reference system of the
#'   object in proj4 notation.
#' @param tiling [\code{character(1)}]\cr pattern of the tiling. Possible
#'   options are \code{"rectangular"} (default), \code{"hexagonal"},
#'   \code{"triangular"}.
#' @param centroids [\code{logical(1)}]\cr should the centroids of the tiling be
#'   returned (\code{TRUE}) or should the tiling be returned (\code{FALSE},
#'   default)?
#' @return An invisible \code{geom}.
#' @examples
#' # create grid for GFC data
#' gfcWindow <- data.frame(x = c(-180, 180),
#'                         y = c(-60, 80))
#' tiles_gfc <- geomTiles(window = gfcWindow, cells = c(36, 14), crs = projs$longlat)
#'
#' # create grid for MODIS data
#' modWindow <- data.frame(x = c(-20015109.354, 20015109.354),
#'                         y = c(-10007554.677, 10007554.677))
#' tiles_modis <- geomTiles(window = modWindow, cells = c(36, 18), crs = projs$sinu)
#'
#' # create grid for the sentinel data
#' #sntWindow <- data.frame(x = c(),
#' #                        y = c())
#' #tiles_sentinel <- geomTiles(window = sntWindow, cells = c(), crs = projs$utm)
#' @importFrom checkmate testDataFrame assertNames testClass testIntegerish
#'   assertDataFrame assertNames assertCharacter assertSubset assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export

geomTiles <- function(window = NULL, cells = NULL, crs = NULL,
                      tiling = "rectangular", centroids = FALSE){

  # check arguments
  assertIntegerish(cells, len = 2, any.missing = FALSE)
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  colnames(window) <- tolower(colnames(window))
  assertNames(names(window), must.include = c("x", "y"))
  assertCharacter(crs, fixed = "+proj", null.ok = TRUE)
  assertCharacter(tiling, ignore.case = TRUE, any.missing = FALSE, len = 1)
  assertSubset(tiling, choices = c("rectangular", "hexagonal", "triangular"))
  assertLogical(centroids)

  if(!is.null(crs)){
    projection <- crs
  } else{
    projection <- NA
  }

  if(tiling == "rectangular"){

    xDist <- (abs(min(window$x)) + abs(max(window$x)))/cells[1]
    yDist <- (abs(min(window$y)) + abs(max(window$y)))/cells[2]
    if(round(xDist, 3) != round(yDist, 3)){
      stop("a tiling of ", cells[1], " by ", cells[2], " cells would yield irregular cells with the given 'window'.\n Please adapt either 'cells' or 'window'.")
    }

    # determine centroids
    xCentroids <- seq(min(window$x) + xDist/2, max(window$x), xDist)
    yCentroids <- seq(min(window$y) + yDist/2, max(window$y), yDist)
    cntrds <- tibble(fid = seq(1:(cells[1]*cells[2])),
                     x = rep(xCentroids, times = length(yCentroids)),
                     y = rep(yCentroids, each = length(xCentroids)))

    angle <- 360/4
    angles <- seq(from = 45, to = 360-angle+45, by = angle)
    radius <- sqrt(xDist**2 + yDist**2)/2

  } else if(tiling == "hexagonal"){

    height <- (abs(min(window$y)) + abs(max(window$y)))/cells[2]
    width <- 2 * (height / sqrt(3))

    # determine centroids
    xC1 <- seq(min(window$x), max(window$x) + width/2, by = 3/2*width)
    xC2 <- seq(min(window$x) + 3/4*width, max(window$x), by = 3/2*width)
    if(length(xC1) > cells[1]/2+1){
      xC1 <- xC1[c(1:(cells[1]/2+1))]
      xC2 <- xC2[c(1:(cells[1]/2))]
    }
    yC1 <- seq(min(window$y), max(window$y), by = height)
    yC2 <- seq(min(window$y) + height/2, max(window$y), by = height)

    cntrds <- tibble(fid = seq(1:(length(yC1)*length(xC1) + length(yC2)*length(xC2))),
                     x = c(rep(xC1, times = length(yC1)), rep(xC2, times = length(yC2))),
                     y = c(rep(yC1, each = length(xC1)), rep(yC2, each = length(xC2))))

    angle <- 360/6
    angles <- seq(from = 0, to = 360-angle, by = angle)
    radius <- width/2

  } else if(tiling == "triangular"){
  }

  if(!centroids){
    nodes <- NULL
    for(i in seq_along(cntrds$fid)){
      cx <- round(cntrds$x[i] + radius*cos(rad(angles)))
      cy <- round(cntrds$y[i] + radius*sin(rad(angles)))
      theNodes <- tibble(fid = i, vid = 1:length(angles), x = cx, y = cy)
      nodes <- bind_rows(nodes, theNodes)
    }
    theType <- "polygon"
  } else{
    nodes <- cntrds
    theType <- "point"
  }

  window <- tibble(x = rep(window$x, each = 2),
                   y = c(window$y, rev(window$y)))

  theTiles <- new(Class = "geom",
                  type = theType,
                  coords = nodes,
                  attr = tibble(fid = unique(nodes$fid), n = 1),
                  window = window,
                  scale = "absolute",
                  crs = as.character(projection),
                  history = list(paste0("tiled geometry of type '", theType, "' was created.")))

  invisible(theTiles)
}