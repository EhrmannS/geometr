#' Create a regular tiling \code{geom}
#'
#' Create a regular tiling polygon geometry (of class \code{geom}) for the
#' extent of an anchor value.
#' @param anchor [\code{geom(1)}|\code{data.frame(1)}]\cr Object to derive the
#'   tiling \code{geom} from. It must include column names \code{x}, \code{y}
#'   and optionally a custom \code{fid}.
#' @param width [\code{numeric(1)}]\cr the width (which does not correspond to
#'   the height in case of \code{pattern = "hexagonal"}) of a tile.
#' @param pattern [\code{character(1)}]\cr pattern of the tiling. Possible
#'   options are \code{"squared"} (default) or \code{"hexagonal"}.
#' @param centroids [\code{logical(1)}]\cr should the centroids of the tiling be
#'   returned (\code{TRUE}) or should the tiling be returned (\code{FALSE},
#'   default)?
#' @param ... [various]\cr additional arguments; see Details.
#' @details When deriving a regular tiling for a prescribed window, there is
#'   only a limited set of legal combinations of cells in x and y dimension. For
#'   instance, a window of 100 by 100 can't comprise 10 by 5 squares of
#'   side-length 10, because then the y-dimension wouldn't be fully covered. The
#'   same is true for hexagonal and triangular tilings. As all tilings are
#'   regular, the measurement of one dimension is sufficient to specify the
#'   dimensions of tiles, which is \code{width}.
#'
#'   Possible additional arguments are: \itemize{ \item verbose = TRUE/FALSE
#'   \item graphical parameters to \code{\link{gt_locate}}, in case points are
#'   sketched; see \code{\link{gpar}}}
#' @return An invisible \code{geom}.
#' @family tilings
#' @examples
#' # create a squared tiling
#' library(magrittr)
#' aWindow <- data.frame(x = c(-180, 180),
#'                       y = c(-60, 80))
#' gs_tiles(anchor = aWindow, width = 10) %>%
#'   visualise(`10° world tiles` = .)
#'
#' # create a hexagonal tiling on top of a geom
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#' visualise(`honeycomb background` = aGeom)
#' gs_tiles(anchor = aGeom, width = 8, pattern = "hexagonal") %>%
#'   visualise(., linecol = "deeppink", new = FALSE)
#' @importFrom checkmate testDataFrame assertNames testClass testIntegerish
#'   assertDataFrame assertNames assertCharacter assertSubset assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows group_by summarise left_join select mutate
#' @export

gs_tiles <- function(anchor = NULL, width = NULL, pattern = "squared",
                     centroids = FALSE, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor, ...)
  assertIntegerish(x = width, len = 1)
  assertChoice(x = pattern, choices = c("squared", "hexagonal", "triangular"))
  assertLogical(x = centroids)

  if(!is.null(anchor)){
    if(anchor$type == "geom"){
      anchor <- anchor$obj
    } else if(anchor$type == "df"){
      anchor <- gs_rectangle(anchor = anchor$obj)
    }
  }

  # set pattern specific properties
  if(pattern == "squared"){

    offset <- width*0.5*-1

    # determine centroids
    xCentroids <- seq(min(anchor@point$x) - offset, max(anchor@point$x) + width + offset, by = width)
    yCentroids <- seq(min(anchor@point$y) - offset, max(anchor@point$y) + width + offset, by = width)
    cntrds <- tibble(fid = seq(1:(length(xCentroids)*length(yCentroids))),
                     x = rep(xCentroids, times = length(yCentroids)),
                     y = rep(yCentroids, each = length(xCentroids)))
    targetCentroids <- gs_point(anchor = cntrds, window = anchor@window)

    offset <- 45
    vertices <- 4

    radius <- width/2
    radius <- sqrt(radius**2 + radius**2)

  } else if(pattern == "hexagonal"){
    # https://www.redblobgames.com/grids/hexagons/

    offset <- 0

    inRadius <- width/2
    circumRadius <- 2/sqrt(3) * inRadius
    radius <- circumRadius

    xOffset <- inRadius*2*offset*-1
    yOffset <- 2*circumRadius*offset*-1

    # determine centroids
    xC1 <- seq(min(anchor@point$x) - 2*inRadius - xOffset, max(anchor@point$x) + 2*inRadius + xOffset, by = inRadius*2)
    xC2 <- seq(min(anchor@point$x) - inRadius - xOffset, max(anchor@point$x) + 3*inRadius + xOffset, by = inRadius*2)
    yC1 <- seq(min(anchor@point$y) - yOffset, max(anchor@point$y) + circumRadius + yOffset, by = 3*circumRadius)
    yC2 <- seq(min(anchor@point$y) + 3/2*circumRadius - yOffset, max(anchor@point$y) + 3/2*circumRadius + yOffset, by = 3*circumRadius)

    cntrds <- tibble(fid = seq(1:(length(yC1)*length(xC1) + length(yC2)*length(xC2))),
                     x = c(rep(xC1, times = length(yC1)), rep(xC2, times = length(yC2))),
                     y = c(rep(yC1, each = length(xC1)), rep(yC2, each = length(xC2))))
    targetCentroids <- gs_point(anchor = cntrds, window = anchor@window)

    offset <- 30
    vertices <- 6

  } else if(pattern == "triangular"){
    stop("triangular tiling is not yet supported.")
  }

  rotation <- offset
  angle <- 360/vertices
  angles <- seq(from = 0, to = 360-angle, by = angle) - rotation

  relX <- radius*cos(.rad(angles))
  relY <- radius*sin(.rad(angles))

  if(!centroids){
    nodes <- NULL
    for(i in seq_along(targetCentroids@point$fid)){
      cx <- targetCentroids@point$x[i] + relX
      cx <- c(cx, cx[1])
      cy <- targetCentroids@point$y[i] + relY
      cy <- c(cy, cy[1])
      theNodes <- tibble(x = cx,
                         y = cy,
                         fid = i)
      nodes <- bind_rows(nodes, theNodes)
    }
    theType <- "polygon"
  } else{
    nodes <- tibble(x = cntrds$x,
                    y = cntrds$y,
                    fid = cntrds$fid)
    theType <- "point"
  }

  nodes$incl <- as.logical(pointInGeomC(vert = as.matrix(nodes[c("x", "y")]),
                                        geom = as.matrix(anchor@point[c("x", "y")]),
                                        invert = FALSE))

  retain <- NULL
  for(i in unique(nodes$fid)){
    temp <- nodes[nodes$fid == i,]
    if(any(temp$incl)){
      if(is.null(retain$fid)){
        temp$fid <- 1
      } else {
        temp$fid <- max(retain$fid) + 1
      }
      retain <- rbind(retain, temp)
    }
  }
  theFeatures <- tibble(fid = unique(retain$fid), gid = unique(retain$fid))
  theGroups <- tibble(gid = unique(retain$fid))

  theTiles <- new(Class = "geom",
                  type = theType,
                  point = tibble(fid = retain$fid, x = retain$x, y = retain$y),
                  feature = list(geometry = theFeatures),
                  group = list(geometry = theGroups),
                  window = anchor@window,
                  scale = "absolute",
                  crs = NA_character_,
                  history = list(paste0("tiled geometry of type '", theType, "' was created.")))

  invisible(theTiles)
}