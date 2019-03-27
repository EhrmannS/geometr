#' Create a polygon geometry
#'
#' Create any (regular) polygon geometry (of class \code{\link{geom}}) either by
#' specifying its parameters or by sketching it.
#' @param anchor [\code{geom} | \code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. In case of \code{data.frame}, it must include column
#'   names \code{x}, \code{y} and optinal variables such as \code{id}; see
#'   Examples.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the geometry.
#' @param features [\code{integerish(1)}]\cr number of geometries to create.
#' @param vertices [\code{integerish(.)}]\cr number of vertices per geometry;
#'   will be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param regular [\code{logical(1)}]\cr should the polygon be regular, i.e.
#'   point symmetric (\code{TRUE}) or should the vertices be selected according
#'   to \code{anchor} or \code{vertices} (\code{FALSE}, default)?
#' @param fixed [\code{logical(1)}]\cr should the polygon be aligned vertically
#'   (\code{TRUE}, default), or sohuld it be aligned according to the second
#'   click (\code{FALSE}); only relevant if \code{regular = TRUE}.
#' @param ... [various]\cr graphical parameters to \code{\link{locate}}, in case
#'   a polygon is sketched; see \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @details The arguments \code{anchor} and \code{template} indicate how the
#'   geometry is created: \itemize{ \item \code{anchor}: if set, the geometry is
#'   created parametrically, the input provided is used to parameterise the
#'   geometry \itemize{ \item if \code{regular = FALSE} the resulting geometry
#'   is the boundary per feature, \item if \code{regular = TRUE}, only the
#'   first two vertices are considered, as center and indicating the (outer)
#'   radius.} \item \code{template}: if set, the geometry is created
#'   interactively, by clicking into the plot.}
#' @family shapes
#' @examples
#' library(magrittr)
#'
#' # create a polygon programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#'
#' # if no window is set, the bounding box (i.e. min/max values) will be set as window
#' (aGeom <- gs_polygon(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' visualise(geom = gs_polygon(anchor = coords,
#'                             vertices = 6, regular = TRUE))
#' gs_triangle(anchor = coords, window = window) %>%
#'   visualise(geom = ., linecol = "green", new = FALSE)
#'
#' # when a geom is used in 'anchor', its properties (e.g. 'window') are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' gs_polygon(anchor = aGeom) %>%
#'   visualise(geom = ., fillcol = "deeppink")
#' gs_rectangle(anchor = aGeom) %>%
#'   visualise(geom = ., new = FALSE)
#'
#' \dontrun{
#'
#' input <- gtRasters$continuous
#'
#' # create a square interactively
#' squareGeom <- gs_square(template = input)
#' visualise(geom = squareGeom, linecol = "orange", new = FALSE)
#'
#' # ... or an approximate circle (actually a hectogon)
#' circleGeom <- gs_polygon(template = input, vertices = 100, regular = TRUE) %>%
#'   visualise(geom = ., linecol = "deeppink", new = FALSE)
#'
#' # create two arbitrary polygons interactively
#' polyGeom <- gs_polygon(template = input, features = 2, vertices = c(4, 6)) %>%
#'   visualise(geom = ., linecol = "green", lwd = 1, lty = "dashed", new = FALSE)
#' }
#' @importFrom stats dist
#' @importFrom checkmate testDataFrame assertNames testClass assertDataFrame
#'   testTRUE testNull testClass assertIntegerish assertLogical assert
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang !!
#' @export

gs_polygon <- function(anchor = NULL, window = NULL, template = NULL, features = 1,
                       vertices = NULL, regular = FALSE, fixed = TRUE, ...){

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
  assertLogical(regular)
  assertLogical(fixed)
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

  # build a regular geometry
  nodes <- fids <- NULL
  for(i in 1:features){

    # if anchor does not exists, make it
    if(!anchorIsDF & !anchorIsGeom){

      if(regular){
        message("please click the polygons' center and the first vertex.")
        clicks <- 2
      } else{
        message(paste0("please click the ", vertices[i], " vertices."))
        clicks <- vertices[i]
      }
      visualise(raster = template)
      theClicks <- locate(samples = clicks, panel = tempName, silent = TRUE, ...)
      window <- tibble(x = c(0, dims[2]),
                       y = c(0, dims[1]))
      tempAnchor <- tibble(fid = i,
                           vid = 1:clicks,
                           x = theClicks$x,
                           y = theClicks$y)

      if(fixed){
        openingAngle <- 0
      } else{
        # get the angle between the first and second click
        openingAngle <- atan((theClicks$x[1] - theClicks$x[2]) / (theClicks$y[1] - theClicks$y[2])) * 180 / pi
      }

    } else if(anchorIsGeom){
      if(!windowExists){
        window <- anchor@window
      }
      tempAnchor <- anchor@vert[anchor@vert$fid == i,]
      openingAngle <- 0
    } else if(anchorIsDF){
      if(!windowExists){
        window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                         y = c(min(anchor$y), max(anchor$y)))
      }
      tempAnchor <- anchor[anchor$fid == i, ]
      openingAngle <- 0
    }

    if(regular){

      # trigonometry
      angle <- 360/vertices[i]
      angles <- seq(from = 90, to = 360-angle+90, by = angle) - openingAngle

      radius <- dist(tempAnchor[c(1:2),])
      cx <- tempAnchor$x[1] + radius*cos(rad(angles))
      cy <- tempAnchor$y[1] + radius*sin(rad(angles))
      theNodes <- tibble(fid = i, vid = 1:vertices, x = cx, y = cy)
      if(any(theNodes$x < min(window$x)) | any(theNodes$x > max(window$x)) | any(theNodes$y < min(window$y)) | any(theNodes$y > max(window$y))){
        window <- tibble(x = c(min(theNodes$x), max(theNodes$x)), y = c(min(theNodes$y), max(theNodes$y)))
      }

      nodes <- bind_rows(nodes, theNodes)
      fids <- c(fids, length(unique(theNodes$fid)))

    } else{

      theNodes <- tempAnchor[c("fid", "vid", "x", "y")]

      nodes <- bind_rows(nodes, theNodes)
      fids <- c(fids, length(unique(theNodes$vid)))
    }

  }

  out <- new(Class = "geom",
             type = "polygon",
             vert = nodes,
             attr = tibble(fid = unique(nodes$fid), gid = unique(nodes$fid)),
             window = tibble(x = rep(c(min(window$x), max(window$x)), each = 2), y = c(min(window$y), max(window$y), max(window$y), min(window$y))),
             scale = "absolute",
             crs = as.character(projection),
             history = list(paste0("geometry was created as 'polygon'")))

  invisible(out)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 3} and
#'   \code{regular = TRUE}.
#' @export

gs_triangle <- function(anchor = NULL, window = NULL, template = NULL,
                        features = 1, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)

  theGeom <- gs_polygon(anchor = anchor,
                        window = window,
                        template = template,
                        features = features,
                        vertices = 3,
                        regular = TRUE,
                        ...)

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 4} and
#'   \code{regular = TRUE}.
#' @export

gs_square <- function(anchor = NULL, window = NULL, template = NULL,
                      features = 1, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)

  theGeom <- gs_polygon(anchor = anchor,
                        window = window,
                        template = template,
                        features = features,
                        vertices = 4,
                        regular = TRUE,
                        ...)

  # centroid <- colMeans(theGeom@vert[c("x", "y")])
  # rotGeom <- gt_rotate(geom = theGeom,
  #                      angle = 45,
  #                      about = centroid)

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 2},
#'   \code{regular = FALSE} and the two complementing corners are derived from
#'   the two given opposing corners.
#' @export

gs_rectangle <- function(anchor = NULL, window = NULL, template = NULL,
                         features = 1, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)

  theGeom <- gs_polygon(anchor = anchor,
                        window = window,
                        template = template,
                        features = features,
                        vertices = 2,
                        ...)

  outTable <- NULL
  for(i in seq_along(theGeom@attr$fid)){
    geomSubset <- getSubset(theGeom, fid == !!i, slot = "table")
    temp <- getExtent(geomSubset)
    temp <- tibble(fid = i,
                   vid = 1:4,
                   x = rep(temp$x, each = 2),
                   y = c(temp$y, rev(temp$y)))
    outTable <- bind_rows(outTable, temp)
  }

  theGeom@vert <- outTable

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 6} and
#'   \code{regular = TRUE}.
#' @export

gs_hexagon <- function(anchor = NULL, window = NULL, template = NULL,
                       features = 1, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)

  theGeom <- gs_polygon(anchor = anchor,
                        window = window,
                        template = template,
                        features = features,
                        vertices = 6,
                        regular = TRUE,
                        ...)

  invisible(theGeom)
}
