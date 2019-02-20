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
#' @param show [\code{logical(1)}]\cr should the geometry be plotted
#'   (\code{TRUE}) or should it not be plotted (\code{FALSE}, default)? In case
#'   \code{template} is set, it is automatically \code{TRUE}.
#' @param ... [various]\cr graphical parameter, in case \code{show = TRUE}; see
#'   \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @details The arguments \code{anchor} and \code{template} have \code{NULL}
#'   value, because leaving them unset is meant to result in a specific
#'   behaviour: \itemize{ \item \code{anchor}: if unset, this argument triggers
#'   that the geometry is created interactively (hence, \code{template} must be
#'   set); if set, the input provided is used to parameterise the geometry:
#'   \itemize{ \item if \code{regular = FALSE} the resulting geometry is the
#'   line connecting the vertices, \item if \code{regular = TRUE}, only the
#'   first two coordinates are considered as center and indicating the (outer)
#'   radius.} \item \code{template}: if unset, this argument triggers that the
#'   geometry is created programmatically (hence, \code{anchor} must be set).}
#' @family shapes
#' @examples
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
#' aTriangle <- gs_polygon(anchor = coords, window = window, vertices = 3, regular = TRUE,
#'                          fill = "darkorange", show = FALSE)
#' (gs_hexagon(anchor = coords, col = "green", show = TRUE))
#'
#' # if a geom is used in 'anchor', its properties (e.g. 'window') are passed on
#' grid::grid.newpage()
#' aGeom <- gs_polygon(anchor = coords, window = window, fill = "deeppink", show = TRUE)
#' anExtent <- geomRectangle(anchor = aGeom, show = TRUE)
#'
#' # geoms with more than one element are treated element-wise
#' aGeom <- gGroup(geom = aGeom, index = c(1, 2, 1, 2))
#' visualise(geom = aGeom)
#' itsExtent <- geomRectangle(anchor = aGeom, show = TRUE)
#'
#' \dontrun{
#'
#' input <- rtRasters$continuous
#'
#' # create a square interactively
#' squareGeom <- geomSquare(template = input, show = TRUE, col = "orange")
#'
#' # ... or an approximate circle (actually a hectogon)
#' circleGeom <- gs_polygon(template = input, vertices = 100, regular = TRUE,
#'                           show = TRUE, col = "deeppink")
#'
#' # create two arbitrary polygons interactively
#' polyGeom <- gs_polygon(template = input, features = 2, vertices = c(4, 6),
#'                         col = "green", lwd = 1, lty = "dashed", show = TRUE)
#' }
#' @importFrom stats dist
#' @importFrom checkmate testDataFrame assertNames testList testTRUE testNull
#'   testClass assertIntegerish assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols bind_rows
#' @export

gs_polygon <- function(anchor = NULL, window = NULL, template = NULL, features = 1,
                       vertices = NULL, regular = FALSE, show = FALSE, ...){

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
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(regular)
  assertLogical(show)
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

  # build a regular geometry
  nodes <- fids <- NULL
  out <- NULL
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
      theClicks <- locate(raster = template, samples = clicks, panel = tempName, silent = TRUE, show = FALSE, ...)
      window <- tibble(x = c(0, dims[2]),
                       y = c(0, dims[1]))
      tempAnchor <- tibble(fid = i,
                           vid = 1:clicks,
                           x = theClicks$x,
                           y = theClicks$y)

    } else if(anchorIsGeom){
      if(!windowExists){
        window <- anchor@window
      }
      tempAnchor <- anchor@coords[anchor@coords$fid == i,]
    } else if(anchorIsDF){
      if(!windowExists){
        window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                         y = c(min(anchor$y), max(anchor$y)))
      }
      tempAnchor <- anchor[anchor$fid == i, ]
    }

    if(regular){

      # trigonometry
      angle <- 360/vertices[i]
      angles <- seq(from = 90, to = 360-angle+90, by = angle)

      radius <- dist(tempAnchor[c(1:2),])
      cx <- tempAnchor$x[1] + radius*cos(rad(angles))
      cy <- tempAnchor$y[1] + radius*sin(rad(angles))
      theNodes <- tibble(fid = i, vid = 1:vertices, x = cx, y = cy)
      if(any(theNodes$x < min(window$x)) | any(theNodes$x > max(window$x)) | any(theNodes$y < min(window$y)) | any(theNodes$y > max(window$y))){
        window <- tibble(x = c(min(theNodes$x), max(theNodes$x)), y = c(min(theNodes$y), max(theNodes$y)))
      }

      temp <- new(Class = "geom",
                  type = "polygon",
                  coords = theNodes,
                  attr = tibble(fid = unique(theNodes$fid), n = length(unique(theNodes$vid))),
                  window = tibble(x = rep(c(min(window$x), max(window$x)), each = 2), y = c(min(window$y), max(window$y), max(window$y), min(window$y))),
                  scale = "absolute",
                  crs = as.character(projection),
                  history = list(paste0()))

      if(show){
        if(!any(names(listArgs()) == "new")){
          visualise(geom = temp, new = FALSE, ...)
        } else{
          visualise(geom = temp, ...)
        }
      }
      nodes <- bind_rows(nodes, theNodes)
      fids <- c(fids, length(unique(theNodes$fid)))

    } else{

      theNodes <- tempAnchor[c("fid", "vid", "x", "y")]

      temp <- new(Class = "geom",
                  type = "polygon",
                  coords = theNodes,
                  attr = tibble(fid = unique(theNodes$fid), n = length(unique(theNodes$vid))),
                  window = tibble(x = rep(c(min(window$x), max(window$x)), each = 2), y = c(min(window$y), max(window$y), max(window$y), min(window$y))),
                  scale = "absolute",
                  crs = as.character(projection),
                  history = list(paste0()))

      if(show){
        if(!any(names(listArgs()) == "new")){
          visualise(geom = temp, new = TRUE, ...)
        } else{
          visualise(geom = temp, ...)
        }
      }
      nodes <- bind_rows(nodes, theNodes)
      fids <- c(fids, length(unique(theNodes$vid)))
    }

  }

  out <- new(Class = "geom",
             type = "polygon",
             coords = nodes,
             attr = tibble(fid = unique(nodes$fid), n = fids),
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
                         features = 1, show = FALSE, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(show)

  theGeom <- gs_polygon(anchor = anchor,
                         window = window,
                         template = template,
                         features = features,
                         vertices = 3,
                         regular = TRUE,
                         show = FALSE)

  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = theGeom, new = TRUE, ...)
    } else{
      visualise(geom = theGeom, ...)
    }
  }

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 4},
#'   \code{regular = TRUE} and a rotation by 45° about the centroid has been
#'   applied.
#' @export

gs_square <- function(anchor = NULL, window = NULL, template = NULL,
                       features = 1, show = FALSE, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(show)

  theGeom <- gs_polygon(anchor = anchor,
                         window = window,
                         template = template,
                         features = features,
                         vertices = 4,
                         regular = TRUE,
                         show = FALSE)

  centroid <- colMeans(theGeom@coords[c("x", "y")])
  rotGeom <- gt_rotate(geom = theGeom,
                     angle = 45,
                     about = centroid)

  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = rotGeom, new = TRUE, ...)
    } else{
      visualise(geom = rotGeom, ...)
    }
  }
  invisible(rotGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 2},
#'   \code{regular = FALSE} and the two complementing corners are derived from
#'   the two given opposing corners.
#' @export

gs_rectangle <- function(anchor = NULL, window = NULL, template = NULL,
                          features = 1, show = FALSE, ...){

  anchorIsDF <- testDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
  anchorIsGeom <- testClass(anchor, classes = "geom")
  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(show)

  if(anchorIsGeom){
    anchors <- anchor@coords
    window <- anchor@window
  } else{
    anchors <- anchor
  }

  newCoords <- NULL
  if(!any(colnames(anchors) == "fid")){
    anchors <- cbind(anchors, fid = 1)
  }

  for(i in unique(anchors$fid)){
    tempAnchor <- anchors[anchors$fid == i,]
    # get minimum and maximum value of x and y
    tempAnchor <- tibble(fid = i,
                         x = c(min(tempAnchor$x), max(tempAnchor$x)),
                         y = c(min(tempAnchor$y), max(tempAnchor$y)))
    # change positions of vertices, so that they follow a square
    tempAnchor <- tibble(fid = i,
                         x = rep(tempAnchor$x, each = 2),
                         y = c(tempAnchor$y, rev(tempAnchor$y)))
    newCoords <- rbind(newCoords, tempAnchor)
  }

  theGeom <- gs_polygon(anchor = newCoords,
                         window = window,
                         template = template,
                         features = features,
                         vertices = 4,
                         regular = FALSE,
                         show = FALSE)

  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = theGeom, new = TRUE, ...)
    } else{
      visualise(geom = theGeom, ...)
    }
  }
  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 6} and
#'   \code{regular = TRUE}.
#' @export

gs_hexagon <- function(anchor = NULL, window = NULL, template = NULL,
                        features = 1, show = FALSE, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(show)

  theGeom <- gs_polygon(anchor = anchor,
                         window = window,
                         template = template,
                         features = features,
                         vertices = 6,
                         regular = TRUE,
                         show = FALSE)

  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = theGeom, new = TRUE, ...)
    } else{
      visualise(geom = theGeom, ...)
    }
  }
  invisible(theGeom)
}
