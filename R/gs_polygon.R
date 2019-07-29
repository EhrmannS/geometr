#' Create a polygon geometry
#'
#' Create any (regular) polygon geometry (of class \code{\link{geom}}) either by
#' specifying anchor values or by sketching it.
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param features [\code{integerish(1)}]\cr number of geometries to create.
#' @param vertices [\code{integerish(.)}]\cr number of vertices per geometry;
#'   will be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param sketch [\code{RasterLayer(1)}|\code{matrix(1)}]\cr Gridded object that
#'   serves as template to sketch the geometry.
#' @param regular [\code{logical(1)}]\cr should the polygon be regular, i.e.
#'   point symmetric (\code{TRUE}) or should the vertices be selected according
#'   to \code{anchor} or \code{vertices} (\code{FALSE}, default)?
#' @param fixed [\code{logical(1)}]\cr should the polygon be aligned vertically
#'   (\code{TRUE}, default), or should it be aligned according to the second
#'   click (\code{FALSE}); only relevant if \code{regular = TRUE}.
#' @param ... [various]\cr additional arguments; see Details.
#' @details The arguments \code{anchor} and \code{sketch} indicate how the
#'   geometry is created: \itemize{ \item \code{anchor}: if set, the geometry is
#'   created parametrically, the input provided is used to parameterise the
#'   geometry \itemize{ \item if \code{regular = FALSE} the resulting geometry
#'   is the boundary per feature, \item if \code{regular = TRUE}, only the first
#'   two vertices are considered, as center and indicating the (outer) radius.}
#'   \item \code{sketch}: if set, the geometry is created interactively, by
#'   clicking into the plot.}
#'
#'   Possible additional arguments are: \itemize{ \item verbose = TRUE/FALSE
#'   \item graphical parameters to \code{\link{gt_locate}}, in case points are
#'   sketched; see \code{\link{gpar}}}.
#' @return An invisible \code{geom}.
#' @family geometry shapes
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
#' @importFrom stats dist
#' @importFrom checkmate testDataFrame assertNames testClass assertDataFrame
#'   testTRUE testNull testClass assertIntegerish assertLogical assert
#' @importFrom tibble tibble add_row
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang !!
#' @export

gs_polygon <- function(anchor = NULL, window = NULL, features = 1, vertices = NULL,
                       sketch = NULL, regular = FALSE, fixed = TRUE, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor, ...)
  theWindow <- .testWindow(x = window, ...)
  assertIntegerish(x = features, len = 1, lower = 1)
  assertIntegerish(x = vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = regular)
  assertLogical(x = fixed)

  if(is.null(anchor) & is.null(sketch)){
    stop("please provide anchor values.")
  }
  if(!is.null(anchor)){
    if(anchor$type == "geom"){
      if(anchor$obj@type == "point"){
        anchor$obj@vert$fid <- rep(1, length(anchor$obj@vert$fid))
        anchor$obj@feat <- tibble(fid = 1, gid = 1)
        anchor$obj@group <- tibble(gid = 1)
        features <- 1
      } else {
        features <- length(unique(anchor$obj@feat$fid))
      }
    } else if(anchor$type == "df"){
      if("fid" %in% names(anchor$obj)){
        features <- length(unique(anchor$obj$fid))
      }
    }
  }

  # if anchor does not exists, make it
  if(!is.null(sketch)){

    template <- .testTemplate(x = sketch, ...)
    theGeom <- gt_sketch(template = template$obj, shape = "polygon", features = features, vertices = vertices, regular = regular, fixed = fixed)

  } else{

    theVertices <- theFeatures <- theGroups <- NULL
    for(i in 1:features){

      if(anchor$type == "geom"){
        # }

        # } else if(anchor$type == "geom"){

        if(is.null(theWindow)){
          theWindow <- anchor$obj@window
        }
        tempAnchor <- anchor$obj@vert[anchor$obj@vert$fid == i,]
        openingAngle <- atan((tempAnchor$x[1] - tempAnchor$x[2]) / (tempAnchor$y[1] - tempAnchor$y[2])) * 180 / pi
        tempFeatures <- anchor$obj@feat[anchor$obj@feat$fid == i,]
        tempGroups <- anchor$obj@group[anchor$obj@group$gid == i,]
        projection <- getCRS(x = anchor$obj)

      } else if(anchor$type == "df"){

        if(is.null(theWindow)){
          theWindow = tibble(x = c(min(anchor$obj$x), max(anchor$obj$x), max(anchor$obj$x), min(anchor$obj$x), min(anchor$obj$x)),
                             y = c(min(anchor$obj$y), min(anchor$obj$y), max(anchor$obj$y), max(anchor$obj$y), min(anchor$obj$y)))
        }
        if("fid" %in% names(anchor$obj)){
          tempAnchor <- anchor$obj[anchor$obj$fid == i, ]
        } else {
          tempAnchor <- anchor$obj
          tempAnchor <- bind_cols(tempAnchor, fid = rep(i, length.out = length(anchor$obj$x)))
        }
        openingAngle <- atan((tempAnchor$x[1] - tempAnchor$x[2]) / (tempAnchor$y[1] - tempAnchor$y[2])) * 180 / pi
        tempFeatures <- tibble(fid = i, gid = i)
        tempGroups <- tibble(gid = i)
        projection <- NA

      }

      # build a regular geometry
      if(regular){
        # trigonometry
        angle <- 360/vertices[i]
        angles <- seq(from = 90, to = 360-angle+90, by = angle) - openingAngle

        radius <- dist(tempAnchor[c(1:2),])
        cx <- tempAnchor$x[1] + radius*cos(.rad(angles))
        cy <- tempAnchor$y[1] + radius*sin(.rad(angles))
        theNodes <- tibble(x = cx, y = cy, fid = i)
        theWindow <- .updateWindow(geom = theNodes, window = theWindow)
      } else{
        theNodes <- tempAnchor[c("x", "y", "fid")]
      }

      # create vertices that would close rings and thus make valid polygons, check
      # whether the first vertex has a duplicate...
      dupBwd <- duplicated(theNodes, fromLast = TRUE)
      if(!dupBwd[1]){
        # ... if not, test whether there is any duplicate, hence any closed ring
        if(any(dupBwd)){
          theNodes <- add_row(theNodes, x = theNodes$x[1], y = theNodes$y[1], fid = theNodes$fid[1], .before = which(dupBwd))
        } else {
          theNodes <- add_row(theNodes, x = theNodes$x[1], y = theNodes$y[1], fid = theNodes$fid[1])
        }
      }

      # check whether the last vertex has a duplicate...
      dupFwd <- duplicated(theNodes)
      if(!dupFwd[dim(theNodes)[1]]){
        # ... if not, test whether there is any duplicate, hence any closed ring
        if(any(dupFwd)){
          theNodes <- add_row(theNodes, x = theNodes$x[dim(theNodes)[1]], y = theNodes$y[dim(theNodes)[1]], fid = theNodes$fid[dim(theNodes)[1]], .after = which(dupFwd)+1)
        } else {
          theNodes <- add_row(theNodes, x = theNodes$x[1], y = theNodes$y[1], fid = theNodes$fid[1])
        }
      }

      theVertices <- bind_rows(theVertices, theNodes)
      theFeatures <- bind_rows(theFeatures, tempFeatures)
      theGroups <- bind_rows(theGroups, tempGroups)
    }

    theGeom <- new(Class = "geom",
                   type = "polygon",
                   vert = theVertices,
                   feat = theFeatures,
                   group = theGroups,
                   window = theWindow,
                   scale = "absolute",
                   crs = as.character(projection),
                   history = list(paste0("geometry was created as 'polygon'.")))
  }

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 3} and
#'   \code{regular = TRUE}.
#' @export

gs_triangle <- function(anchor = NULL, window = NULL, sketch = NULL,
                        features = 1, fixed = FALSE, ...){

  if(is.null(anchor) & is.null(sketch)){
    stop("please provide anchor values.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)

  theGeom <- gs_polygon(anchor = anchor,
                        window = window,
                        sketch = sketch,
                        features = features,
                        vertices = 3,
                        regular = TRUE,
                        fixed = fixed,
                        ...)

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 4} and
#'   \code{regular = TRUE}.
#' @export

gs_square <- function(anchor = NULL, window = NULL, sketch = NULL,
                      features = 1, fixed = FALSE, ...){

  if(is.null(anchor) & is.null(sketch)){
    stop("please provide anchor values.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)

  theGeom <- gs_polygon(anchor = anchor,
                        window = window,
                        sketch = sketch,
                        features = features,
                        vertices = 4,
                        regular = TRUE,
                        fixed = fixed,
                        ...)

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 2},
#'   \code{regular = FALSE} and the two complementing corners are derived from
#'   the two given opposing corners.
#' @export

gs_rectangle <- function(anchor = NULL, window = NULL, sketch = NULL,
                         features = 1, ...){

  if(is.null(anchor) & is.null(sketch)){
    stop("please provide anchor values.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)

  theGeom <- gs_polygon(anchor = anchor,
                        window = window,
                        sketch = sketch,
                        features = features,
                        vertices = 2,
                        ...)

  outTable <- NULL
  for(i in seq_along(theGeom@feat$fid)){
    geomSubset <- getSubset(theGeom, fid == !!i, slot = "feat")
    temp <- getExtent(geomSubset)
    temp <- tibble(x = c(rep(temp$x, each = 2), temp$x[1]),
                   y = c(temp$y, rev(temp$y), temp$y[1]),
                   fid = i)
    outTable <- bind_rows(outTable, temp)
  }

  theGeom@vert <- outTable

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 6} and
#'   \code{regular = TRUE}.
#' @export

gs_hexagon <- function(anchor = NULL, window = NULL, sketch = NULL,
                       features = 1, fixed = FALSE, ...){

  if(is.null(anchor) & is.null(sketch)){
    stop("please provide anchor values.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)

  theGeom <- gs_polygon(anchor = anchor,
                        window = window,
                        sketch = sketch,
                        features = features,
                        vertices = 6,
                        regular = TRUE,
                        fixed = fixed,
                        ...)

  invisible(theGeom)
}
