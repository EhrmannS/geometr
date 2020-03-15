#' Create a polygon \code{geom}
#'
#' Create any (regular) polygon geometry (of class \code{\link{geom}}) either by
#' specifying anchor values or by sketching it.
#' @param anchor [\code{geom(1)}|\code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. It must include column names \code{x}, \code{y} and
#'   optionally a custom \code{fid}.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param features [\code{integerish(1)}]\cr number of polygons to create.
#' @param vertices [\code{integerish(.)}]\cr number of vertices per polygon;
#'   will be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param sketch [\code{raster(1)}]\cr raster object that serves as template to
#'   sketch polygons.
#' @param regular [\code{logical(1)}]\cr should the polygon be regular, i.e.
#'   point symmetric (\code{TRUE}) or should the vertices be selected as
#'   provided by \code{anchor} (\code{FALSE}, default)?
#' @param ... [various]\cr graphical parameters to \code{\link{gt_locate}}, in
#'   case points are sketched; see \code{\link[grid]{gpar}}
#' @details The arguments \code{anchor} and \code{sketch} indicate how the line
#'   is created: \itemize{ \item if \code{anchor} is set, the line is created
#'   parametrically from the given objects' points, \item if an object is set in
#'   \code{sketch}, this is used to create the \code{geom} interactively, by
#'   clicking into the plot.}
#'
#'   The argument \code{regular} determines how the vertices provided in
#'   \code{anchor} or via \code{sketch} are transformed into a polygon:
#'   \itemize{ \item if \code{regular = FALSE} the resulting polygon is created
#'   from all vertices in \code{anchor}, \item if \code{regular = TRUE}, only
#'   the first two vertices are considered, as center and indicating the
#'   distance to the (outer) radius.}
#' @return An invisible \code{geom}.
#' @family geometry shapes
#' @examples
#' # 1. create a polygon programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#'
#' # if no window is set, the bounding box will be set as window
#' (aGeom <- gs_polygon(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' library(magrittr)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' gs_polygon(anchor = coords, vertices = 6, window = window,
#'            regular = TRUE) %>%
#'   visualise(linecol = "green")
#'
#' # when a geom is used in 'anchor', its properties are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' gs_polygon(anchor = aGeom) %>%
#'   visualise(geom = ., fillcol = "deeppink")
#' gs_rectangle(anchor = aGeom) %>%
#'   visualise(geom = ., new = FALSE)
#' \donttest{
#' # 2. sketch a hexagon by clicking into a template
#' gs_hexagon(sketch = gtRasters$continuous) %>%
#'   visualise(geom = ., linecol = "deeppink", linetype = 2, new = FALSE)
#' }
#' @importFrom stats dist
#' @importFrom checkmate testDataFrame assertNames testClass assertDataFrame
#'   testTRUE testNull testClass assertIntegerish assertLogical assert
#' @importFrom tibble tibble add_row
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom rlang !!
#' @export

gs_polygon <- function(anchor = NULL, window = NULL, features = 1, vertices = NULL,
                       sketch = NULL, regular = FALSE, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor)
  theWindow <- .testWindow(x = window)
  assertIntegerish(x = features, len = 1, lower = 1)
  assertIntegerish(x = vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = regular)

  if(is.null(anchor) & is.null(sketch)){
    stop("please provide anchor values.")
  }
  if(!is.null(anchor)){
    if(anchor$type == "geom"){
      hist <- paste0("object was cast to 'polygon' geom.")
      features <- length(unique(anchor$obj@feature$geometry$fid))
      projection <- getCRS(x = anchor$obj)
    } else if(anchor$type == "df"){
      hist <- paste0("object was created as 'polygon' geom.")
      if("fid" %in% names(anchor$obj)){
        features <- length(unique(anchor$obj$fid))
      }
      projection <- NA
    }
  }

  # sketch the geometry
  if(!is.null(sketch)){
    hist <- paste0("object was sketched as 'polygon' geom.")

    template <- .testTemplate(x = sketch, ...)
    theGeom <- gt_sketch(template = template$obj,
                         shape = "polygon",
                         features = features,
                         vertices = vertices,
                         regular = regular,
                         ...)
  } else{

    theVertices <- theFeatures <- theGroups <- NULL
    for(i in 1:features){

      if(anchor$type == "geom"){

        if(is.null(theWindow)){
          theWindow <- anchor$obj@window
        }
        tempAnchor <- anchor$obj@point[anchor$obj@point$fid == i,]
        if(dim(tempAnchor)[1] < 3){
          stop(paste0("a polygon geom must have at least 3 points per 'fid'."))
        }
        openingAngle <- atan((tempAnchor$x[1] - tempAnchor$x[2]) / (tempAnchor$y[1] - tempAnchor$y[2])) * 180 / pi
        tempFeatures <- anchor$obj@feature$geometry[anchor$obj@feature$geometry$fid == i,]
        tempGroups <- anchor$obj@group$geometry[anchor$obj@group$geometry$gid == i,]

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
        theWindow <- .updateWindow(input = theNodes, window = theWindow)
      } else{
        theNodes <- tempAnchor[c("x", "y", "fid")]
      }

      theNodes <- .updateVertices(input = theNodes)
      theVertices <- bind_rows(theVertices, theNodes)
      theFeatures <- bind_rows(theFeatures, tempFeatures)
      theGroups <- bind_rows(theGroups, tempGroups)
    }

    theGeom <- new(Class = "geom",
                   type = "polygon",
                   point = theVertices,
                   feature = list(geometry = theFeatures),
                   group = list(geometry = theGroups),
                   window = theWindow,
                   scale = "absolute",
                   crs = as.character(projection),
                   history = c(getHistory(x = anchor$obj), list(hist)))
  }

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 3} and
#'   \code{regular = TRUE}.
#' @export

gs_triangle <- function(anchor = NULL, window = NULL, sketch = NULL,
                        features = 1, ...){

  theGeom <- gs_polygon(anchor = anchor, window = window, sketch = sketch, features = features, vertices = 3, regular = TRUE, ...)

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 4} and
#'   \code{regular = TRUE}.
#' @export

gs_square <- function(anchor = NULL, window = NULL, sketch = NULL,
                      features = 1, ...){

  theGeom <- gs_polygon(anchor = anchor, window = window, sketch = sketch, features = features, vertices = 4, regular = TRUE, ...)

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 2},
#'   \code{regular = FALSE} and the two complementing corners are derived from
#'   the two given opposing corners.
#' @export

gs_rectangle <- function(anchor = NULL, window = NULL, sketch = NULL,
                         features = 1, ...){

  theGeom <- gs_polygon(anchor = anchor, window = window, sketch = sketch, features = features, vertices = 2, ...)

  outTable <- NULL
  for(i in seq_along(theGeom@feature$geometry$fid)){
    geomSubset <- getFeatures(theGeom, fid == !!i)
    temp <- getExtent(geomSubset)
    temp <- tibble(x = c(rep(temp$x, each = 2), temp$x[1]),
                   y = c(temp$y, rev(temp$y), temp$y[1]),
                   fid = i)
    outTable <- bind_rows(outTable, temp)
  }

  theGeom@point <- outTable

  invisible(theGeom)
}

#' @describeIn gs_polygon wrapper of gs_polygon where \code{vertices = 6} and
#'   \code{regular = TRUE}.
#' @export

gs_hexagon <- function(anchor = NULL, window = NULL, sketch = NULL,
                       features = 1, ...){

  theGeom <- gs_polygon(anchor = anchor, window = window, sketch = sketch, features = features, vertices = 6, regular = TRUE, ...)

  invisible(theGeom)
}
