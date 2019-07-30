#' Sketch geometries
#'
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the geometry.
#' @param shape [\code{character(1)}]\cr a geometry shape that should be
#'   sketched, possible are the geom types \code{"point"}, \code{"line"} and
#'   \code{"polygon"} and special cases thereof (recently implemented are
#'   \code{"triangle"}, \code{"rectangle"}, \code{"square"}, \code{"hexagon"})
#'   and \code{"random"}.
#' @param features [\code{integerish(1)}]\cr number of geometries to create.
#' @param vertices [\code{integerish(.)}]\cr number of vertices per geometry;
#'   will be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param regular [\code{logical(1)}]\cr if a polygon is sketched, should it be
#'   regular, i.e. point symmetric (\code{TRUE}) with the number of corners
#'   defined by \code{vertices} or should the vertices be selected according to
#'   the click locations, resulting in a non-regular polygon (\code{FALSE},
#'   default)?
#' @param fixed [\code{logical(1)}]\cr if a regular polygon is sketched, should
#'   it be aligned vertically (\code{TRUE}, default), or should it be aligned
#'   according to the second click (\code{FALSE}); only relevant if
#'   \code{regular = TRUE}.
#' @param show [\code{logical(1)}]\cr should plot information be shown in the
#'   plot (\code{TRUE}), or should the geom merely be returned in the console
#'   (\code{FALSE, default})
#' @param ... [various]\cr additional arguments to \code{\link{gt_locate}}.
#' @details bla
#' @return An invisible \code{geom}.
#' @family geometry tools
#' @examples
#'
#' \dontrun{
#'
#' input <- gtRasters$continuous
#'
#' # create a square interactively
#' squareGeom <- gs_square(sketch = input) %>%
#' visualise(geom = squareGeom, linecol = "orange", new = FALSE)
#'
#' # ... or an approximate circle (actually a hectogon)
#' circleGeom <- gs_polygon(template = input, vertices = 100, regular = TRUE) %>%
#'   visualise(geom = ., linecol = "deeppink", new = FALSE)
#'
#' # create two arbitrary polygons interactively
#' polyGeom <- gs_polygon(template = input, features = 2, vertices = c(4, 6)) %>%
#'   visualise(geom = ., linecol = "green", linewidth = 1, linetype = "dashed", new = FALSE)
#' }
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
#' @importFrom checkmate assertSubset assertIntegerish assertLogical
#' @export

gt_sketch <- function(template = NULL, shape = NULL, features = 1, vertices = NULL,
                      regular = FALSE, fixed = TRUE, show = FALSE, ...){

  theCoices <- c("point", "line", "polygon", "triangle", "rectangle", "square", "hexagon", "random")
  # check arguments
  template <- .testTemplate(x = template, ...)
  assert_character(x = shape, len = 1)
  assertSubset(x = shape, choices = theCoices)
  assertIntegerish(x = features, len = 1, lower = 1)
  assertIntegerish(x = vertices, min.len = 1, null.ok = TRUE)
  assertLogical(x = regular)
  assertLogical(x = fixed)

  # get the geom type and, if missing, number of vertices
  if(shape == "random"){
    shape <- sample(x = theCoices, size = 1)
  }
  if(shape %in% "point"){
    type <- "point"
    vertices <- features
    features <- 1
  } else if(shape %in% c("line")){
    type <- "line"
    if(is.null(vertices)){
      vertices <- 2
    }
  } else if(shape %in% c("polygon", "triangle", "rectangle", "square", "hexagon")){
    type <- "polygon"
    if(is.null(vertices)){
      vertices <- 3
    }
  }

  # recycle vertices to match the number of features
  if(length(vertices) != features){
    vertices <- rep(vertices, length.out = features)
  }

  # get some raster properties
  if(template$type == "RasterLayer"){
    tempName <- names(template$obj)
    dims <- dim(template$obj)
    projection <- getCRS(x = template$obj)
  } else{
    tempName <- "layer"
    dims <- dim(template$obj)
    projection <- NA
  }

  theVertices <- theFeatures <- theGroups <- NULL
  for(i in 1:features){

    if(regular & type == "polygon"){
      message("please click the polygons' center and first vertex.")
      clicks <- 2
    } else{
      message(paste0("please click ", vertices[i], " vertices."))
      clicks <- vertices[i]
    }
    if(i == 1){
      visualise(template$obj)
    }
    theClicks <- gt_locate(samples = clicks, panel = tempName, show = show)
    theWindow <- tibble(x = c(0, dims[2], dims[2], 0, 0),
                        y = c(0, 0, dims[1], dims[1], 0))
    tempAnchor <- tibble(x = theClicks$x,
                         y = theClicks$y,
                         fid = i)
    tempFeatures <- tibble(fid = i, gid = i)
    tempGroups <- tibble(gid = i)

    if(fixed){
      openingAngle <- 0
    } else{
      # get the angle between the first and second click
      openingAngle <- atan((theClicks$x[1] - theClicks$x[2]) / (theClicks$y[1] - theClicks$y[2])) * 180 / pi
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
      # theWindow <- .updateWindow(geom = theNodes, window = theWindow)
    } else {
      theNodes <- tempAnchor
    }

    theVertices <- bind_rows(theVertices, theNodes)
    theFeatures <- bind_rows(theFeatures, tempFeatures)
    theGroups <- bind_rows(theGroups, tempGroups)
  }

  theGeom <- new(Class = "geom",
                 type = type,
                 vert = theVertices,
                 feat = theFeatures,
                 group = theGroups,
                 window = theWindow,
                 scale = "absolute",
                 crs = as.character(projection),
                 history = list(paste0("geometry was created as '", type, "'.")))

  if(show){
    visualise(theGeom, new = FALSE)
  }
  invisible(theGeom)
}
