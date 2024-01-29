#' Create or cast to a point \code{geom}
#'
#' Create a point geometry (of class \code{\link{geom}}) either programatically
#' by specifying coordinate values, by sketching it or by casting to it from
#' another geom type.
#' @param crds [data.frame(2)][data.frame]\cr Coordinates to build the
#'   \code{geom} from. It must include the column names \code{x} and \code{y}.
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @param vertices [integerish(1)][integer]\cr if neither \code{crds} nor
#'   \code{window} are given, this indicates how often you can click into a plot
#'   to determine the location of the coordinates manually.
#' @param geom [gridded(1)][geom]\cr the geom to cast to type 'point'.
#' @return A point geom.
#' @family geometry shapes
#' @examples
#' # 1. create points programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#'
#' # if no window is set, the bounding box will be set as window ...
#' (pointGeom <- geo_point(crds = coords))
#'
#' # ... otherwise the vertices are plotted relative to the window
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' points <- geo_point(crds = coords, window = window)
#'
#' geo_vis(points, linecol = "#FFB000")
#'
#' # 2. cast to point geom from another type
#' pointGeom <- as_point(geom = gtGeoms$polygon)
#'
#' geo_vis(gtGeoms$polygon, linecol = "#FFB000", theme = setTheme(box = list(fillcol = "#282828")))
#' geo_vis(pointGeom, linecol = "#33FF00", pointsymbol = 5, new = FALSE)
#'
#' # 3. sketch two points
#' if(dev.interactive()){
#'   points <- geo_point(vertices = 2)
#'   geo_vis(points, linecol = "#B24223", pointsymbol = 22, new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testNull assert testClass
#'   assertLogical assertIntegerish
#' @importFrom geomio getProvenance getPoints getFeatures getGroups getWindow
#'   getCRS
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

geo_point <- function(crds = NULL, window = NULL, vertices = 1){

  # check arguments ----
  assertDataFrame(x = crds, types = "numeric", any.missing = FALSE, min.cols = 2, min.rows = 1, null.ok = TRUE, .var.name = "crds->cols(x)")
  assertDataFrame(x = window, types = "numeric", any.missing = FALSE, ncols = 2, nrows = 2, null.ok = TRUE, .var.name = "window->cols(x)")
  if(!is.null(window)) assertNames(x = colnames(window), permutation.of = c("x", "y"), .var.name = "window->names(x)")
  assertIntegerish(vertices, min.len = 1, lower = 0, any.missing = FALSE)

  # build the tables ...
  thePoints <- tibble(fid = integer(), x = numeric(), y = numeric())
  theFeatures <- tibble(fid = integer(), gid = integer())
  theGroups <- tibble(gid = integer())

  # ... then fill them with values
  if(!is.null(crds)){

    assertNames(x = colnames(crds), must.include = c("x", "y"), .var.name = "crds->names(x)")

    if(is.null(window)){
      window = tibble(x = c(min(crds$x), max(crds$x)),
                      y = c(min(crds$y), max(crds$y)))
    }

    thePoints <- crds

    if(!"gid" %in% names(crds)){
      crds$gid <- 1
    }

    thePoints$fid <- seq_along(thePoints$x)
    vertices <- dim(thePoints)[1]
    theFeatures <- tibble(fid = 1:vertices, gid = crds$gid)
    theGroups <- tibble(gid = unique(crds$gid))

  } else {

    # if no plot is available, first make one
    if(is.null(dev.list())){

      if(is.null(window)){
        window <- tibble(x = c(0, 1), y = c(0, 1))
      }

      geo_vis(window = window)

    } else {

      extentGrobMeta <- grid.get(gPath("extentGrob"))
      window <- tibble(x = c(as.numeric(extentGrobMeta$x), as.numeric(extentGrobMeta$x) + as.numeric(extentGrobMeta$width)),
                       y = c(as.numeric(extentGrobMeta$y), as.numeric(extentGrobMeta$y) + as.numeric(extentGrobMeta$height)))

    }

    # ... if there are more than 0 vertices to be made, locate them
    if(vertices != 0){

      message(paste0("please click ", vertices, " vertices."))
      tempcrds <- geo_locate(samples = vertices)
      assertNames(names(tempcrds), must.include = c("x", "y"), .var.name = "points->names(x)")

      if(is.null(tempcrds)){
        tempcrds <- geo_random(type = "point", vertices = vertices)
        tempcrds <- tempcrds@geometry
      }

      thePoints <- tibble(fid = seq_along(tempcrds$fid), x = tempcrds$x, y = tempcrds$y)
      theFeatures = tibble(fid = thePoints$fid, gid = 1)
      theGroups = tibble(gid = 1)

    }

  }

  # manage provenance -----
  theHistory <- list(paste0("object was created as 'point' geom."))

  # put together the geom ----
  theData <- list(features = theFeatures, groups = theGroups)

  theGeom <- new(Class = "geom",
                 type = "point",
                 label = "point_geom",
                 geometry = thePoints,
                 data = theData,
                 window = window,
                 crs = NA_character_,
                 provenance = theHistory)

  invisible(theGeom)
}

#' @rdname geo_point
#' @export

as_point <- function(geom){

  # extract data -----
  thePoints <- getPoints(x = geom)
  theFeatures <- getFeatures(x = geom)
  theGroups <- getGroups(x = geom)

  dups <- duplicated(thePoints)

  # build slots -----
  theFeatures <- merge(x = thePoints[-which(names(thePoints) %in% c("x", "y"))], y = theFeatures, by = "fid", all.x = TRUE)[!dups,]
  theFeatures$fid <- seq_along(theFeatures$fid)
  thePoints <- thePoints[!dups,]
  thePoints$fid <- seq_along(thePoints$x)

  # manage provenance -----
  hist <- paste0("object was cast to 'point' geom.")

  # put together the geom ----
  tempData <- list(features = theFeatures, groups = theGroups)
  theData <- stats::setNames(object = list(tempData), nm = "point_geom")

  theGeom <- new(Class = "geom",
                 type = "point",
                 geometry = thePoints,
                 data = theData,
                 window = getWindow(x = geom),
                 crs = getCRS(x = geom),
                 provenance = c(getProvenance(x = geom), list(hist)))

  invisible(theGeom)
}
