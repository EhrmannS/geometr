#' Create a voronoi tiling \code{geom}
#'
#' @param crds [data.frame(2)][data.frame]\cr Coordinates to build the
#'   \code{geom} from. It must include the column names \code{x} and \code{y}.
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @param features [integerish(1)][integer]\cr number of tiles to sketch.
#' @return A \code{geom}.
#' @family tilings
#' @examples
#' # 1. create voronoi polygons programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geo_point(crds = coords, window = window)
#' geo_vis(voronoi = aGeom, linecol = "deeppink")
#'
#' tiles <- geo_voronoi(crds = coords, window = window)
#' geo_vis(tiles, new = FALSE)
#'
#' # 2. sketch a voronoi polygon
#' if(dev.interactive()){
#'   tiles <- geo_voronoi()
#'   geo_vis(tiles, new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testClass testNull
#'   assertDataFrame assert assertIntegerish
#' @importFrom geomio getCRS
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @importFrom deldir deldir tile.list
#' @export

geo_voronoi <- function(crds = NULL, window = NULL, features = 3){

  # check arguments
  assertDataFrame(x = crds, types = "numeric", any.missing = FALSE, min.cols = 2, min.rows = 1, null.ok = TRUE, .var.name = "crds->cols(x)")
  assertDataFrame(x = window, types = "numeric", any.missing = FALSE, ncols = 2, nrows = 2, null.ok = TRUE, .var.name = "window->cols(x)")
  if(!is.null(window)) assertNames(x = colnames(window), permutation.of = c("x", "y"), .var.name = "window->names(x)")
  assertIntegerish(features, len = 1, lower = 1)

  if(!is.null(crds)){

    assertNames(x = colnames(crds), must.include = c("x", "y"), .var.name = "crds->names(x)")

    if(is.null(window)){
      window = tibble(x = c(min(crds$x), max(crds$x)),
                      y = c(min(crds$y), max(crds$y)))
    }
    tempcrds <- crds
    theFeatures = tibble(fid = seq_along(tempcrds$x), gid = seq_along(tempcrds$x))
    theGroups = tibble(gid = seq_along(tempcrds$x))

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

    message(paste0("please click ", features, " vertices."))
    tempcrds <- geo_locate(samples = features)
    assertNames(names(tempcrds), must.include = c("x", "y"), .var.name = "points->names(x)")

    if(is.null(tempcrds)){
      tempcrds <- geo_random(type = "point", vertices = features)
      tempcrds <- tempcrds@point
    }

    theFeatures = tibble(fid = seq_along(tempcrds$x), gid = seq_along(tempcrds$x))
    theGroups = tibble(gid = theFeatures$gid)

  }

  temp <- deldir(as.data.frame(tempcrds), rw = c(min(window$x), max(window$x), min(window$y), max(window$y)), suppressMsge = TRUE)
  tempTiles <- tile.list(temp)

  for(i in seq_along(tempTiles)){

    temp <- tibble(x = tempTiles[[i]]$x,
                   y = tempTiles[[i]]$y,
                   fid = i)
    if(i == 1){
      thePoints <- temp
    } else{
      thePoints <- bind_rows(thePoints, temp)
    }

  }

  # manage provenance -----
  theHistory <- list(paste0("object was created as 'polygon' geom in a voronoi tiling."))

  # put together the geom ----
  theData <- list(features = theFeatures, groups = theGroups)

  theGeom <- new(Class = "geom",
                 label = "voronoi_geom",
                 type = "polygon",
                 geometry = thePoints,
                 data = theData,
                 window = window,
                 crs = NA_character_,
                 provenance = theHistory)

  invisible(theGeom)
}