#' Create a voronoi tiling \code{geom}
#'
#' @param anchor [\code{geom(1)}|\code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. It must include column names \code{x}, \code{y} and
#'   optionally a custom \code{fid}.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param sketch [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the tiling.
#' @param features [\code{integerish(1)}]\cr number of tiles to sketch.
#' @param ... [various]\cr graphical parameters to \code{\link{gt_locate}}, in
#'   case the tiling is sketched; see \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @family tilings
#' @examples
#' # 1. create voronoi polygons programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_point(anchor = coords, window = window)
#' visualise(voronoi = aGeom)
#'
#' tiles <- gs_voronoi(anchor = aGeom)
#' visualise(tiles, new = FALSE)
#' \donttest{
#' # 2. sketch a voronoi polygon by clicking into a template
#' gs_voronoi(sketch = gtRasters$continuous) %>%
#'   visualise(tiles = ., new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testClass testNull
#'   assertDataFrame assert assertIntegerish
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @importFrom deldir deldir tile.list
#' @export

gs_voronoi <- function(anchor = NULL, window = NULL, features = 3, sketch = NULL,
                       ...){

  # check arguments
  anchor <- .testAnchor(x = anchor)
  theWindow <- .testWindow(x = window)
  assertIntegerish(features, len = 1, lower = 1)

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

    template <- .testTemplate(x = sketch, ...)
    theGeom <- gt_sketch(template = template$obj,
                         shape = "point",
                         features = features,
                         ...)
    tempAnchor <- theGeom@point
    theWindow <- theGeom@window
    theFeatures = tibble(fid = seq_along(tempAnchor$x), gid = seq_along(tempAnchor$x))
    theGroups = tibble(gid = seq_along(tempAnchor$x))
    projection <- NA

  } else{
    if(anchor$type == "geom"){

      if(is.null(theWindow)){
        theWindow <- anchor$obj@window
      }
      tempAnchor <- anchor$obj@point
      theFeatures <- anchor$obj@feature$geometry
      theGroups <- anchor$obj@group$geometry
      projection <- getCRS(x = anchor$obj)

    } else if(anchor$type == "df"){

      if(is.null(theWindow)){
        theWindow = tibble(x = c(min(anchor$obj$x), max(anchor$obj$x), max(anchor$obj$x), min(anchor$obj$x), min(anchor$obj$x)),
                           y = c(min(anchor$obj$y), min(anchor$obj$y), max(anchor$obj$y), max(anchor$obj$y), min(anchor$obj$y)))
      }
      tempAnchor <- anchor$obj
      theFeatures = tibble(fid = seq_along(tempAnchor$x), gid = seq_along(tempAnchor$x))
      theGroups = tibble(gid = seq_along(tempAnchor$x))
      projection <- NA

    }
  }

  temp <- deldir(as.data.frame(tempAnchor), rw = c(min(theWindow$x), max(theWindow$x), min(theWindow$y), max(theWindow$y)), suppressMsge = TRUE)
  tempTiles <- tile.list(temp)

  for(i in seq_along(tempTiles)){

    temp <- tibble(x = tempTiles[[i]]$x,
                   y = tempTiles[[i]]$y,
                   fid = i)
    if(i == 1){
      theVertices <- temp
    } else{
      theVertices <- bind_rows(theVertices, temp)
    }
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

  invisible(theGeom)
}