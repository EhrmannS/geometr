#' Create a voronoi tiling
#'
#' @param anchor [\code{geom} | \code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. In case of \code{data.frame}, it must include column
#'   names \code{x}, \code{y}, \code{fid} and optinal attributes; see Examples.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the tiling.
#' @param features [\code{integerish(1)}]\cr number of tiles to create.
#' @param ... [various]\cr graphical parameters to \code{\link{gt_locate}}, in case
#'   the tiling is sketched; see \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @family tilings
#' @examples
#' # create voronoi polygons programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_point(anchor = coords, window = window)
#' visualise(voronoi = aGeom)
#'
#' tiles <- gs_voronoi(anchor = aGeom)
#' visualise(tiles, new = FALSE)
#'
#' \dontrun{
#'
#' input <- gtRasters$continuous
#'
#' # create voronoi polygons interactively
#' tiles <- gs_voronoi(template = input)
#' visualise(geom = tiles, linecol = "orange", new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testClass testNull
#'   assertDataFrame assert assertIntegerish
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @importFrom deldir deldir tile.list
#' @export

gs_voronoi <- function(anchor = NULL, window = NULL, template = NULL, features = 3,
                       ...){

  # check arguments
  anchor <- .testAnchor(x = anchor, ...)
  theWindow <- .testWindow(x = window, ...)
  template <- .testTemplate(x = template, ...)

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  if(!is.null(anchor)){
    if(anchor$type == "geom"){
      features <- length(unique(anchor$obj@feat$fid))
    } else if(anchor$type == "df"){
      if("fid" %in% names(anchor$obj)){
        features <- length(unique(anchor$obj$fid))
      }
    }
  }
  assertIntegerish(features, len = 1, lower = 1)

  # get some raster properties
  if(!is.null(template)){
    if(template$type == "RasterLayer"){
      tempName <- names(template$obj)
      dims <- dim(template$obj)
      projection <- getCRS(x = template$obj)
    } else{
      tempName <- "layer"
      dims <- dim(template$obj)
      projection <- NA
    }
  } else{
    tempName <- "layer"
    projection <- NA
  }

  # if anchor does not exists, make it
  if(is.null(anchor)){

    message(paste0("please click the ", features, " vertices."))
    visualise(raster = template$obj)
    theClicks <- gt_locate(samples = features, panel = tempName, silent = TRUE, ...)
    theWindow <- tibble(x = c(0, dims[2], dims[2], 0, 0),
                        y = c(0, 0, dims[1], dims[1], 0))
    tempAnchor <- tibble(x = theClicks$x,
                         y = theClicks$y,
                         fid = 1:features)
    theFeatures <- tibble(fid = unique(tempAnchor$fid), gid = unique(tempAnchor$fid))
    theGroups <- tibble(gid = unique(tempAnchor$fid))

  } else if(anchor$type == "geom"){
    if(is.null(theWindow)){
      theWindow <- anchor$obj@window
    }
    tempAnchor <- anchor$obj@vert
    theFeatures <- anchor$obj@feat
    theGroups <- anchor$obj@group
  } else if(anchor$type == "df"){
    if(is.null(theWindow)){
      theWindow = tibble(x = c(min(anchor$obj$x), max(anchor$obj$x), max(anchor$obj$x), min(anchor$obj$x), min(anchor$obj$x)),
                         y = c(min(anchor$obj$y), min(anchor$obj$y), max(anchor$obj$y), max(anchor$obj$y), min(anchor$obj$y)))
    }
    tempAnchor <- anchor$obj
    theFeatures = tibble(fid = seq_along(tempAnchor$x), gid = seq_along(tempAnchor$x))
    theGroups = tibble(gid = seq_along(tempAnchor$x))
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

  out <- new(Class = "geom",
             type = "polygon",
             vert = theVertices,
             # feat = tibble(fid = unique(theVertices$fid), gid = unique(theVertices$fid)),
             # group = tibble(gid = unique(theVertices$fid)),
             feat = theFeatures,
             group = theGroups,
             window = theWindow,
             scale = "absolute",
             crs = as.character(projection),
             history = list(paste0("geometry was created as voronoi 'polygon'.")))

  invisible(out)
}