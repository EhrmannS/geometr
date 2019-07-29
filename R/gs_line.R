#' Create a line geometry
#'
#' Create a line geometry (of class \code{\link{geom}}) either by specifying its
#' parameters or by sketching it.
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the geometry.
#' @param features [\code{integerish(1)}]\cr number of geometries to create.
#' @param vertices [\code{integerish(.)}]\cr number of vertices per geometry;
#'   will be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param ... [various]\cr graphical parameters to \code{\link{gt_locate}}, in
#'   case points are sketched; see \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @family geometry shapes
#' @details The arguments \code{anchor} and \code{template} indicate how the
#'   geometry is created: \itemize{ \item \code{anchor}: if set, the geometry is
#'   created parametrically, the input provided is used to parameterise the
#'   geometry. \item \code{template}: if set, the geometry is created
#'   interactively, by clicking into the plot.}
#'
#'   Possible additional arguments are: \itemize{ \item verbose = TRUE/FALSE
#'   \item graphical parameters to \code{\link{gt_locate}}, in case points are
#'   sketched; see \code{\link{gpar}}}.
#' @examples
#' library(magrittr)
#'
#' # create a points programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#'
#' # if no window is set, the bounding box (i.e. min/max values) will be set as window
#' (aGeom <- gs_line(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' visualise(geom = gs_line(anchor = coords))
#' gs_line(anchor = coords, window = window) %>%
#'   visualise(geom = ., linecol = "green", new = FALSE)
#'
#' # when a geom is used in 'anchor', its properties (e.g. 'window') are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' gs_line(anchor = aGeom) %>%
#'   visualise(geom = ., linecol = "deeppink")
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
#' @importFrom checkmate testDataFrame assertNames testClass testNull
#'   assertDataFrame assert assertIntegerish
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @export

gs_line <- function(anchor = NULL, window = NULL, template = NULL, features = 1,
                    vertices = NULL, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor, ...)
  theWindow <- .testWindow(x = window, ...)
  template <- .testTemplate(x = template, ...)

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
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
  assertIntegerish(features, len = 1, lower = 1)
  if(is.null(anchor)){
    assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE)
    if(length(vertices) != features){
      vertices <- rep(vertices, length.out = features)
    }
  } else{
    assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)
  }

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

  theVertices <- theFeatures <- theGroups <- NULL
  for(i in 1:features){

    # if anchor does not exists, make it
    if(is.null(anchor)){

      message(paste0("please click the ", vertices[i], " vertices."))
      if(i == 1){
        visualise(raster = template$obj)
      }
      theClicks <- gt_locate(samples = vertices[i], panel = tempName, silent = TRUE, ...)
      theWindow <- tibble(x = c(0, dims[2], dims[2], 0, 0),
                          y = c(0, 0, dims[1], dims[1], 0))
      tempAnchor <- tibble(fid = i,
                           # vid = 1:vertices[i],
                           x = theClicks$x,
                           y = theClicks$y)
      tempFeatures <- tibble(fid = i, gid = i)
      tempGroups <- tibble(gid = i)

    } else if(anchor$type == "geom"){
      # if(anchor$obj@type == "point"){
      #   anchor$obj@vert$fid <- rep(1, length(anchor$obj@vert$fid))
      #   anchor$obj@feat <- tibble(fid = 1, gid = 1)
      #   anchor$obj@group <- tibble(gid = 1)
      # }
      if(is.null(theWindow)){
        theWindow <- anchor$obj@window
      }
      tempAnchor <- anchor$obj@vert[anchor$obj@vert$fid == i,]
      tempFeatures <- anchor$obj@feat[anchor$obj@feat$fid == i,]
      tempGroups <- anchor$obj@group[anchor$obj@group$gid == i,]
    } else if(anchor$type == "df"){
      if(is.null(theWindow)){
        theWindow = tibble(x = c(min(anchor$obj$x), max(anchor$obj$x), max(anchor$obj$x), min(anchor$obj$x), min(anchor$obj$x)),
                           y = c(min(anchor$obj$y), min(anchor$obj$y), max(anchor$obj$y), max(anchor$obj$y), min(anchor$obj$y)))
      }
      if("fid" %in% names(anchor$obj)){
        tempAnchor <- anchor$obj[anchor$obj$fid == i, ]
      } else {
        tempAnchor <- anchor$obj
        tempAnchor <- bind_cols(tempAnchor, fid = rep(1, length.out = length(anchor$obj$x)))
      }
      tempFeatures <- tibble(fid = i, gid = i)
      tempGroups <- tibble(gid = i)
    }

    theNodes <- tempAnchor[c("x", "y", "fid")]

    theVertices <- bind_rows(theVertices, theNodes)
    theFeatures <- bind_rows(theFeatures, tempFeatures)
    theGroups <- bind_rows(theGroups, tempGroups)
  }

  theGeom <- new(Class = "geom",
                 type = "line",
                 vert = theVertices,
                 feat = theFeatures,
                 group = theGroups,
                 window = theWindow,
                 scale = "absolute",
                 crs = as.character(projection),
                 history = list(paste0("geometry was created as 'line'.")))

  invisible(theGeom)
}


# gs_curve <- function(){
#
# }


# gs_circle <- function(){
#
# }
# gs_ellipse <- function(){
#
# }