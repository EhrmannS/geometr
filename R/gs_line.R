#' Create a line \code{geom}
#'
#' Create a line geometry (of class \code{\link{geom}}) either by specifying
#' anchor values or by sketching it.
#' @param anchor [\code{geom(1)}|\code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. It must include column names \code{x}, \code{y} and
#'   optionally a custom \code{fid}. To set further attributes, use
#'   \code{\link{setTable}}.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param sketch [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch lines.
#' @param features [\code{integerish(1)}]\cr number of lines to create.
#' @param vertices [\code{integerish(.)}]\cr number of vertices per line; will
#'   be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param ... [various]\cr additional arguments; see Details.
#' @return An invisible \code{geom}.
#' @family geometry shapes
#' @details The arguments \code{anchor} and \code{sketch} indicate how the line
#'   is created: \itemize{ \item if \code{anchor} is set, the line is created
#'   parametrically from the given objects' points, \item if an object is set in
#'   \code{sketch}, this is used to create the \code{geom} interactively, by
#'   clicking into the plot.}
#'
#'   Possible additional arguments are: \itemize{ \item verbose = TRUE/FALSE
#'   \item graphical parameters to \code{\link{gt_locate}}, in case points are
#'   sketched; see \code{\link[grid]{gpar}}}
#' @examples
#' # create a line programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#'
#' # if no window is set, the bounding box will be set as window
#' (aGeom <- gs_line(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' library(magrittr)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' gs_line(anchor = coords, window = window) %>%
#'   visualise(linecol = "green")
#'
#' # if a plot is already open, vertices are set relative to its' window
#' visualise(geom = gs_line(anchor = coords), new = FALSE)
#'
#' # when a geom is used in 'anchor', its properties are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' gs_line(anchor = aGeom) %>%
#'   visualise(linecol = "deeppink")
#'
#' \dontrun{
#'
#' # sketch a line by clicking into a template
#' gs_line(sketch = gtRasters$continuous, vertices = 4) %>%
#'   visualise(geom = ., linecol = "orange", linewidth = 5, new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testClass testNull
#'   assertDataFrame assert assertIntegerish
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @export

gs_line <- function(anchor = NULL, window = NULL, features = 1, vertices = NULL,
                    sketch = NULL, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor, ...)
  theWindow <- .testWindow(x = window, ...)
  assertIntegerish(features, len = 1, lower = 1)
  assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)

  if(is.null(anchor) & is.null(sketch)){
    stop("please provide anchor values.")
  }
  if(!is.null(anchor)){
    if(anchor$type == "geom"){
      hist <- paste0("object was cast to 'line' geom.")
      if(anchor$obj@type == "point"){
        anchor$obj@point$fid <- rep(1, length(anchor$obj@point$fid))
        anchor$obj@feature <- tibble(fid = 1, gid = 1)
        anchor$obj@group <- tibble(gid = 1)
        features <- 1
      } else {
        features <- length(unique(anchor$obj@feature$fid))
      }
    } else if(anchor$type == "df"){
      hist <- paste0("object was created as 'line' geom.")
      if("fid" %in% names(anchor$obj)){
        features <- length(unique(anchor$obj$fid))
      }
    }
  }

  # sketch the geometry
  if(!is.null(sketch)){
    hist <- paste0("object was sketched as 'line' geom.")

    template <- .testTemplate(x = sketch, ...)
    theGeom <- gt_sketch(template = template$obj,
                         shape = "line",
                         features = features,
                         vertices = vertices,
                         ...)

  } else {

    theVertices <- theFeatures <- theGroups <- NULL
    for(i in 1:features){

      if(anchor$type == "geom"){

        if(is.null(theWindow)){
          theWindow <- anchor$obj@window
        }
        tempAnchor <- anchor$obj@point[anchor$obj@point$fid == i,]
        tempFeatures <- anchor$obj@feature[anchor$obj@feature$fid == i,]
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
          tempAnchor <- bind_cols(tempAnchor, fid = rep(1, length.out = length(anchor$obj$x)))
        }
        tempFeatures <- tibble(fid = i, gid = i)
        tempGroups <- tibble(gid = i)
        projection <- NA

      }

      theNodes <- tempAnchor[c("x", "y", "fid")]

      theVertices <- bind_rows(theVertices, theNodes)
      theFeatures <- bind_rows(theFeatures, tempFeatures)
      theGroups <- bind_rows(theGroups, tempGroups)
    }

    theGeom <- new(Class = "geom",
                   type = "line",
                   point = theVertices,
                   feature = theFeatures,
                   group = theGroups,
                   window = theWindow,
                   scale = "absolute",
                   crs = as.character(projection),
                   history = c(getHistory(x = anchor$obj), list(hist)))
  }

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