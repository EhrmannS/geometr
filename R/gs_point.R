#' Create a point geometry
#'
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#'   that serves as template to sketch the geometry.
#' @param vertices [\code{integer(1)}]\cr number of vertices.
#' @param ... [various]\cr graphical parameters to \code{\link{locate}}, in case
#'   points are sketched; see \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @family shapes
#' @examples
#' library(magrittr)
#'
#' # create points programmatically
#' somePoints <- data.frame(X = c(5190599, 5222810, 5041066, 5234735,
#'                          5326537, 5027609, 5281527, 5189955), Y = c(3977612,
#'                          4060164, 3997230, 4117856, 4028167, 3971119, 4118207,
#'                          4062838))
#' (pointsGeom <- gs_point(anchor = somePoints)) %>%
#'   visualise(geom = ., col = "darkorange")
#'
#' \dontrun{
#'
#' input <- gtRasters$continuous
#'
#' # create points interactively
#' myPoints <- gs_point(template = input, vertices = 5, col = "deeppink") %>%
#'   gt_group(, index = rep(1, 5))
#' anExtent <- gs_rectangle(myPoints)
#' visualise(geom = anExtent, col = "green", new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testNull assert testClass
#'   assertLogical assertIntegerish
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom methods new
#' @export

gs_point <- function(anchor = NULL, window = NULL, template = NULL,
                     vertices = NULL, ...){

  # check arguments
  anchorExists <- !testNull(anchor)
  if(anchorExists){
    assertDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
    colnames(anchor) <- tolower(colnames(anchor))
    assertNames(names(anchor), must.include = c("x", "y"), subset.of = c("fid", "vid", "x", "y"))
  }
  windowExists <- !testNull(window)
  if(windowExists){
    assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
    colnames(window) <- tolower(colnames(window))
    assertNames(names(window), must.include = c("x", "y"))
  } else{
    if(anchorExists){
      window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                       y = c(min(anchor$y), max(anchor$y)))
    }
  }
  templateExists <- !testNull(template)
  if(templateExists){
    assert(
      testClass(template, "RasterLayer"),
      testClass(template, "matrix")
    )
  }
  if(!anchorExists & !templateExists){
    stop("please provide either 'anchor' or 'template'.")
  }
  if(!anchorExists){
    assertIntegerish(vertices, min.len = 1, any.missing = FALSE)
  } else{
    assertIntegerish(vertices, min.len = 1, any.missing = FALSE, null.ok = TRUE)
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

  # if anchor does not exists, make it
  if(!anchorExists){
    message("please click the ", vertices, " vertices.")
    visualise(raster = template)
    coords <- locate(samples = vertices, panel = tempName, silent = TRUE, show = TRUE, ...)
    window <- tibble(x = c(0, dims[2]),
                     y = c(0, dims[1]))
    anchor <- tibble(x = coords$x,
                     y = coords$y)
  } else{
    if(!windowExists){
      window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                       y = c(min(anchor$y), max(anchor$y)))
    }
  }

  if(!"vid" %in% names(anchor)){
    anchor <- bind_cols(vid = seq_along(anchor$x), anchor)
  }
  if(!"fid" %in% names(anchor)){
    anchor <- bind_cols(fid = seq_along(anchor$x), anchor)
  }
  anchor <- anchor[c("fid", "vid", "x", "y")]
  theGeom <- new(Class = "geom",
                 type = "point",
                 coords = anchor,
                 attr = tibble(fid = unique(anchor$fid), n = as.data.frame(table(anchor$fid))$Freq),
                 window = tibble(x = rep(window$x, each = 2), y = c(window$y, rev(window$y))),
                 scale = "absolute",
                 crs = as.character(projection),
                 history = list(paste0("geometry was created as 'point'")))

  invisible(theGeom)
}
