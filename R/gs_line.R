#' Create a line \code{geom}
#'
#' Create a line geometry (of class \code{\link{geom}}) either by specifying
#' anchor values or by sketching it.
#' @param anchor [\code{geom(1)}|\code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. It must include column names \code{x}, \code{y} and
#'   optionally a custom \code{fid}.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param features [\code{integerish(1)}]\cr number of lines to create.
#' @param vertices [\code{integerish(.)}]\cr number of vertices per line; will
#'   be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param ... [various]\cr graphical parameters to \code{\link{gt_locate}}, in
#'   case points are sketched; see \code{\link[grid]{gpar}}
#' @return A \code{geom}.
#' @family geometry shapes
#' @details The argument \code{anchor} indicates how the geom is created:
#'   \itemize{ \item if \code{anchor} is set, the geom is created parametrically
#'   from the points given in \code{anchor}, \item if it is not set either
#'   \code{window} or a default window between 0 and 1 is opened to sketch the
#'   geom.}
#' @examples
#' # 1. create a line programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#'
#' # if no window is set, the bounding box will be set as window
#' (aGeom <- gs_line(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aLine <- gs_line(anchor = coords, window = window)
#' visualise(aLine, linecol = "green")
#'
#' # when a geom is used in 'anchor', its properties are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' aLine <- gs_line(anchor = aGeom)
#' visualise(aLine, linecol = "deeppink")
#'
#' # 2. sketch a line
#' if(dev.interactive()){
#'   aLine <- gs_line(vertices = 4)
#'   visualise(aLine, linecol = "orange", linewidth = 5, new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testClass testNull
#'   assertDataFrame assert assertIntegerish
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @export

gs_line <- function(anchor = NULL, window = NULL, features = 1, vertices = 2, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor)
  theWindow <- .testWindow(x = window)
  assertIntegerish(features, len = 1, lower = 1)
  assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)

  if(!is.null(anchor)){
    if(anchor$type == "geom"){
      hist <- paste0("object was cast to 'line' geom.")
      if(getType(x = anchor$obj)[1] == "point"){
        features <- length(unique(anchor$obj@feature$gid))
      } else {
        features <- length(unique(anchor$obj@feature$fid))
      }
      projection <- getCRS(x = anchor$obj)
    } else if(anchor$type == "df"){
      hist <- paste0("object was created as 'line' geom.")
      if("fid" %in% names(anchor$obj)){
        features <- length(unique(anchor$obj$fid))
      }
      projection <- NA
    }
  }

  # recycle vertices to match the number of features
  if(length(vertices) != features){
    vertices <- rep(vertices, length.out = features)
  }

  theVertices <- theFeatures <- theGroups <- NULL
  for(i in 1:features){

    if(!is.null(anchor)){

      if(anchor$type == "geom"){

        hist <- paste0("object was cast to 'line' geom.")
        theHistory <- c(getHistory(x = anchor$obj), list(hist))

        if(is.null(theWindow)){
          theWindow <- anchor$obj@window
        }

        if(getType(x = anchor$obj)[1] == "point"){
          tempAnchor <- gt_filter(obj = anchor$obj, gid == !!i)
        } else {
          tempAnchor <- gt_filter(obj = anchor$obj, gid == !!i)
        }
        tempPoints <- getPoints(tempAnchor)
        tempFeatures <- getFeatures(tempAnchor)
        tempGroups <- getGroups(tempAnchor)

        tempPoints <- left_join(tempPoints, tempFeatures, by = "fid")
        tempPoints <- select(mutate(tempPoints, fid = gid), -gid)

        if(dim(tempAnchor@point)[1] < 2){
          stop(paste0("a line geom must have at least 2 points per 'fid'."))
        }
      } else if(anchor$type == "df"){

        theHistory <- list(paste0("object was created as 'line' geom."))

        if(is.null(theWindow)){
          theWindow = tibble(x = c(min(anchor$obj$x), max(anchor$obj$x)),
                             y = c(min(anchor$obj$y), max(anchor$obj$y)))
        }
        if("fid" %in% names(anchor$obj)){
          tempPoints <- anchor$obj[anchor$obj$fid == i, ]
        } else {
          tempPoints <- anchor$obj
          tempPoints <- bind_cols(tempPoints, fid = rep(1, length.out = length(anchor$obj$x)))
        }
        tempFeatures <- tibble(fid = i, gid = i)
        tempGroups <- tibble(gid = i)
      }

    } else {

      clicks <- vertices[i]

      theHistory <- list(paste0("object was created as 'polygon' geom."))

      # first, ensure that a plot is available, otherwise make one
      if(is.null(dev.list())){
        if(is.null(theWindow)){
          theWindow <- tibble(x = c(0, 1), y = c(0, 1))
        }
        visualise(window = theWindow)
      } else {
        extentGrobMeta <- grid.get(gPath("extentGrob"))
        theWindow <- tibble(x = c(0, as.numeric(extentGrobMeta$width)) + as.numeric(extentGrobMeta$x),
                            y = c(0, as.numeric(extentGrobMeta$height)) + as.numeric(extentGrobMeta$y))
      }
      message(paste0("please click ", clicks, " vertices."))
      tempAnchor <- gt_locate(samples = clicks)
      tempAnchor <- .testPoints(x = tempAnchor)
      if(is.null(tempAnchor)){
        tempAnchor <- gs_random(type = "line", vertices = vertices)
        tempAnchor <- tempAnchor@point
      }

      tempPoints <- tibble(fid = i, x = tempAnchor$x, y = tempAnchor$y)
      tempFeatures = tibble(fid = i, gid = 1)
      tempGroups = tibble(gid = 1)

      projection <- NA

    }

    theNodes <- tempPoints[c("x", "y", "fid")]
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
                 crs = as.character(projection),
                 history = theHistory)

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