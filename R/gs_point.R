#' Create a point \code{geom}
#'
#' Create a point geometry (of class \code{\link{geom}}) either by specifying
#' anchor values or by sketching it.
#' @param anchor [\code{geom(1)}|\code{data.frame(1)}]\cr Object to derive the
#'   \code{geom} from. It must include column names \code{x}, \code{y} and
#'   optionally a custom \code{fid}.
#' @param window [\code{data.frame(1)}]\cr in case the reference window deviates
#'   from the bounding box of \code{anchor} (minimum and maximum values),
#'   specify this here.
#' @param vertices [\code{integer(1)}]\cr number of vertices.
#' @param ... [various]\cr graphical parameters to \code{\link{gt_locate}}, in
#'   case points are sketched; see \code{\link[grid]{gpar}}
#' @return An invisible \code{geom}.
#' @family geometry shapes
#' @details The arguments \code{anchor} and \code{template} indicate how the line
#'   is created: \itemize{ \item if \code{anchor} is set, the line is created
#'   parametrically from the given objects' points, \item if an object is set in
#'   \code{template}, this is used to create the \code{geom} interactively, by
#'   clicking into the plot.}
#' @examples
#' # 1. create points programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#'
#' # if no window is set, the bounding box will be set as window
#' (aGeom <- gs_point(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' points <- gs_point(anchor = coords, window = window)
#' visualise(points, linecol = "green")
#'
#' # when a geom is used in 'anchor', its properties are passed on
#' aGeom <- setWindow(x = aGeom, to = window)
#' points <- gs_point(anchor = aGeom)
#' visualise(points)
#' \donttest{
#' # 2. sketch two points
#' points <- gs_point(vertices = 2)
#' visualise(points, linecol = "green", pointsymbol = 5, new = FALSE)
#' }
#' @importFrom checkmate testDataFrame assertNames testNull assert testClass
#'   assertLogical assertIntegerish
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom methods new
#' @export

gs_point <- function(anchor = NULL, window = NULL, vertices = 1, ...){

  # check arguments
  anchor <- .testAnchor(x = anchor)
  theWindow <- .testWindow(x = window)
  assertIntegerish(vertices, min.len = 1, lower = 0, any.missing = FALSE)

  if(!is.null(anchor)){

    if(anchor$type == "geom"){

      hist <- paste0("object was cast to 'point' geom.")
      theHistory <- c(getHistory(x = anchor$obj), list(hist))

      if(is.null(theWindow)){
        theWindow <- anchor$obj@window
      }
      thePoints <- getPoints(anchor$obj)
      theFeatures <- getFeatures(anchor$obj)
      theGroups <- getGroups(anchor$obj)
      projection <- getCRS(x = anchor$obj)

      dups <- duplicated(thePoints)

      theFeatures <- left_join(dplyr::select(thePoints, -x, -y), theFeatures, by = "fid")[!dups,]
      theFeatures$fid <- seq_along(theFeatures$fid)
      thePoints <- thePoints[!dups,]
      thePoints$fid <- seq_along(thePoints$x)

    } else if(anchor$type == "df"){

      theHistory <- list(paste0("object was created as 'point' geom."))

      if(is.null(theWindow)){
        theWindow = tibble(x = c(min(anchor$obj$x), max(anchor$obj$x)),
                           y = c(min(anchor$obj$y), max(anchor$obj$y)))
      }
      thePoints <- bind_cols(anchor$obj)
      if(!"gid" %in% names(thePoints)){
        thePoints$fid <- seq_along(thePoints$x)
        vertices <- dim(thePoints)[1]
        theFeatures <- tibble(fid = 1:vertices, gid = 1)
        theGroups <- tibble(gid = 1)
      } else {
        thePoints$fid <- seq_along(thePoints$x)
        vertices <- dim(thePoints)[1]
        theFeatures <- tibble(fid = 1:vertices, gid = thePoints$gid)
        theGroups <- tibble(gid = unique(thePoints$gid))
      }
      projection <- NA

    }

  } else {

    theHistory <- list(paste0("object was created as 'point' geom."))

    if(vertices != 0){

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
      message(paste0("please click ", vertices, " vertices."))
      tempAnchor <- gt_locate(samples = vertices)
      tempAnchor <- .testPoints(x = tempAnchor)
      if(is.null(tempAnchor)){
        tempAnchor <- gs_random(type = "point", vertices = vertices)
        tempAnchor <- tempAnchor@point
      }

      thePoints <- tibble(fid = seq_along(tempAnchor$fid), x = tempAnchor$x, y = tempAnchor$y)
      theFeatures = tibble(fid = thePoints$fid, gid = 1)
      theGroups = tibble(gid = 1)

    } else {

      if(is.null(theWindow)){
        theWindow <- tibble(x = c(0, 1), y = c(0, 1))
      }
      thePoints <- tibble(fid = integer(), x = numeric(), y = numeric())
      theFeatures <- tibble(fid = integer(), gid = integer())
      theGroups <- tibble(gid = integer())
    }

    projection <- NA

  }

  theGeom <- new(Class = "geom",
                 type = "point",
                 point = thePoints,
                 feature = theFeatures,
                 group = theGroups,
                 window = theWindow,
                 crs = as.character(projection),
                 history = theHistory)

  invisible(theGeom)
}
