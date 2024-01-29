#' Create a polygon \code{geom}
#'
#' Create any (regular) polygon geometry (of class \code{\link{geom}}) either by
#' specifying anchor values or by sketching it.
#' @param crds [data.frame(2)][data.frame]\cr Coordinates to build the
#'   \code{geom} from. It must include the column names \code{x} and \code{y}.
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @param features [integerish(1)][integer]\cr number of polygons to create.
#' @param vertices [integerish(1)][integer]\cr number of vertices per polygon;
#'   will be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param regular [logical(1)][logical]\cr should the polygon be regular, i.e.
#'   point symmetric (\code{TRUE}) or should the vertices be selected as
#'   provided by \code{anchor} (\code{FALSE}, default)?
#' @param geom [gridded(1)][geom]\cr the geom to cast to type 'polygon'.
#' @details The argument \code{regular} determines how the vertices provided in
#'   \code{anchor} or via \code{template} are transformed into a polygon:
#'   \itemize{ \item if \code{regular = FALSE} the resulting polygon is created
#'   from all vertices in \code{anchor}, \item if \code{regular = TRUE}, only
#'   the first two vertices are considered, as centre and indicating the
#'   distance to the (outer) radius.}
#' @return A \code{geom}.
#' @family geometry shapes
#' @examples
#' # 1. create a polygon programmatically
#' coords <- data.frame(x = c(0, 40, 40, 0),
#'                      y = c(0, 0, 40, 40))
#'
#' # if no window is set, the bounding box will be set as window
#' polyGeom <- geo_polygon(crds = coords)
#' geo_vis(polyGeom)
#'
#' # derive a regular polygon from the (first two) coordinates (per feature)
#' hexaGeom <- geo_polygon(crds = coords, vertices = 6, regular = TRUE)
#' geo_vis(hexaGeom, linecol = "green")
#'
#' # 2. cast to point geom from another type
#' polyGeom <- as_polygon(geom = gtGeoms$point)
#'
#' geo_vis(gtGeoms$point, linecol = "#FFB000", pointsymbol = 5)
#' geo_vis(polyGeom, linecol = "#33FF00", new = FALSE)
#'
#' # 3. sketch a hexagon
#' if(dev.interactive()){
#'   aHexagon <- geo_hexagon(features = 1)
#'   geo_vis(aHexagon, linecol = "#33FF00", linetype = 2, new = FALSE)
#' }
#' @importFrom stats dist
#' @importFrom checkmate testDataFrame assertNames testClass assertDataFrame
#'   testTRUE testNull testClass assertIntegerish assertLogical assert
#' @importFrom geomio getType getCRS getProvenance getPoints getFeatures
#'   getGroups getExtent
#' @importFrom tibble tibble add_row
#' @importFrom dplyr bind_cols bind_rows filter
#' @importFrom rlang !!
#' @export

geo_polygon <- function(crds = NULL, window = NULL, features = 1, vertices = 3,
                       regular = FALSE){

  # check arguments
  assertDataFrame(x = crds, types = "numeric", any.missing = FALSE, min.cols = 2, min.rows = 1, null.ok = TRUE, .var.name = "crds->cols(x)")
  assertDataFrame(x = window, types = "numeric", any.missing = FALSE, ncols = 2, nrows = 2, null.ok = TRUE, .var.name = "window->cols(x)")
  if(!is.null(window)) assertNames(x = colnames(window), permutation.of = c("x", "y"), .var.name = "window->names(x)")
  assertIntegerish(x = features, len = 1, lower = 1, any.missing = FALSE)
  assertIntegerish(x = vertices, min.len = 1, lower = 2, any.missing = FALSE)
  assertLogical(x = regular)

  # recycle vertices to match the number of features
  if(length(vertices) != features){
    vertices <- rep(vertices, length.out = features)
  }

  # build slots -----

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

    for(i in 1:features){

      if(!"fid" %in% names(crds)){
        crds$fid <- i
      }
      if(!"gid" %in% names(crds)){
        crds$gid <- i
      }

      tempPoints <- as_tibble(crds[crds$fid == i, ])
      tempFeatures <- tibble(fid = i, gid = unique(tempPoints$gid)[1])
      tempGroups <- tibble(gid = unique(tempPoints$gid)[1])

      # make a regular polygon, if that is requested
      if(regular){
        tempPoints <- .makeRegular(pts = tempPoints, vrt = vertices[i])
        window <- .updateWindow(input = tempPoints, window = window)
      }

      # combine with previous slots
      thePoints <- rbind(thePoints, tempPoints)
      theFeatures <- rbind(theFeatures, tempFeatures)
      theGroups <- rbind(theGroups, tempGroups)

    }

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

    for(i in 1:features){

      if(regular){
        clicks <- 2
      } else {
        clicks <- vertices[i]
      }

      message(paste0("please click ", clicks, " vertices."))
      tempcrds <- geo_locate(samples = clicks)
      assertNames(names(tempcrds), must.include = c("x", "y"), .var.name = "points->names(x)")
      if(is.null(tempcrds)){
        tempcrds <- geo_random(type = "polygon", vertices = vertices)
        tempcrds <- tempcrds@point
      }

      tempPoints <- tibble(fid = i, x = tempcrds$x, y = tempcrds$y)
      tempFeatures = tibble(fid = i, gid = i)
      tempGroups = tibble(gid = i)

      # make a regular polygon, if that is requested
      if(regular){
        tempPoints <- .makeRegular(pts = tempPoints, vrt = vertices[i])
        window <- .updateWindow(input = tempPoints, window = window)
      }

      # combine with previous slots
      theVertices <- rbind(thePoints, tempPoints)
      theFeatures <- rbind(theFeatures, tempFeatures)
      theGroups <- rbind(theGroups, tempGroups)

    }

  }

  # manage provenance -----
  theHistory <- list(paste0("object was created as 'polygon' geom."))

  # put together the geom ----
  theData <- list(features = theFeatures, groups = theGroups)

  theGeom <- new(Class = "geom",
                 type = "polygon",
                 label = "polygon_geom",
                 geometry = thePoints,
                 data = theData,
                 window = window,
                 crs = NA_character_,
                 provenance = theHistory)

  invisible(theGeom)
}

#' @describeIn geo_polygon wrapper of geo_polygon where \code{vertices = 3} and
#'   \code{regular = TRUE}.
#' @export

geo_triangle <- function(crds = NULL, window = NULL, features = 1){

  theGeom <- geo_polygon(crds = crds, window = window, features = features, vertices = 3, regular = TRUE)

  invisible(theGeom)
}

#' @describeIn geo_polygon wrapper of geo_polygon where \code{vertices = 4} and
#'   \code{regular = TRUE}.
#' @export

geo_square <- function(crds = NULL, window = NULL, features = 1){

  theGeom <- geo_polygon(crds = crds, window = window, features = features, vertices = 4, regular = TRUE)

  invisible(theGeom)
}

#' @describeIn geo_polygon wrapper of geo_polygon where \code{vertices = 2},
#'   \code{regular = FALSE} and the two complementing corners are derived from
#'   the two given opposing corners.
#' @export

geo_rectangle <- function(crds = NULL, window = NULL, features = 1){

  theGeom <- geo_polygon(crds = crds, window = window, features = features, vertices = 2)

  outTable <- NULL
  for(i in seq_along(theGeom@feature$fid)){
    geomSubset <- geo_filter(theGeom, fid == !!i)
    temp <- getExtent(geomSubset)
    temp <- tibble(x = c(rep(temp$x, each = 2), temp$x[1]),
                   y = c(temp$y, rev(temp$y), temp$y[1]),
                   fid = i)
    outTable <- bind_rows(outTable, temp)
  }

  theGeom@point <- outTable

  invisible(theGeom)
}

#' @describeIn geo_polygon wrapper of geo_polygon where \code{vertices = 6} and
#'   \code{regular = TRUE}.
#' @export

geo_hexagon <- function(crds = NULL, window = NULL, features = 1){

  theGeom <- geo_polygon(crds = crds, window = window, features = features, vertices = 6, regular = TRUE)

  invisible(theGeom)
}

# gs_circle <- function(){
#
# }

# gs_ellipse <- function(){
#
# }

#' @rdname geo_polygon
#' @export

as_polygon <- function(geom){

  # extract data -----
  thePoints <- getPoints(x = geom)
  theFeatures <- getFeatures(x = geom)
  theGroups <- getGroups(x = geom)
  theType <- getType(x = geom)

  dups <- duplicated(thePoints)

  # build slots -----
  theFeatures <- merge(x = thePoints[-which(names(thePoints) %in% c("x", "y"))], y = theFeatures, by = "fid", all.x = TRUE)[!dups,]
  thePoints <- thePoints[!dups,]
  if(theType[1] == "point"){
    theFeatures$fid <- theFeatures$gid
    thePoints$fid <- theFeatures$gid
  }

  # manage provenance -----
  hist <- paste0("object was cast to 'line' geom.")

  # put together the geom ----
  tempData <- list(features = theFeatures, groups = theGroups)
  theData <- stats::setNames(object = list(tempData), nm = "line_geom")

  theGeom <- new(Class = "geom",
                 type = "line",
                 geometry = thePoints,
                 data = theData,
                 window = getWindow(x = geom),
                 crs = getCRS(x = geom),
                 provenance = c(getProvenance(x = geom), list(hist)))

  #     hist <- paste0("object was cast to 'polygon' geom.")
  #     if(getType(x = anchor$obj)[1] == "point"){
  #       features <- length(unique(anchor$obj@feature$gid))
  #     } else {
  #       features <- length(unique(anchor$obj@feature$fid))
  #     }
  #     projection <- getCRS(x = anchor$obj)
  #
  #       hist <- paste0("object was cast to 'polygon' geom.")
  #       theHistory <- c(getProvenance(x = anchor$obj), list(hist))
  #
  #       if(is.null(theWindow)){
  #         theWindow <- anchor$obj@window
  #       }
  #       if(getType(x = anchor$obj)[1] == "point"){
  #         tempAnchor <- gt_filter(obj = anchor$obj, gid == !!i)
  #       } else {
  #         tempAnchor <- gt_filter(obj = anchor$obj, gid == !!i)
  #       }
  #       tempPoints <- getPoints(tempAnchor)
  #       tempFeatures <- getFeatures(tempAnchor)
  #       tempGroups <- getGroups(tempAnchor)
  #
  #       tempPoints <- left_join(tempPoints, tempFeatures, by = "fid")
  #       tempPoints <- select(mutate(tempPoints, fid = gid), -gid)
  #       tempFeatures <- tibble(fid = unique(tempPoints$fid), gid = unique(tempFeatures$gid))
  #
  #       if(dim(tempAnchor@point)[1] < 3){
  #         stop(paste0("a polygon geom must have at least 3 points per 'fid'."))
  #       }

  invisible(theGeom)
}
