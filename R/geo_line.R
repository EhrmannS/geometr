#' Create a line \code{geom}
#'
#' Create a line geometry (of class \code{\link{geom}}) either by specifying
#' crds values or by sketching it.
#' @param crds [data.frame(2)][data.frame]\cr Coordinates to build the
#'   \code{geom} from. It must include the column names \code{x} and \code{y}.
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @param features [integerish(1)][integer]\cr number of lines to create.
#' @param vertices [integerish(.)][integer]\cr number of vertices per line; will
#'   be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param geom [gridded(1)][geom]\cr the geom to cast to type 'line'.
#' @return A line geom.
#' @family geometry shapes
#' @examples
#' # 1. create a line programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#'
#' # if no window is set, the bounding box will be set as window
#' (lineGeom <- geo_line(crds = coords))
#'
#' # ... otherwise the vertices are plotted relative to the window
#'
#' # the vertices are plottet relative to the window
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' lineGeom <- geo_line(crds = coords, window = window)
#'
#' geo_vis(lineGeom, linecol = "green")
#'
#' # 2. cast to point geom from another type
#' lineGeom <- as_line(geom = gtGeoms$point)
#'
#' geo_vis(gtGeoms$point, linecol = "#FFB000", pointsymbol = 5)
#' geo_vis(lineGeom, linecol = "#33FF00", new = FALSE)
#'
#' # 3. sketch a line
#' if(dev.interactive()){
#'   line <- geo_line(vertices = 4)
#'   geo_vis(line, linecol = "#B24223", linewidth = 5, new = FALSE)
#' }
#' @importFrom checkmate assertIntegerish
#' @importFrom geomio getType getCRS getProvenance getPoints getFeatures
#'   getGroups
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#' @export

geo_line <- function(crds = NULL, window = NULL, features = 1, vertices = 2){

  # check arguments ----
  assertDataFrame(x = crds, types = "numeric", any.missing = FALSE, min.cols = 2, min.rows = 1, null.ok = TRUE, .var.name = "crds->cols(x)")
  assertDataFrame(x = window, types = "numeric", any.missing = FALSE, ncols = 2, nrows = 2, null.ok = TRUE, .var.name = "window->cols(x)")
  if(!is.null(window)) assertNames(x = colnames(window), permutation.of = c("x", "y"), .var.name = "window->names(x)")
  assertIntegerish(features, len = 1, lower = 1)
  assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)

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

      clicks <- vertices[i]

      message(paste0("please click ", clicks, " vertices."))
      tempcrds <- geo_locate(samples = clicks)
      assertNames(names(tempcrds), must.include = c("x", "y"), .var.name = "points->names(x)")

      if(is.null(tempcrds)){
        tempcrds <- geo_random(type = "line", vertices = vertices)
        tempcrds <- tempcrds@point
      }

      tempPoints <- tibble(fid = i, x = tempcrds$x, y = tempcrds$y)
      tempFeatures = tibble(fid = i, gid = i)
      tempGroups = tibble(gid = i)

      # combine with previous slots
      thePoints <- rbind(thePoints, tempPoints)
      theFeatures <- rbind(theFeatures, tempFeatures)
      theGroups <- rbind(theGroups, tempGroups)

    }

  }

  # manage provenance -----
  theHistory <- list(paste0("object was created as 'line' geom."))

  # put together the geom ----
  theData <- list(features = theFeatures, groups = theGroups)

  theGeom <- new(Class = "geom",
                 type = "line",
                 label = "line_geom",
                 geometry = as_tibble(tempPoints),
                 data = theData,
                 window = window,
                 crs = NA_character_,
                 provenance = theHistory)

  invisible(theGeom)
}

#' @rdname geo_line
#' @export

as_line <- function(geom){

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

  invisible(theGeom)
}


# gs_curve <- function(){
#
# }
