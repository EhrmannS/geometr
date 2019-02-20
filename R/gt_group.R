#' Group geometries
#'
#' \code{gGroup} assigns the vertices of a \code{geom} into groups of features.
#' @param geom [\code{geom}]\cr Object of class \code{\link{geom}}.
#' @param index [\code{integerish(.)}]\cr a vector with a value for each vertex,
#'   according to which to group.
#' @param distance [\code{numeric(1)}]\cr specific distance of two vertices
#'   below which they will be included in the same cluster; must be within the
#'   range of \code{extent}.
#' @param clusters [\code{integerish(1)}]\cr the number of clusters for
#'   \code{\link{kmeans}} clustering.
#' @param ...  [various]\cr additional arguments either to
#'   \code{\link[stats]{hclust}} or to \code{\link[stats]{kmeans}}.
#' @details Only one of the three arguments \code{index}, \code{distance} or
#'   \code{clusters} need to be set, as grouping is only carried out by one of
#'   them.
#'
#'   In case the geom had an attribute table, this has to be redefined by
#'   default because it is impossible to determine how these attributes should
#'   be reattributed without external information.
#' @return \code{geom} with grouped coordinates.
#' @examples
#' coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
#'                      y = c(40, 40, 60, 70, 10, 20, 40),
#'                      fid = c(1, 1, 1, 1, 2, 2, 2))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window, show = TRUE)
#'
#' grouped <- gGroup(geom = aGeom, distance = 40)
#' visualise(geom = grouped)
#' @importFrom checkmate testList assertNames assertDataFrame
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom methods new
#' @importFrom stats kmeans dist hclust cutree
#' @export

gt_group <- function(geom, index = NULL, distance = NULL, clusters = NULL, ...){

  assertClass(geom, classes = "geom")
  assertIntegerish(index, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  assertNumeric(distance, finite = TRUE, null.ok = TRUE)
  assertIntegerish(clusters, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  if(is.null(distance) & is.null(index) & is.null(clusters)){
    stop("please provide either 'distance', 'index' or 'clusters'.")
  }

  coords <- geom@coords
  toGroup <- coords[c("x", "y")]

  if(!is.null(index)){
    newId <- rep(index, length.out = dim(toGroup)[1])
  }
  if(!is.null(distance)){
    temp <- dist(toGroup)
    h <- hclust(temp, ...)
    newId <- cutree(h, h=distance)
  }
  if(!is.null(clusters)){
    temp <- kmeans(toGroup, centers = clusters, ...)
    newId <- temp$cluster
  }

  temp <- bind_cols(fid = newId, vid = coords$vid, toGroup)
  temp <- temp[order(temp$fid),]
  if(geom@type == "point"){
    vertices <- as.integer(table(newId))
  } else{
    vertices <- rep(1, length(unique(newId)))
  }

  out <- new(Class = "geom",
             type = geom@type,
             coords = temp,
             attr = tibble(fid = unique(newId), n = vertices),
             window = geom@window,
             scale = geom@scale,
             crs = geom@crs,
             history = list(paste0("geometry values were regrouped")))

  return(out)
}