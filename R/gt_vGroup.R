#' Group point geometries
#'
#' \code{gt_vGroup} assigns the vertices of a \code{geom} of type \code{point}
#' into new groups.
#' @param geom [\code{geom}]\cr object of class \code{\link{geom}} and type
#'   \code{point}.
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
#'   \code{clusters} needs to be set, as grouping is only carried out by one of
#'   them.
#' @return \code{geom} with grouped coordinates.
#' @examples
#' coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
#'                      y = c(40, 40, 60, 70, 10, 20, 40),
#'                      fid = c(1, 1, 1, 1, 2, 2, 2))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_point(anchor = coords, window = window)
#'
#' grouped <- gt_vGroup(geom = aGeom, distance = 40)
#' visualise(geom = grouped)
#' @importFrom checkmate testList assertNames assertDataFrame
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols left_join
#' @importFrom methods new
#' @importFrom stats kmeans dist hclust cutree
#' @export

gt_vGroup <- function(geom, index = NULL, distance = NULL, clusters = NULL, ...){

  assertClass(geom, classes = "geom")
  assertTRUE(geom@type == "point")
  assertIntegerish(index, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  assertNumeric(distance, finite = TRUE, null.ok = TRUE)
  assertIntegerish(clusters, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  if(is.null(distance) & is.null(index) & is.null(clusters)){
    stop("please provide either 'distance', 'index' or 'clusters'.")
  }

  vert <- getVertices(x = geom)
  attr <- getTable(x = geom)
  toGroup <- vert[c("x", "y")]

  # determine new fids
  if(!is.null(index)){
    newFids <- rep(index, length.out = dim(toGroup)[1])
  }
  if(!is.null(distance)){
    temp <- dist(toGroup)
    h <- hclust(temp, ...)
    newFids <- cutree(h, h=distance)
  }
  if(!is.null(clusters)){
    temp <- kmeans(toGroup, centers = clusters, ...)
    newFids <- temp$cluster
  }

  newAttr <- tibble(fid = newFids, gid = newFids)
  newAttr <- left_join(newAttr, attr[-which(colnames(attr) == "gid")], by = "fid")

  # determine vers
  vertices <- bind_cols(fid = newFids, vid = vert$vid, toGroup)
  vertices <- vertices[order(vertices$fid),]

  out <- new(Class = "geom",
             type = geom@type,
             vert = vertices,
             attr = newAttr,
             window = geom@window,
             scale = geom@scale,
             crs = geom@crs,
             history = list(paste0("geometry values were regrouped")))

  return(out)
}