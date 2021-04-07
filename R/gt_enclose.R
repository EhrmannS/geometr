#' Derive bounding container of geometric objects
#'
#' Enclose all vertices in different types of bounding containers.
#' @param obj [\code{geometric object(1)}]\cr the object for which to derive a
#'   bounding container.
#' @param type [\code{character(1)}]\cr the type of bounding container to
#'   derive, by default this is the \code{convex} hull. Other options are
#'   \code{rectangle}, \code{box}, \code{diamond}, \code{ball} and
#'   \code{ellipsoid}, for explanations, see Details.
#' @param fid [\code{integerish(.)}]\cr in case a bounding container shall be
#'   derived only for a subset of features, specify that here.
#' @details Bounding containers are geometric objects of the smallest area that
#'   encloses a set of vertices. \itemize{
#'
#'   \item The \code{convex} hull is the smallest bounding container, as it
#'   encloses all vertices along the outer edges, and is contained in any other
#'   bounding container.
#'
#'   \item The minimal bounding \code{rectangle} is the enclosing rectangle with
#'   the smallest area and an arbitrary orientation.
#'
#'   \item The bounding \code{box} is a special case of the minimal bounding
#'   rectangle, which is derived from the minimum and maximum values in x and y
#'   dimension and is thus "parallel" to the x and y axis.
#'
#'   \item The bounding \code{diamond} is also a special case of the minimal
#'   bounding rectangle, where the lines have a slope of -1 or 1 (i.e., which is
#'   rotated by 45°).
#'
#'   \item The bounding \code{ball}, or minimal spanning sphere, is the smallest
#'   circle that encloses all vertices.
#'
#'   \item The bounding \code{ellipsoid}, or minimal spanning ellipsoid, is the
#'   smallest ellipsoid that encloses all vertices. }
#' @return \code{geom} of the bounding container.
#' @family geometry tools
#' @examples
#' @importFrom checkmate assertChoice
#' @importFrom methods new
#' @export

gt_enclose <- function(obj, fid = NULL, type = "convex"){

  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assertChoice(x = type, choices = c("convex", "box", "rectangle", "diamond", "ball", "ellipsoid"))

  # http://www.geomalgorithms.com/a12-_hull-3.html

}

