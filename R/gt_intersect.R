#' Intersect geometric objects
#'
#' @param obj [\code{geometric object(1)}]\cr the object to intersect.
#' @param fid [\code{integerish(.)}]\cr in case only a subset of features shall
#'   be intersected, specify that here.
#' @param identify [\code{logical(1)}]\cr whether or not to identify from which
#'   input features the intersected features originate. This is saved into the
#'   features attribute table and can be used to carry out set operations, see
#'   Details.
#' @section Set-theoretic operations: At the basis of most set operations is an
#'   intersection of the involved sets, which allows to recombine the members
#'   into new sets. In \code{geometr} this is handled via
#'   \code{gt_intersect(..., identify = TRUE)}, which attaches the parent
#'   membership of each member of the intersected set to the features attribute
#'   table, ready for subsetting.
#'
#'   For example, when two features are intersected, the intersection would have
#'   the "set"-attribute \code{"1 2"} (according to the feature ID of the parent
#'   members), while the non-overlapping parts would have the "set"-attribute
#'   \code{"1"} and \code{"2"} respectively. Moreover, the number of memberships
#'   is pasted into the "parent"-attribute, where the intersection would have
#'   the value 2 (because it originates from an intersection of 2 parent
#'   members) and the non-overlapping parts would have the number 1. This allows
#'   to carry out all basic and more complicated set operations via selecting
#'   the respective subsets with \code{\link{gt_filter}} and merging members
#'   into new sets via \code{\link{gt_dissolve}}. \itemize{
#'
#'   \item union:
#'
#'   \item intersection:
#'
#'   \item difference:
#'
#'   \item symmetric difference:
#'   }
#' @family geometry tools
#' @examples
#' @return \code{geom} of the intersected input objects.
#' @importFrom checkmate assertLogical
#' @importFrom methods new
#' @export

gt_intersect <- function(obj, fid = NULL, identify = TRUE){

  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = update, len = 1, any.missing = FALSE)


}