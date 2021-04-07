#' Dissolve geometric objects
#'
#' Remove all vertices that are not on the outer ring(s) enclosing all the input
#' features.
#' @param obj [\code{geometric object(1)}]\cr the object to dissolve.
#' @param by [\code{character(1)}]\cr in case sets of features to dissolve shall
#'   be selected based on an attribute, specify that here.
#' @param fid [\code{integerish(.)}]\cr in case only a subset of features shall
#'   be dissolved, specify that here.
#' @details This function encompases the functionality of operations that are
#'   known under several names. \itemize{
#'
#'   \item \emph{union}: When a new set is created that comprises all input
#'   members. Simple features access proposes that the union operation (both in
#'   at least sf and postgis, \code{st_union}) is not only a collection of the
#'   members, but is in fact the merged outcome thereof. ESRI seems to disagree,
#'   because here the union operation is limited to merely intersecting and
#'   combining all new members in one geometric object (while retaining the
#'   parent members attributes), which corresponds to
#'   \code{\link{gt_intersect}}.
#'
#'   \item \emph{merge}: When several features with coinciding vertices are
#'   joined into a single feature.
#'
#'   \item \emph{dissolve}: When the boundaries between features that shall be
#'   merged are removed. }
#'
#'   Functionally, these operations all do the same, they identify a certain set
#'   of features (irrespective of whether they overlap or not) that shall be
#'   grouped and joined into a smaller set of features by removing the vertices
#'   that would be inside the outer ring enclosing all features to join.
#'   However, since \code{geometr} doesn't know MULTI* features, disjoint
#'   features are not part of the same (MULTI*) feature, but part of the same
#'   feature group.
#' @return \code{geom} of the dissolved \code{obj}.
#' @family geometry tools
#' @examples
#' visualise(gtGeoms$polygon, linewidth = 3)
#' newPoly <- gt_dissolve(obj = gtGeoms$polygon)
#' visualise(geom = newPoly, linecol = "green", new = FALSE)
#'
#' @importFrom methods new
#' @export

gt_dissolve <- function(obj, by = NULL, fid = NULL){

  assertNumeric(x = fid, lower = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)

}