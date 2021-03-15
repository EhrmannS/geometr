#' Extract a single column from a geometric object
#'
#' This function allows to extract a specific column from any geometric object
#' for which all required getters are available and thus reflects the base
#' function \code{$}.
#' @param obj [\code{geometric object(1)}]\cr the object to pull a column from.
#' @param var [\code{character(1)}]\cr name of the variable to pull.
#' @details This function searches for \code{var} by first looking in the
#'   groups, then the features and finally the points of \code{obj}. This
#'   results always in an output that is limited to the unique cases of
#'   \code{var}. In case you want the explicit values of, for instance,
#'   \code{fid} in \code{obj@points}, you have to extract points and then use
#'   \code{\link[dplyr]{pull}} on the result.
#' @return unique values of the column specified in \code{var}.
#' @family geometry tools
#' @examples
#' # pull values from a geom (there are two features, thus two values) ...
#' gt_pull(gtGeoms$point, "fid")
#'
#' # pull from a Raster* with RAT
#' gt_pull(gtGeoms$grid$categorical, "cover")
#'
#' # pull from an sf-object
#' gt_pull(gtSF$point, "a")
#' @importFrom dplyr pull
#' @export

gt_pull <- function(obj, var){

  theGroups <- getGroups(x = obj)
  theFeatures <- getFeatures(x = obj)
  thePoints <- getPoints(x = obj)

  var <- as.character(var)
  if(var %in% names(theGroups)){
    out <- pull(theGroups, var)
  } else if(var %in% names(theFeatures)){
    out <- pull(theFeatures, var)
  } else if(var %in% names(thePoints)){
    out <- pull(thePoints, var)
  } else {
    message(paste0("the variable '", var, "' is not available in this object."))
    out <- NULL
  }

  return(out)
}