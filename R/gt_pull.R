#' Extract a single column from a geometric object
#'
#' This function allows to extract a specific column from any geometric object
#' for which all required getters are available and thus reflects the base
#' function \code{$}.
#' @param obj [\code{geometric object(1)}]\cr the object to pull a column from.
#' @param var [\code{character(1)}]\cr name of the variable to pull.
#' @param ungroup [\code{logical(1)}]\cr inversely to the argument \code{group}
#'   in \code{\link{gc_geom}}, this argument provides the attribute to pull per
#'   each individual feature, producing duplicates in case there is more than
#'   one feature per group.
#' @details This function searches for \code{var} by first looking in the
#'   groups, then the features and finally the points of \code{obj}. This
#'   results always in an output that is limited to the unique cases of
#'   \code{var}. In case you want the explicit values of, for instance,
#'   \code{fid} in \code{obj@points}, you have to extract points and then use
#'   \code{\link[dplyr]{pull}} on the result.
#' @return vector of the column specified in \code{var}.
#' @family geometry tools
#' @examples
#' # pull values from a geom (there are two features, thus two values) ...
#' gt_pull(gtGeoms$point, "fid")
#'
#' # pull from a Raster* with RAT
#' gt_pull(gtGeoms$grid$categorical, "cover")
#' @importFrom checkmate assertLogical
#' @importFrom dplyr select all_of pull left_join
#' @importFrom tibble tibble
#' @importFrom raster getValues
#' @export

gt_pull <- function(obj, var, ungroup = FALSE){

  assertLogical(x = ungroup, len = 1)

  theGroups <- getGroups(x = obj)
  theFeatures <- getFeatures(x = obj)
  thePoints <- getPoints(x = obj)

  if(dim(theGroups)[1] == 0){
    if(is.numeric(theFeatures$values)){
      theGroups <- tibble(gid = sortUniqueC(theFeatures$values))
    }
  }

  var <- as.character(var)
  if(var %in% names(theGroups)){
    out <- select(theGroups, all_of(var), gid)

    if(ungroup){
      out <- left_join(theFeatures, out, by = "gid")
      out <- pull(out, var)
    } else {
      out <- pull(out, var)
    }
  } else if(var %in% names(theFeatures)){
    out <- pull(theFeatures, var)
  } else if(var %in% names(thePoints)){
    out <- pull(thePoints, var)
  } else {
    # message(paste0("the variable '", var, "' is not available in this object."))
    out <- NULL
  }


  return(out)
}