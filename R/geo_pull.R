#' Extract a single column from a geometric object
#'
#' This function allows to extract a specific column from any geometric object
#' for which all required getters are available and thus reflects the base
#' function \code{$}.
#' @param obj [gridded(1)][geom]\cr the object to pull a column from.
#' @param var [character(1)][character]\cr name of the variable to pull.
#' @param ungroup [logical(1)][logical]\cr this argument provides the attribute to
#'   pull per each individual feature, producing duplicates in case there is
#'   more than one feature per group.
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
#' geo_pull(obj = gtGeoms$point, var = "fid")
#'
#' # pull from a Raster* with RAT
#' geo_pull(obj = gtGeoms$grid, var = "cover")
#' @importFrom checkmate assertLogical
#' @importFrom geomio getGroups getFeatures getPoints
#' @importFrom dplyr select all_of pull left_join
#' @importFrom tibble tibble
#' @export

geo_pull <- function(obj, var, ungroup = FALSE){

  assertLogical(x = ungroup, len = 1)

  theGroups <- getGroups(x = obj)
  theFeatures <- getFeatures(x = obj)
  thePoints <- getPoints(x = obj)

  groupNames <- map(seq_along(theGroups), function(ix){
    tibble(layer = names(theGroups[ix]), names = names(theGroups[[ix]]))
  })
  groupNames <- do.call(rbind, groupNames)

  var <- as.character(var)
  if(var %in% groupNames$layer){

    temp <- dplyr::select(theGroups, gid, !!var)
    if(dim(temp)[1] != 0){

      temp <- as_tibble(merge(x = theFeatures, y = temp, by = names(temp)[1]))
      temp <- temp[order(temp$fid),]
      out <- temp[[var]]

    } else {

      out <- theFeatures[[var]]

    }

  } else if(var %in% names(theFeatures)){

    out <- theFeatures[[var]]

  } else if(var %in% names(thePoints)){

    out <- thePoints[[var]]

  } else {

    message(paste0("the variable '", var, "' is not available in this object."))
    out <- NULL

  }

  return(out)
}