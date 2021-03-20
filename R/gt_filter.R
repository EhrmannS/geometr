#' Subset a geometric object using column values
#'
#' This function allows to subset any geometric object for which all required
#' getters are available.
#' @param obj [\code{geometric object(1)}]\cr the object to derive a subset
#'   from.
#' @param ... subset based on logical predicates defined in terms of the columns
#'   in \code{x} (of both, points, features and groups). Multiple conditions are
#'   combined with \code{&}. Only rows where the condition evaluates to TRUE are
#'   kept.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after deriving the subset.
#' @return \code{geom} of the subset of \code{obj}.
#' @family geometry tools
#' @examples
#' # get a subset of a geom
#' gt_filter(gtGeoms$point, y < -10)
#'
#' # get a subset of an sf-object
#' gt_filter(obj = gtSF$multilinestring, a == 1)
#' @importFrom rlang enquos eval_tidy exprs
#' @importFrom dplyr left_join
#' @importFrom methods new
#' @export

gt_filter <- function(obj, ..., update = TRUE){

  thePoints <- getPoints(x = obj)
  theFeatures <- getFeatures(x = obj)
  theGroups <- getGroups(x = obj)
  theWindow <- getWindow(x = obj)
  theType <- getType(x = obj)[1]

  theAttribs <- left_join(theFeatures, theGroups, by = "gid")
  subset <- exprs(...)
  matchesAttribs <- tryCatch(eval(parse(text = subset), envir = theAttribs), error = function(e) NULL)
  matchesPoints <- tryCatch(eval(parse(text = subset), envir = thePoints), error = function(e) NULL)

  if(!is.null(matchesAttribs)){
    theAttribs <- theAttribs[matchesAttribs,]
    newFeatures <- unique(theAttribs[names(theFeatures)])
    newGroups <- unique(theAttribs[names(theGroups)])
    newPoints <- thePoints[thePoints$fid %in% newFeatures$fid,]
  } else if(!is.null(matchesPoints)){
    newPoints <- thePoints[matchesPoints,]
    newFeatures <- theFeatures[theFeatures$fid %in% newPoints$fid,]
    newGroups <- theGroups[theGroups$gid %in% newFeatures$gid,]
  } else {
    stop(paste0("'", subset, "' matches neither in points, nor features nor groups."))
  }

  # update window
  if(update){
    theWindow <- .updateWindow(input = newPoints, window = theWindow)
  }

  # make history
  hist <- paste0("geom was subset with '", subset, "'.")

  out <- new(Class = "geom",
             type = theType,
             point = newPoints,
             feature = newFeatures,
             group = newGroups,
             window = theWindow,
             crs = getCRS(x = obj),
             history = c(getHistory(x = obj), list(hist)))

return(out)
}
