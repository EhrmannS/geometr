#' Subset a geometric object using column values
#'
#' This function allows to subset any geometric object for which all required
#' getters are available.
#' @param obj [gridded(1)][geom]\cr the object to derive a subset
#'   from.
#' @param ... subset based on logical predicates defined in terms of the columns
#'   in \code{x} (of both, points, features and groups). Multiple conditions are
#'   combined with \code{&}. Only rows where the condition evaluates to TRUE are
#'   kept.
#' @param update [logical(1)][logical]\cr whether or not to update the window slot
#'   after deriving the subset.
#' @return \code{geom} of the subset of \code{obj}.
#' @family geometry tools
#' @examples
#' geo_filter(obj = gtGeoms$point, y < -10)
#'
#' geo_filter(obj = gtGeoms$point, attr %in% c("A", "C", "E"))
#' @importFrom geomio getPoints getFeatures getGroups getWindow getType getNames
#'   getCRS getProvenance
#' @importFrom rlang enquos eval_tidy exprs
#' @importFrom dplyr left_join
#' @importFrom methods new
#' @export

geo_filter <- function(obj, ..., update = TRUE){

  thePoints <- getPoints(x = obj)
  theFeatures <- getFeatures(x = obj)
  theGroups <- getGroups(x = obj)

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
    window <- tibble(x = c(min(newPoints$x), max(newPoints$x)),
                     y = c(min(newPoints$y), max(newPoints$y)))
  } else {
    window <- getWindow(x = obj)
  }

  # make history
  hist <- paste0("geom was subset with '", subset, "'.")

  tempData <- list(features = newFeatures, groups = newGroups)
  theData <- stats::setNames(list(tempData), getNames(x = obj))

  out <- new(Class = "geom",
             type = getType(x = obj)[1],
             geometry = newPoints,
             data = theData,
             window = window,
             crs = getCRS(x = obj),
             provenance = c(getProvenance(x = obj), list(hist)))

return(out)
}
