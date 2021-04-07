#' Combine geometric objects
#'
#' @param ... [\code{geometric object(.)}]\cr the objects to be combined.
#' @details The output of this function is merely a combination of the input
#'   objects and does NOT equal to the set operation "union". For set
#'   operations, see \code{\link{gt_intersect}}.
#' @return combined \code{geom} of the input objects.
#' @family geometry tools
#' @examples
#' (gt_combine(gtSF$multipolygon, gtGeoms$polygon))
#' @importFrom rlang eval_tidy
#' @importFrom checkmate assertTRUE
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom methods new
#' @export

gt_combine <- function(...){

  objs <- rlang::enquos(...)

  # ensure that objs have the same type and CRS
  for(i in seq_along(objs)){
    obj <- eval_tidy(objs[[i]])

    if(i == 1){
      theType <- getType(x = obj)[1]
      theCRS <- getCRS(x = obj)
    } else {
      # assertTRUE(x = getType(x = obj)[1] == theType, .var.name = "getType(...)")
      assertNames(x = getType(x = obj)[1], identical.to = theType, .var.name = paste0("getType(x = objs[", i, "])"))
      assertTRUE(x = getCRS(x = obj) == theCRS, .var.name = paste0("getCRS(x = objs[", i, "])"), na.ok = TRUE)
    }
  }

  for(i in seq_along(objs)){

    obj <- eval_tidy(objs[[i]])

    tempPoints <- getPoints(x = obj)
    tempFeatures <- getFeatures(x = obj)
    tempGroups <- getGroups(x = obj)

    if(i == 1){
      thePoints <- tempPoints
      theFeatures <- tempFeatures
      theGroups <- tempGroups

      maxFID <- max(tempFeatures$fid)
      maxGID <- max(tempGroups$gid)
    } else {
      tempPoints$fid <- tempPoints$fid + maxFID
      tempFeatures$fid <- tempFeatures$fid + maxFID
      tempFeatures$gid <- tempFeatures$gid + maxGID
      tempGroups$gid <- tempGroups$gid + maxGID

      thePoints <- bind_rows(thePoints, tempPoints)
      theFeatures <- bind_rows(theFeatures, tempFeatures)
      theGroups <- bind_rows(theGroups, tempGroups)

      maxFID <- maxFID + max(tempFeatures$fid)
      maxGID <- maxGID + max(tempGroups$gid)
    }
  }

  # make the window
  theWindow <- tibble(x = c(min(thePoints$x), max(thePoints$x)),
                      y = c(min(thePoints$y), max(thePoints$y)))

  # make history
  # hist <- paste0("coordinate values were rescaled between x[", paste0(range$x, collapse = " "), "] and y[",  paste0(range$y, collapse = " "), "]")

  # make new geom
  out <- new(Class = "geom",
             type = theType,
             point = thePoints,
             feature = theFeatures,
             group = theGroups,
             window = theWindow,
             crs = theCRS,
             history = list())

}