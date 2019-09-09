#' Reflect geometries
#'
#' Reflect \code{geom}s across a line.
#' @param geom [\code{geom(.)}]\cr the object to reflect.
#' @param fid [\code{integerish(.)}]\cr if only a subset of features shall be
#'   rotated, specify that here.
#' @param update [\code{logical(1)}]\cr whether or not to update the window slot
#'   after rotation.
#'

gt_reflect <- function(geom = NULL, fid = NULL, update = NULL){

  # assertClass(geom, classes = "geom")
  # xIsList <- testList(x, types = "numeric", any.missing = FALSE)
  # xIsNumeric <- testNumeric(x, any.missing = FALSE, len = 1, null.ok = TRUE)
  # yIsList <- testList(y, types = "numeric", any.missing = FALSE)
  # yIsNumeric <- testNumeric(y, any.missing = FALSE, len = 1, null.ok = TRUE)
  # assert(xIsList, xIsNumeric)
  # assert(yIsList, yIsNumeric)
  # assertIntegerish(x = fid, any.missing = FALSE, null.ok = TRUE)
  # assertLogical(x = update, len = 1, any.missing = FALSE)
  #
  # if(length(ids) == 1){
  #   newHistory <- paste0("geometry was reflected")
  # } else {
  #   newHistory <- paste0("geometries were reflected")
  # }
  #
  # out <- new(Class = "geom",
  #            type = geom@type,
  #            vert = temp,
  #            feat = geom@feat,
  #            group = geom@group,
  #            window = window,
  #            scale = geom@scale,
  #            crs = geom@crs,
  #            history = c(geom@history, list(newHistory)))
  #
  # return(out)
}