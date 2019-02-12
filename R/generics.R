#' Get the attribute table (generic)
#' @param x the object from which to derive the attribute table.
#' @param ... other arguments.
#' @export

setGeneric(name = "getTable",
           def = function(x, ...){
             standardGeneric("getTable")
           })

#' Set the attribute table (generic)
#' @param x the object to which to assign \code{table}.
#' @param table [\code{data.frame(.)}]\cr the attribute table.
#' @param ... other arguments.
#' @export

setGeneric(name = "setTable",
           def = function(x, table, ...){
             standardGeneric("setTable")
           })

#' Get the table of coordinates (generic)
#' @param x the object from which to extract the coordinates
#' @param ... other arguments.
#' @export

setGeneric(name = "getCoords",
           def = function(x, ...){
             standardGeneric("getCoords")
           })

#' Get the reference window (generic)
#' @param x the object from which to derive the reference window.
#' @param ... other arguments.
#' @export

setGeneric(name = "getWindow",
           def = function(x, ...){
             standardGeneric("getWindow")
           })

#' Set the reference window (generic)
#' @param x the object in which to set the reference window.
#' @param to [\code{data.frame(1)}]\cr two oposing corners or all four
#'   corners of the rectangle.
#' @param ... other arguments.
#' @export

setGeneric(name = "setWindow",
           def = function(x, to, ...){
             standardGeneric("setWindow")
           })


#' Get the extent (generic)
#' @param x the object from which to derive the extent.
#' @param ... other arguments.
#' @export

setGeneric(name = "getExtent",
           def = function(x, ...){
             standardGeneric("getExtent")
           })

#' Get the subset (generic)
#'
#' \code{getSubset} returns the subsetted object.
#' @param x object to \code{subset}.
#' @param attr [\code{integerish(.)} | \code{logical(.)} |
#'   \code{character(1)}]\cr rows of the attribute table to keep.
#' @param coords [\code{integerish(.)} | \code{logical(.)} |
#'   \code{character(1)}]\cr coordinates to keep.
#' @param ... other arguments.
#' @export

setGeneric(name = "getSubset",
           def = function(x, attr, coords, ...){
             standardGeneric("getSubset")
           })

#' Get the coordinate reference system (generic)
#'
#' @param x the object from which to derive the coordinate reference system.
#' @param ... other arguments.
#' @seealso \code{\link{setCRS}}
#' @export

setGeneric(name = "getCRS",
           def = function(x, ...){
             standardGeneric("getCRS")
           })

#' Set (or transform) the coordinate reference system (generic)
#'
#' In case an object has not yet assigned a coordinate reference system, this
#' function simply assigns it. In case the object has already a valid crs, a
#' transformation to the new crs will be carried out. The transformation is
#' computed with the standard defined in the \code{rgdal} package.
#' @param x the object for which to set the coordinate reference system.
#' @param crs [\code{character(1)}]\cr the coordinate reference system to set
#'   for this object.
#' @param ... other arguments.
#' @seealso \code{\link{getCRS}}
#' @export

setGeneric(name = "setCRS",
           def = function(x, crs, ...){
             standardGeneric("setCRS")
           })

#' Get the history (generic)
#' @param x the object from which to derive the history.
#' @param ... other arguments.
#' @export

setGeneric(name = "getHistory",
           def = function(x, ...){
             standardGeneric("getHistory")
           })
