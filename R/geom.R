#' Geometry class (S4) and methods
#'
#' A \code{geom} stores the vertices and all additional information that
#' characterise a geometry. A \code{geom} can be spatial, but is not by default.
#' A \code{geom} can either have absolute or relative values, where relative
#' values specify the vertex position relative to the \code{window} slot.
#'
#' A \code{geom} either has the feature type \itemize{ \item \code{"point"},
#' when none of the vertices are connected to other vertices, \item
#' \code{"line"}, where vertices with the same \code{fid} are connected
#' following the sequence of their order, without the line closing in itself and
#' \item \code{"polygon"}, where vertices with the same \code{fid} are connected
#' following the sequence of their order, where the last vertex is identical to
#' the first and the lines thus closes in on itself.}
#'
#' Moreover, a \code{geom} does not have the slot \emph{extent}, which
#' characterises the minimum and maximum value of the vertex coordinates and
#' which is thus derived "on the fly" from the vertices. Instead it has a
#' \emph{reference window}, which is sort of a second extent that may be bigger
#' (or smaller) than \code{extent} and which determines the relative position of
#' the vertices when plotting.
#'
#' @slot type [\code{character(1)}]\cr the type of feature, recently either
#'   \code{"point"}, \code{"line"} or \code{"polygon"}.
#' @slot coords [\code{data.frame(1)}]\cr the \code{vid} (vertex ID), \code{fid}
#'   (feature ID), \code{x} and \code{y} coordinates per vertex and optional
#'   arbitrary coordinate attributes.
#' @slot attr [\code{data.frame(1)}]\cr \code{fid}, \code{n} (number of
#'   vertices) and optional arbitrary feature attributes.
#' @slot window [\code{data.frame(1)}]\cr the minimum and maximum value in x and
#'   y dimension of the reference window in which the \code{geom} dwells.
#' @slot scale [\code{character(1)}]\cr whether the vertex coordinates are
#'   stored as \code{"absolute"} values, or \code{"relative"} to \code{window}.
#' @slot crs [\code{character(1)}]\cr the coordinate reference system in proj4
#'   notation.
#' @slot history [\code{list(.)}]\cr a list of steps taken to derive the
#'   \code{geom} in focus.

geom <- setClass(Class = "geom",
                 slots = c(type = "character",
                           coords = "data.frame",
                           attr = "data.frame",
                           window = "data.frame",
                           scale = "character",
                           crs = "character",
                           history = "list"
                 )
)

# setValidity("geom", function(){

# this must include a warning when for instance a polygon has less than 3 vertices, other such cases?
#
#   if(existsGeom){
#     assertNames(names(geom), identical.to = c("coords", "extent", "type"))
#     assertNames(names(geom$coords), permutation.of = c("x", "y", "id"))
#     assertNames(names(geom$extent), permutation.of = c("x", "y"))
#     assertSubset(geom$type, choices = c("point", "points", "line", "lines", "polygon", "polygons"))
#   }
# })

# setClass("SpatialPolygonsDataFrame",
#          contains = "SpatialPolygons",
#          slots = c(data = "data.frame"),
#          validity = function(object) {
#            if (!inherits(object@data, "data.frame"))
#              stop("data should be of class data.frame")
#            if (nrow(object@data) != length(object@polygons))
#              stop("number of rows in data.frame and polygons in SpatialPolygons don't match")
#            return(TRUE)
#          }
# )

#' Print geom in the console
#'
#' @param object [\code{geom}]\cr object to \code{show}.
#' @importFrom utils head

setMethod(f = "show",
          signature = "geom",
          definition = function(object){
            attribs <- length(object@attr)
            cat("class      : ", class(object), "\n", sep = "")
            cat("type       : ", object@type, "\n", sep = "")
            cat("features   : ", length(unique(object@coords$fid)), "  (", length(object), " vertices)\n", sep = "")
            cat("window     : ", min(object@window$x), ", ", max(object@window$x), ", ", min(object@window$y), ", ", max(object@window$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("extent     : ", min(object@coords$x), ", ", max(object@coords$x), ", ", min(object@coords$y), ", ", max(object@coords$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("scale      : ", object@scale, "\n", sep = "")
            cat("crs        : ", object@crs, "\n", sep = "")
            cat("attributes : ", attribs, "  (",
                ifelse(attribs < 9,
                       paste0(names(object@attr)[!names(object@attr) %in% c("x", "y")], collapse = ", "),
                       paste0(c(head(names(object@attr)[!names(object@attr) %in% c("x", "y")], 9), "..."), collapse = ", ")
                ), ")\n", sep = "")
          }
)

#' Determin number of vertices
#'
#' @param x [\code{geom}]\cr object from which to determine \code{length}.

setMethod(f = "length",
          signature = signature("geom"),
          definition = function(x){
            dim(x@coords)[1]
          }
)

# setMethod(f = "head",
#           signature = signature("geom"),
#           definition = function(x){
#
#           }
# )
#
# setMethod(f = "tail",
#           signature = signature("geom"),
#           definition = function(x){
#
#           }
# )
#
# setMethod(f = "plot",
#           signature = signature("geom"),
#           definition = function(x){
#
#           }
# )
#
# setMethod(f = "as.list",
#           signature = signature("geom"),
#           definition = function(x){
#
#           }
# )
#
# setMethod(f = "dropFeature",
#           signature = signature("geom"),
#           definition = function(x){
#
#           }
# )
#
# setMethod(f = "addFeature",
#           signature = signature("geom"),
#           definition = function(x){
#
#           }
# )
#
# setMethod(f = "adropVertex",
#           signature = signature("geom"),
#           definition = function(x){
#
#           }
# )
#
# setMethod(f = "addVertex",
#           signature = signature("geom"),
#           definition = function(x){
#
#           }
# )