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
#' following the sequence of their order and the line thus closes in on itself.}
#'
#' The data model for storing vertices follows the spaghetti model. Vertices are
#' stored as a sequence of x and y values, additionally characterised with two
#' IDs. The feature ID relates coordinates to features and thus common
#' attributes and the vertex ID enumerates all vertices per feature. Points and
#' Lines are implemented straightforward in this model, but polygons, which may
#' contain holes, are a bit trickier. In \code{geometr} they are implemented as
#' follows: \enumerate{ \item All vertices with the same \code{fid} make up one
#' polygon, irrespective of it containing holes or not. \item The outer
#' path/ring of a polygon is composed of all vertices until a duplicated of its
#' first vertex occurs. This signals that all following vertices are part of
#' another path/ring, which is a hole when it is inside the polygon and which
#' consists of all vertices until a duplicate of it's first vertex occurs. \item
#' This repeats until all vertices of the feature are processed.}
#'
#' Moreover, a \code{geom} does not have the slot \emph{extent}, which
#' characterises the minimum and maximum value of the vertex coordinates and
#' which is thus derived "on the fly" from the vertices. Instead it has a
#' \emph{reference window}, which is sort of a second extent that may be bigger
#' (or smaller) than \code{extent} and which determines the relative position of
#' the vertices when plotting.
#'
#' @slot type [\code{character(1)}]\cr the type of feature, either
#'   \code{"point"}, \code{"line"} or \code{"polygon"}.
#' @slot vert [\code{data.frame(1)}]\cr the \code{fid} (feature ID), \code{vid}
#'   (vertex ID), \code{x} and \code{y} coordinates per vertex and optional
#'   arbitrary coordinate attributes.
#' @slot attr [\code{data.frame(1)}]\cr \code{fid} (feature ID), \code{gid}
#'   (group ID) and optional arbitrary feature attributes.
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
                           vert = "data.frame",
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
#     assertNames(names(geom), identical.to = c("vert", "extent", "type"))
#     assertNames(names(geom$vert), permutation.of = c("x", "y", "id"))
#     assertNames(names(geom$extent), permutation.of = c("x", "y"))
#     assertSubset(geom$type, choices = c("point", "points", "line", "lines", "polygon", "polygons"))
#   }
# })

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
            cat("features   : ", length(unique(object@vert$fid)), "  (", length(object@vert$fid), " vertices)\n", sep = "")
            cat("window     : ", min(object@window$x), ", ", max(object@window$x), ", ", min(object@window$y), ", ", max(object@window$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("extent     : ", min(object@vert$x), ", ", max(object@vert$x), ", ", min(object@vert$y), ", ", max(object@vert$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("scale      : ", object@scale, "\n", sep = "")
            cat("crs        : ", object@crs, "\n", sep = "")
            cat("attributes : ", attribs, "  (",
                ifelse(attribs < 9,
                       paste0(names(object@attr)[!names(object@attr) %in% c("x", "y")], collapse = ", "),
                       paste0(c(head(names(object@attr)[!names(object@attr) %in% c("x", "y")], 9), "..."), collapse = ", ")
                ), ")\n", sep = "")
          }
)

# Determin number of vertices
#
# @param x [\code{geom}]\cr object from which to determine \code{length}.

# setMethod(f = "length",
#           signature = signature("geom"),
#           definition = function(x){
#             dim(x@vert)[1]
#           }
# )

# setMethod(f = "head",
#           signature = signature("geom"),
#           definition = function(x, n){
# # subset all tables to cover only the first n vertices
#           }
# )
#
# setMethod(f = "tail",
#           signature = signature("geom"),
#           definition = function(x, n){
# # subset all tables to cover only the last n vertices
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
# setMethod(f = "dropVertex",
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