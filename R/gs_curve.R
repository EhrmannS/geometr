# Sketch a curve
#
# This creates the coordinates to any continuous curve.
# @param anchor [\code{geom} | \code{data.frame(1)}]\cr Object to derive the
#   \code{geom} from. In case of \code{data.frame}, it must include column
#   names \code{x}, \code{y} and optinal variables such as \code{id}; see
#   Examples.
# @param template [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
#   that serves as template to sketch the geometry.
# @param control .
# @param weights .
# @param closed .
# @param show [\code{logical(1)}]\cr in case \code{template} is set, should the
#   geometry be plotted (\code{TRUE}, default) or should it not be plotted
#   (\code{FALSE})?
# @param ... [various]\cr
#   graphical parameter, in case \code{show = TRUE}.
# @details m
# @return An invisible geometry object.
# @family shapes

# gs_curve <- function(anchor = NULL, template = NULL, control, weights = 1, closed = FALSE,
#                       show = TRUE, ...){
#
#   # http://www.antigrain.com/research/bezier_interpolation/
#   # https://en.wikipedia.org/wiki/B%C3%A9zier_curve
#
#   if(missing(template)) stop("please provide a template.")
#   # check that center is vector with length 2
#   # check that radius does not exceed the dim of template
#   if(is.null(names(template))){
#     tempName <- "layer"
#   } else{
#     tempName <- names(template)
#   }
#
#   #   # circleGrob()
#   #
#   #   if(show){
#   #     # raster::plot(template)
#   #   }
#   #
#   #   message("please click first in the center of the ellipse and then on a location through which the perimeter should go.\n")
#   #
#   #   # coords <- locator(n = 2)
#   #   # coords <- cbind(coords$x, coords$y)
#   #
#   #   width <- dist(coords)*2
#   #   height <- width*ratio
#   #
#
#   #
#   #   mask <- SpatialPolygons(list(Polygons(list(Polygon(ellipse)), 1)))
#   # plot(mask, add = T)
#   #   if(show){
#   #     raster::plot(mask, add = TRUE, ...)
#   #   }
#   #   invisible(mask)
#   #
#   #   # this is from spSketch
#   #   # center <- raster::click(obj, n = 1, cell = TRUE, show = FALSE)$cell
#   #   # center <- raster::rasterToPoints(obj, spatial = TRUE)[center,]
#   #   # outer <- unlist(locator(n = 1))
#   #   # circle <- rbind(center@coords, outer)
#   #   # mask <- rgeos::gBuffer(spgeom = center, width = dist(circle))
#
# }
# geomLine <- function(){
#   geomCurve()
# }
# geomCircle <- function(){
#   geomCurve()
# }
# geomEllipse <- function(){
#   geomCurve()
# }