#' Example \code{geom} objects
#'
#' A set of two geometries.
#' @format The list contains two objects of class geom, a polygon and a couple
#'   of points. Both are mostly used in the example and test-sections of this
#'   package.
"gtGeoms"

#' Projections which are in use in lomm
#'
#' @format List with 5 elemens. \describe{ \item{\code{laea}}{Lambert azimuthal
#'   equal-area projection,\cr Based on the Geodetic Reference System 1980 (GRS
#'   80) Ellipsoid} \item{\code{longlat}}{Longitude/Latitude "projection",\cr
#'   Based on the World Geodetic System 1985 (WGS 84) Ellipsoid}
#'   \item{\code{utm}}{Universal Transverse Mercator "projection", zone 32,\cr
#'   Based on the World Geodetic System 1985 (WGS 84) Ellipsoid}
#'   \item{\code{sinu}}{Sinusoidal projection,\cr Based on a pseudocylinder}
#'   \item{\code{tmerc}}{Transverse Mercator projection,\cr Based on the Bessel
#'   ellipsoid} }
"projs"

#' Example \code{Spatial} objects
#'
#' A set of four sp objects
#' @format The list contains four objects of class \code{Spatial}, a
#'   \code{SpatialPoints}, a \code{SpatialMultiPoints}, a, \code{SpatialLines}
#'   and a \code{SpatialPolygons} object. They are mostly used in the example
#'   and test-sections of this package.
"gtSP"

#' Example \code{sf} objects
#'
#' A set of six sp objects
#' @format The list contains six objects of class \code{sf}, a \code{POINT}, a
#'   \code{MULTIPOINT}, a \code{LINESTRING}, a \code{MULTILINESTRING}, a
#'   \code{POLYGON}, and a \code{MULTIPOLYGON} object. They are mostly used in
#'   the example and test-sections of this package.
"gtSF"

#' Default visualising theme
#'
"gtTheme"