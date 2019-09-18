#' geometr: Generate and modify interoperable geometric shapes
#'
#' The geometr package provides tools that generate and process fully
#' accessible and tidy geometric shapes (of class \code{geom}). Moreover, it aims
#' to improve interoperability of spatial and other geometric classes. Spatial
#' classes are typically a collection of geometric shapes (or their vertices)
#' that are accompanied by various metadata (such as attributes and a coordinate
#' reference system). Most spatial classes are thus conceptually quite similar,
#' yet a common standard lacks for accessing features, vertices or the metadata.
#' Geometr fills this gap by providing tools that have a unified interface and
#' that produce an identical output (socalled getters), irrespective of the
#' treated class or that use an indentical input to write to various classes
#' that require different input (socalled setters).
#'
#' @author \strong{Maintainer, Author}: Steffen Ehrmann
#'   \email{steffen.ehrmann@idiv.de}
#'
#' @seealso \itemize{ \item Github project:
#' \href{https://github.com/EhrmannS/geometr}{https://github.com/EhrmannS/geometr}
#' \item Report bugs:
#' \href{https://github.com/EhrmannS/geometr/issues}{https://github.com/EhrmannS/geometr/issues}
#' }
#'
#' @examples
#' library(sf)
#'
#' # A geom can be created from other classes ...
#' nc_sf <- st_read(system.file("shape/nc.shp", package="sf"))
#' nc_geom <- gc_geom(input = nc_sf)
#'
#' # ... or by hand.
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- gs_polygon(anchor = coords, window = window)
#'
#' # The "tiny map" shows where the vertices are concentrated.
#' nc_geom
#'
#' # Information, such as the attribute table, can be extracted from the object
#' # in interoperable quality (i.e. same arrangement of the same information).
#' attr_sf <- getTable(x = nc_sf)
#' attr_geom <- getTable(x = nc_geom, slot = "feature")
#'
#' # However, a `geom` has three attribute tables, one for vertices, one for
#' # features and one for groups of features. All of them (and not only
#' # features) can be filled with ancilliary information.
#' getTable(x = nc_geom, slot = "point")
#' getTable(x = nc_geom, slot = "group")
#'
#' # Groups of features are called *multi* features in the simple features
#' # standard. By lumping several closed geometric shapes into one multi\*
#' # feature, the separate geometric shapes can't be attributed with ancilliary
#' # information anymore. In a `geom`, multi\* features are separated into
#' # distinc (simpler) features. The equivalent attributes of a multi\* feature
#' # are captured in the 'groups attribute table'
#' currituck <- getSubset(x = nc_geom, gid == 4)
#' getTable(x = currituck, slot = "feature")
#' getTable(x = currituck, slot = "group")
#'
#' # A geom can be visualised ...
#' visualise(`North Carolina` = nc_geom)
#' visualise(`NC - NWBIR74` = nc_geom, fillcol = NWBIR74)
#'
#' # ... and it can be cast into another type simply by providing it in
#' # 'anchor' of the respective type.
#' bndPoints <- gs_point(anchor = nc_geom)
#' visualise(`NC - boundary vertices` = bndPoints)
#'
#' @docType package
#' @name geometr
NULL