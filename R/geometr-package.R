#' geometr: Generate and Modify Interoperable Geometric Shapes
#'
#' The geometr package provides tools that generate and modify fully accessible
#' and tidy geometric shapes (of class \code{geom}). This class unifies both
#' spatial (vector and raster based) and geometric data into a single,
#' simplified data structure.
#'
#' @author \strong{Maintainer, Author}: Steffen Ehrmann
#'   \email{steffen.ehrmann@posteo.de}
#' @author \strong{Copyright holder} Dan Sunday
#'   \href{http://www.geomalgorithms.com/algorithms.html}{fast point-in-polygon
#'   algorithm}
#'
#' @seealso \itemize{ \item Github project:
#'   \href{https://github.com/EhrmannS/geometr}{https://github.com/EhrmannS/geometr}
#'   \item Report bugs:
#'   \href{https://github.com/EhrmannS/geometr/issues}{https://github.com/EhrmannS/geometr/issues}
#'   }
#' @docType package
#' @name geometr
NULL

globalVariables(c("x", "y", "gtTheme", "fid", "gid", "vid", "is_dup", "is_odd"))