% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_polygon.R
\name{geo_polygon}
\alias{geo_polygon}
\alias{geo_triangle}
\alias{geo_square}
\alias{geo_rectangle}
\alias{geo_hexagon}
\alias{as_polygon}
\title{Create a polygon \code{geom}}
\usage{
geo_polygon(
  crds = NULL,
  window = NULL,
  features = 1,
  vertices = 3,
  regular = FALSE
)

geo_triangle(crds = NULL, window = NULL, features = 1)

geo_square(crds = NULL, window = NULL, features = 1)

geo_rectangle(crds = NULL, window = NULL, features = 1)

geo_hexagon(crds = NULL, window = NULL, features = 1)

as_polygon(geom)
}
\arguments{
\item{crds}{\link[=data.frame]{data.frame(2)}\cr Coordinates to build the
\code{geom} from. It must include the column names \code{x} and \code{y}.}

\item{window}{\link[=data.frame]{data.frame(2)}\cr in case the reference window
deviates from the bounding box of \code{crds}, specify here the minimum and
maximum values in columns \code{x} and \code{y}.}

\item{features}{\link[=integer]{integerish(1)}\cr number of polygons to create.}

\item{vertices}{\link[=integer]{integerish(1)}\cr number of vertices per polygon;
will be recycled if it does not have as many elements as specified in
\code{features}.}

\item{regular}{\link[=logical]{logical(1)}\cr should the polygon be regular, i.e.
point symmetric (\code{TRUE}) or should the vertices be selected as
provided by \code{anchor} (\code{FALSE}, default)?}

\item{geom}{\link[=geom]{gridded(1)}\cr the geom to cast to type 'polygon'.}
}
\value{
A \code{geom}.
}
\description{
Create any (regular) polygon geometry (of class \code{\link{geom}}) either by
specifying anchor values or by sketching it.
}
\details{
The argument \code{regular} determines how the vertices provided in
\code{anchor} or via \code{template} are transformed into a polygon:
\itemize{ \item if \code{regular = FALSE} the resulting polygon is created
from all vertices in \code{anchor}, \item if \code{regular = TRUE}, only
the first two vertices are considered, as centre and indicating the
distance to the (outer) radius.}
}
\section{Functions}{
\itemize{
\item \code{geo_triangle()}: wrapper of geo_polygon where \code{vertices = 3} and
\code{regular = TRUE}.

\item \code{geo_square()}: wrapper of geo_polygon where \code{vertices = 4} and
\code{regular = TRUE}.

\item \code{geo_rectangle()}: wrapper of geo_polygon where \code{vertices = 2},
\code{regular = FALSE} and the two complementing corners are derived from
the two given opposing corners.

\item \code{geo_hexagon()}: wrapper of geo_polygon where \code{vertices = 6} and
\code{regular = TRUE}.

}}
\examples{
# 1. create a polygon programmatically
coords <- data.frame(x = c(0, 40, 40, 0),
                     y = c(0, 0, 40, 40))

# if no window is set, the bounding box will be set as window
polyGeom <- geo_polygon(crds = coords)
geo_vis(polyGeom)

# derive a regular polygon from the (first two) coordinates (per feature)
hexaGeom <- geo_polygon(crds = coords, vertices = 6, regular = TRUE)
geo_vis(hexaGeom, linecol = "green")

# 2. cast to point geom from another type
polyGeom <- as_polygon(geom = gtGeoms$point)

geo_vis(gtGeoms$point, linecol = "#FFB000", pointsymbol = 5)
geo_vis(polyGeom, linecol = "#33FF00", new = FALSE)

# 3. sketch a hexagon
if(dev.interactive()){
  aHexagon <- geo_hexagon(features = 1)
  geo_vis(aHexagon, linecol = "#33FF00", linetype = 2, new = FALSE)
}
}
\seealso{
Other geometry shapes: 
\code{\link{geo_line}()},
\code{\link{geo_point}()},
\code{\link{geo_random}()}
}
\concept{geometry shapes}
