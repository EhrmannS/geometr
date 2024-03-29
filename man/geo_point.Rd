% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_point.R
\name{geo_point}
\alias{geo_point}
\alias{as_point}
\title{Create or cast to a point \code{geom}}
\usage{
geo_point(crds = NULL, window = NULL, vertices = 1)

as_point(geom)
}
\arguments{
\item{crds}{\link[=data.frame]{data.frame(2)}\cr Coordinates to build the
\code{geom} from. It must include the column names \code{x} and \code{y}.}

\item{window}{\link[=data.frame]{data.frame(2)}\cr in case the reference window
deviates from the bounding box of \code{crds}, specify here the minimum and
maximum values in columns \code{x} and \code{y}.}

\item{vertices}{\link[=integer]{integerish(1)}\cr if neither \code{crds} nor
\code{window} are given, this indicates how often you can click into a plot
to determine the location of the coordinates manually.}

\item{geom}{\link[=geom]{gridded(1)}\cr the geom to cast to type 'point'.}
}
\value{
A point geom.
}
\description{
Create a point geometry (of class \code{\link{geom}}) either programatically
by specifying coordinate values, by sketching it or by casting to it from
another geom type.
}
\examples{
# 1. create points programmatically
coords <- data.frame(x = c(40, 70, 70, 50),
                     y = c(40, 40, 60, 70))

# if no window is set, the bounding box will be set as window ...
(pointGeom <- geo_point(crds = coords))

# ... otherwise the vertices are plotted relative to the window
window <- data.frame(x = c(0, 80),
                     y = c(0, 80))
points <- geo_point(crds = coords, window = window)

geo_vis(points, linecol = "#FFB000")

# 2. cast to point geom from another type
pointGeom <- as_point(geom = gtGeoms$polygon)

geo_vis(gtGeoms$polygon, linecol = "#FFB000", theme = setTheme(box = list(fillcol = "#282828")))
geo_vis(pointGeom, linecol = "#33FF00", pointsymbol = 5, new = FALSE)

# 3. sketch two points
if(dev.interactive()){
  points <- geo_point(vertices = 2)
  geo_vis(points, linecol = "#B24223", pointsymbol = 22, new = FALSE)
}
}
\seealso{
Other geometry shapes: 
\code{\link{geo_line}()},
\code{\link{geo_polygon}()},
\code{\link{geo_random}()}
}
\concept{geometry shapes}
