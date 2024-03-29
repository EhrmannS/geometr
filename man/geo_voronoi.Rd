% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_voronoi.R
\name{geo_voronoi}
\alias{geo_voronoi}
\title{Create a voronoi tiling \code{geom}}
\usage{
geo_voronoi(crds = NULL, window = NULL, features = 3)
}
\arguments{
\item{crds}{\link[=data.frame]{data.frame(2)}\cr Coordinates to build the
\code{geom} from. It must include the column names \code{x} and \code{y}.}

\item{window}{\link[=data.frame]{data.frame(2)}\cr in case the reference window
deviates from the bounding box of \code{crds}, specify here the minimum and
maximum values in columns \code{x} and \code{y}.}

\item{features}{\link[=integer]{integerish(1)}\cr number of tiles to sketch.}
}
\value{
A \code{geom}.
}
\description{
Create a voronoi tiling \code{geom}
}
\examples{
# 1. create voronoi polygons programmatically
coords <- data.frame(x = c(40, 70, 70, 50),
                     y = c(40, 40, 60, 70))
window <- data.frame(x = c(0, 80),
                     y = c(0, 80))
aGeom <- geo_point(crds = coords, window = window)
geo_vis(voronoi = aGeom, linecol = "deeppink")

tiles <- geo_voronoi(crds = coords, window = window)
geo_vis(tiles, new = FALSE)

# 2. sketch a voronoi polygon
if(dev.interactive()){
  tiles <- geo_voronoi()
  geo_vis(tiles, new = FALSE)
}
}
\seealso{
Other tilings: 
\code{\link{geo_tiles}()}
}
\concept{tilings}
