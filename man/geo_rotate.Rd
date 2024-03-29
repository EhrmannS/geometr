% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_rotate.R
\name{geo_rotate}
\alias{geo_rotate}
\title{Rotate geometric objects}
\usage{
geo_rotate(obj, x = NULL, y = NULL, angle = NULL, fid = NULL, update = TRUE)
}
\arguments{
\item{obj}{\link[=geom]{gridded(1)}\cr the object to rotate.}

\item{x}{\link[=numeric]{numeric(1)}\cr the x position(s) to rotate about.}

\item{y}{\link[=numeric]{numeric(1)}\cr the y position(s) to rotate about.}

\item{angle}{\link[=numeric]{numeric(1)}\cr the counter-clockwise angle(s) by which
\code{obj} shall be rotated (can be negative to rotate clockwise).}

\item{fid}{\link[=integer]{integerish(.)}\cr in case only a subset of features shall
be rotated, specify that here.}

\item{update}{\link[=logical]{logical(1)}\cr whether or not to update the window slot
of the resulting geom.}
}
\value{
\code{geom} of the rotated \code{obj}.
}
\description{
Rotate geometric objects by a certain angle about center coordinates
}
\examples{
# rotate all geoms
geo_vis(gtGeoms$polygon, linewidth = 3)
newPoly <- geo_rotate(obj = gtGeoms$polygon, x = 0, y = 0, angle = 135,
                      update = FALSE)
geo_vis(geom = newPoly, linecol = "green", new = FALSE)

# rotate a single geom
geo_vis(gtGeoms$polygon, linewidth = 3)
newPoly <- geo_rotate(obj = gtGeoms$polygon, x = -10, y = 0, angle = -180,
                      update = FALSE, fid = 2)
geo_vis(geom = newPoly, linecol = "green", new = FALSE)

# rotate different geoms about different centers by different angles
geo_vis(gtGeoms$polygon, linewidth = 3)
newPoly <- geo_rotate(obj = gtGeoms$polygon,
                      x = c(0, -10),
                      y = c(-10, 0),
                      angle = c(75, -135),
                      update = FALSE)
geo_vis(geom = newPoly, linecol = "green", new = FALSE)
}
\seealso{
Other geometry tools: 
\code{\link{geo_filter}()},
\code{\link{geo_locate}()},
\code{\link{geo_pull}()},
\code{\link{geo_reflect}()},
\code{\link{geo_scale}()},
\code{\link{geo_skew}()},
\code{\link{geo_stretch}()},
\code{\link{geo_translate}()}
}
\concept{geometry tools}
