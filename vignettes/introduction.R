## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.width=7, out.width='100%'--------------------------------------
library(geometr)
library(tibble)

coords <- tibble(x = c(40, 70, 70, 50),
                 y = c(40, 40, 60, 70),
                 fid = 1)
(aGeom <- gs_polygon(anchor = coords))
visualise(geom = aGeom)

## ------------------------------------------------------------------------
str(aGeom)

## ---- fig.width=7, out.width='100%'--------------------------------------
window <- tibble(x = c(0, 80),
                 y = c(0, 80))

aGeom <- setWindow(x = aGeom, to = window)
visualise(geom = aGeom)

## ---- fig.width=7, out.width='100%'--------------------------------------
visualise(geom = gtGeoms$polygon, fillcol = fid)
visualise(geom = gtGeoms$line, linecol = "green", new = FALSE)
visualise(geom = gtGeoms$point, linecol = "deeppink", new = FALSE)

## ------------------------------------------------------------------------
# Get properties of a geom
getVertices(x = gtGeoms$polygon)
getTable(x = gtGeoms$polygon)
getWindow(x = gtGeoms$polygon)
getCRS(x = gtGeoms$polygon) # does not contain any value and is thus only valid in the cartesian coordinate system
getHistory(x = gtGeoms$polygon)

# Set properties of a geom ...
# ... such as feature attributes
newAttrs <- tibble(fid = c(2, 1), weight = c(10, 5))
newGeom <- setTable(x = gtGeoms$polygon, table = newAttrs)
getTable(x = newGeom)
visualise(geom = newGeom, fillcol = weight)

## ------------------------------------------------------------------------
getVertices(gtGeoms$point)
getVertices(gtSP$SpatialMultiPoints)
getVertices(gtSF$multipoint)

## ------------------------------------------------------------------------


## ---- fig.width=7, out.width='100%'--------------------------------------
library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))

visualise(geom = gt_sf(nc), fillcol = NWBIR74)

## ------------------------------------------------------------------------

coords <- tibble(x = c(30, 60, 60, 40, 10, 40, 20),
                 y = c(40, 40, 60, 70, 10, 20, 40),
                 fid = c(1, 1, 1, 1, 2, 2, 2))

twoGeoms <- gs_polygon(anchor = coords, window = window)
visualise(geom = twoGeoms)

rotatedGeoms <- gt_rotate(geom = twoGeoms, angle = 90, about = c(40, 40))
visualise(geom = rotatedGeoms, new = FALSE)

