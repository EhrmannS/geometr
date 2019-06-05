## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(geometr)
library(tibble)

## ---- fig.width=7, out.width='100%'--------------------------------------
# library(sf)
# nc <- st_read(system.file("shape/nc.shp", package="sf"))
# 
# visualise(geom = gt_sf(nc), fillcol = NWBIR74)

## ------------------------------------------------------------------------
# coords <- tibble(x = c(30, 60, 60, 40, 10, 40, 20),
#                  y = c(40, 40, 60, 70, 10, 20, 40),
#                  fid = c(1, 1, 1, 1, 2, 2, 2))
# window <- tibble(x = c(0, 80),
#                  y = c(0, 80))
# 
# twoGeoms <- gs_polygon(anchor = coords, window = window)
# visualise(geom = twoGeoms)
# 
# rotatedGeoms <- gt_rotate(geom = twoGeoms, angle = 90, about = c(40, 40))
# visualise(geom = rotatedGeoms, new = FALSE)

