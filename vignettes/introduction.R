## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.width=7, out.width='100%'--------------------------------------
# library(geometr)
# library(tibble)
# 
# coords <- tibble(x = c(40, 70, 70, 50),
#                  y = c(40, 40, 60, 70),
#                  fid = 1)
# (aGeom <- gs_polygon(anchor = coords))
# visualise(geom = aGeom)

## ------------------------------------------------------------------------
# str(aGeom)

## ---- fig.width=7, out.width='100%'--------------------------------------
# window <- tibble(x = c(0, 80),
#                  y = c(0, 80))
# 
# aGeom <- setWindow(x = aGeom, to = window)
# visualise(geom = aGeom)

## ---- fig.width=7, out.width='100%'--------------------------------------
# visualise(geom = gtGeoms$polygon, fillcol = fid)
# visualise(geom = gtGeoms$line, linecol = "green", new = FALSE)
# visualise(geom = gtGeoms$point, linecol = "deeppink", new = FALSE)

