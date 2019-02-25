## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(geometr)

coords <- data.frame(x = c(40, 70, 70, 50),
                     y = c(40, 40, 60, 70),
                     fid = 1)
window <- data.frame(x = c(0, 80),
                     y = c(0, 80))
#(aGeom <- gs_polygon(anchor = coords, window = window, show = TRUE))

