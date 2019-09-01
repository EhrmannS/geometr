# geometR <a href='https://ehrmanns.github.io/geometr/'><img src='man/img/geometr.svg' align="right" height="139" /></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/geometr)](https://cran.r-project.org/package=geometr)
[![Travis-CI Build
Status](https://travis-ci.org/EhrmannS/geometr.svg?branch=master)](https://travis-ci.org/EhrmannS/geometr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/EhrmannS/geometr/master.svg)](https://codecov.io/github/EhrmannS/geometr?branch=master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/geometr)](https://cran.rstudio.com/web/packages/geometr/index.html)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

## Overview

The `geometr` package provides tools that generate and process fully accessible and tidy geometric shapes (of class `geom`). Moreover, it aims to improve interoperability of spatial and other geometric classes. Spatial
classes are typically a collection of geometric shapes (or their vertices) that are accompanied by various metadata (such as attributes and a coordinate reference system). Most spatial classes are thus conceptually quite similar, yet a common standard lacks for accessing features, vertices or the metadata. `Geometr` fills this gap by
providing tools

  - that produce an identical output for the same metadata of different
    classes (via socalled getters) and
  - that use an identical input to write to various classes that
    originally require different input (via socalled setters).

## Installation

1)  Install the development version from github and load it:

<!-- end list -->

``` r
devtools::install_github("EhrmannS/geometr")
library(geometr)
```

2)  The
    [vignette](https://ehrmanns.github.io/geometr/articles/geometr.html)
    gives a more in depth introduction, explains `geometr`s take on
    interoperability and discusses the spatial class `geom` that comes
    with `geometr`.

3)  Have fun being a
    [geometer](https://en.wikipedia.org/wiki/List_of_geometers)\!

## Examples

Create a `geom`

``` r
# ... from other classes
library(sf)
nc_sf <- st_read(system.file("shape/nc.shp", package="sf"))
nc_geom <- gc_geom(input = nc_sf)

# ... or by hand.
library(tibble)
coords <- tibble(x = c(40, 70, 70, 50),
                 y = c(40, 40, 60, 70))
window <- tibble(x = c(0, 80),
                 y = c(0, 80))
aGeom <- gs_polygon(anchor = coords, window = window)

# The "tiny map" shows where the vertices are concentrated.
nc_geom
```

Metadata of different classes can be extracted in interoperable quality
(i.e. the same metadata in the same arrangement).

``` r
getTable(x = nc_sf)
getTable(x = nc_geom, slot = "feat")
```

A `geom` has three attribute tables, one for vertices, one for features
and one for groups of features, all of which can be provided with
ancilliary information ([details on data structure of a
`geom`](https://ehrmanns.github.io/geometr/articles/geometr.html#the-class-geom)).

``` r
getTable(x = nc_geom, slot = "vert")
getTable(x = nc_geom, slot = "group")
```

Groups of features are called *multi\** features in other packages. By
lumping several closed geometric shapes into one multi\* feature, the
separate geometric shapes can’t be attributed with ancilliary
information anymore. In a `geom`, multi\* features are separated into
distinct (simpler) features, while the attributes of multi\* features
can be captured by the *group attribute table*.

``` r
nc_geom2 <- gc_geom(input = nc_sf, group = TRUE)
currituck <- getSubset(x = nc_geom2, gid == 4)
getTable(x = currituck, slot = "feat")
getTable(x = currituck, slot = "group")
```

Visualise a `geom`

``` r
visualise(`North Carolina` = nc_geom)
visualise(`NC - NWBIR74` = nc_geom, fillcol = NWBIR74)
```

A `geom` has the slot `@window`, which contains a reference window, so
to speak. This reference window can be used or modified in many
functions of `geometr`

``` r
visualise(`Currituck` = currituck)
visualise(`Currituck` = currituck, window = tibble(x = c(-75.7, -76.4), y = c(36.6, 36)))
```

Finally, cast a `geom` to another type simply by providing it in
‘anchor’ of the respective type

``` r
library(magrittr)
manyPoints <- gs_point(anchor = currituck) %>% 
   setWindow(to = tibble(x = c(-75.7, -76.4), y = c(36.6, 36)))
visualise(`Currituck - boundary vertices`= manyPoints)
```
