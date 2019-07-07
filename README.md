[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geometr)](https://cran.r-project.org/package=geometr)
[![Travis-CI Build Status](https://travis-ci.org/EhrmannS/geometr.svg?branch=master)](https://travis-ci.org/EhrmannS/geometr)
[![Coverage Status](https://img.shields.io/codecov/c/github/EhrmannS/geometr/master.svg)](https://codecov.io/github/EhrmannS/geometr?branch=master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/geometr)](https://cran.rstudio.com/web/packages/geometr/index.html)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

# geometr

***Generate and Process Geometric Shapes***

The `geometr` package provides tools that generate and process fully accessible and tidy geometric shapes (of class `geom`). Moreover, it aims to improve interoperability of spatial and other geometric classes. Those classes are typically a collection of geometric shapes, or vertices that outline those shapes, which are accompanied by various meta data. The classes are conceptually quite similar, yet they lack a common standard in providing access to the objects, their vetrices or the meta data. `Geometr` fills this gap by providing tools that have a unified interface and that produce an identical output, irrespective of the handled class (socalled getters) or that use an indentical input to write to various classes that originally require different input (socalled setters).


## Installation

1) Install the development version from github and load it:

        devtools::install_github("EhrmannS/geometr")
        library(geometr)

2) The [vignette](articles/geometr.html) gives a more in depth introduction, explains `geometr`s take on interoperability and discusses the spatial class `geom` that comes with `geometr`.

3) Have fun being a [geometer](https://en.wikipedia.org/wiki/List_of_geometers)!


## Examples

An object of class `geom` can be created from other classes ...

    library(sf)
    nc_sf <- st_read(system.file("shape/nc.shp", package="sf"))
    nc_geom <- gc_geom(input = nc_sf)

... or by hand

    coords <- data.frame(x = c(40, 70, 70, 50),
                         y = c(40, 40, 60, 70))
    window <- data.frame(x = c(0, 80),
                         y = c(0, 80))
    aGeom <- gs_polygon(anchor = coords, window = window)

The "tiny map" shows where the vertices are concentrated
    
    nc_geom

Information, such as the attribute table, can be extracted from the object in interoperable quality (i.e. same arrangement of the same information)

    attr_sf <- getTable(x = nc_sf)
    attr_geom <- getTable(x = nc_geom, slot = "feat")
    
However, a `geom` has three attribute tables, one for vertices, one for features and one for groups of features. All of them (and not only features) can be filled with respective ancilliary information.

    getTable(x = nc_geom, slot = "vert")
    getTable(x = nc_geom, slot = "group")
    
Groups of features are called *multi\** features in other packages. By lumping several isolated geometric shapes into one multi\* feature, the separate geometries can't be attributed with ancilliary information anymore. In a `geom`, multi\* features are separated into distinct (simpler) features (that may contain holes), while the multi\* information is preserved in the variable `gid`.

    currituck <- getSubset(x = nc_geom, gid == 4)
    getTable(x = currituck, slot = "feat")
    getTable(x = currituck, slot = "group")

A geom can be visualised ...

    visualise(`North Carolina` = nc_geom)

    visualise(`NC - NWBIR74` = nc_geom, fillcol = NWBIR74)

And it can be cast into another type simply by providing it in 'anchor'

    manyPoints <- gs_point(anchor = nc_geom)






