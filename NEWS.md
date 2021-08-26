# geometr 0.3.0 - fast visuals

- revise `visualise` and all its' helpers to make plotting way faster.
- fix a bug that would not allow zooming


# geoemtr 0.2.10

- include `ungroup` argument in gt_pull
- revise crs-management of `sp`

# geoemtr 0.2.9

- revise documentation of `setTheme` in an effort to make it more accessible.
- include rounding of legend tick labels

# geometr 0.2.8

- `gs_point` now allows to create an empty geom that contains only "window" information.
- `visualise` can now plot empty geoms, making use of their "window" information, to create an empty surface for sketching.
- the option to use a `template` has been removed from all `gs_*` functions. The new workflow to sketch geoms is now either via an already open plot, or via a sketching surface (see above changes).
- the window slot now only contains the lower left and upper right corner, instead of each coordinate of the rectangle.
- removed function `gt_sketch`, as it is redundant.

# geometr 0.2.7

- included the `@extent`-slot for faster processing of large objects where it takes long to find min/max values of coordinates.
- `getRes` now returns a vector, instead of a tibble
- `getLayers` now returns all layers in a list (no subsetting more possible)
- `getGroups` now returns for RasterLayers without a raster attribute table (RAT) an empty tibble.
- `gc_geom` now only adds groups to the group attribute table in case the input raster actually contains a RAT.
- print-method of `gtTheme` has been beautified (it now also shows whether an attribute is scaled to some parameter)
- In case an object doesn't have any provenance information, `getHistory` now prints that this object has been loaded from memory.
- all spatstat getters/setters (for class `ppp`) have been removed temporarily.

# geometr 0.2.6

- identifying and setting groups in gridded `geom`s has been refined. Several layers are by default stacked, but it is now also possible to create a list of separate `geom`s per layer with `gc_geom(..., stack = FALSE)`.
- \"images\" are now handled by being able to create a grid geom with hexadecimal values by setting `gc_geom(..., as_hex = TRUE)`.
- subsetting has been moved out of the respective getters into `gt_filter()`, it also includes subsetting x and y values of the 
vertices.
- pulling columns can now be done with `gt_pull()`.
- new getters `getCols()`, `getRows()` and `getNames()`.
- removed the `@scale`-slot from from the geom class, because it is no longer needed.
- `visualise()` has been revised so that it fully supports plotting all geometric classes that have getters defined.
- all previously existing `gt_*()` functions were revised so that they too support all geometric classes that have getters defined.
- some code adaptions that come with those changes.

# geometr 0.2.5

- remove 'tiny map' from print method of grid geom
- allow specification of 'precision' for getRes()

# geometr 0.2.4

- rewrite `setPoints()`, `setFeatures()` and `setGroups()` so that new tables can only be set via joining by the primary key (`fid`, `fid` and  `gid` respectively).


# geometr 0.2.3

- `gtTheme` now contains a separate slot for all information that relate to the colour scale of values and the legend.
- accordingly, all functions that deal with that information were adapted.
- other minor bug-fixes

# geometr 0.2.2

- included a faster method to determine IDs of paths and sub-paths of grobs when plotting.
- `getLayer()` also for sf and Spatial objects.

# geometr 0.2.1

- incl. group information in geom show-method
- implemented more efficient routine for extracting features and points from sf-objects
- when setting a new features/groups table on a geom, the old respective table is overwritten from now on.
- a gtTheme now also supports a "missing colour" that will be assigned to NA-values when a quick-option is used in `visualise()`
- other minor bug-fixes

# geometr 0.2.0 - grid update

This update introduces the geom type 'grid', which is meant to store raster data. It stores as `@point` slot a compact version that gives (in both x and y dimension) the three values origin, number of cells and cell distance/resolution. It stores values in run-length encoding (rle) (if that is smaller than non-compressed). The `@feature` slot of a grid geom can contain any number of tables that would correspond to layers in a raster. It can store in the `@group` slot an attribute table per each of the layers.

The following functions have been modified:

- `gc_geom()` now has a method to convert Raster objects to grid geoms.
- `get/setTable(x, slot = "point")` has been moved to `get/setPoints(x)`
- `get/setTable(x, slot = "feat")` has been moved to `get/setFeatures(x)`
- `get/setTable(x, slot = "group")` has been moved to `get/setGroups(x)`
- `getPoints()` for grid geoms extracts the compacted `@points` slot into the interoperable table this getter is supposed to hand out.
- `getFeatures()` for grid geoms reconstructs fid for points and extracts rle tables, if they exist.
- `makeObject()` is not a generic anymore but is now a function based on getters. That means that any class that has the respective getters defined, can be plotted with `visualise()` (which includes now sf and sp objects).
- `getSubset()` has been integrated into the respective getters, i.e., to get a subset of points, use `getPoints(x, fid == 1)`, etc.

The following functions are new:

- `gc_raster()` to transform a grid geom to an object of class `Raster*`.
- `getRes()` to get the resolution of a gridded object.
- `getLayer()` to extract layers from an object that has layers.

# geometr 0.1.1

- fix vertex checks also for polygons when they are created via `gs_sketch()`.

# geometr 0.1.0

- initial release
