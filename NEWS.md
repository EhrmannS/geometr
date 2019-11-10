# geometr 0.2.0 - grid update (development)

This minor version introduces the geom type 'grid', which is meant to store raster data. It stores as `@point` slot a compact version that gives (in both x and y dimension) the three values origin, number of cells and cell distance/resolution. It stores values in run-length encoding (rle) (if that is smaller than non-compressed) and the `@feature` table can contain any number of columns, which would correspond to layers in a raster. It can store in the `@group` slot an attribute table per layer (column in the feature table).

The following functions have been modified:

- `gc_geom()` now has a method to convert Raster objects to grid geoms.
- `get/setTable(x, slot = "point")` has been moved to `get/setPoints(x)`
- `get/setTable(x, slot = "feat")` has been moved to `get/setFeatures(x)`
- `get/setTable(x, slot = "group")` has been moved to `get/setGroups(x)`
- `getPoints()` for grid geoms extracts the compacted @points slot into the interoperable table this getter is supposed to hand out.
- `getFeatures()` for grid geoms reconstructs fid for points and extracts rle tables, if they exist.
- `makeObject()` is not a generic anymore but is now a function based on getters. That means that any class that has the respective getters defined, can be plotted with `visualise()`
- `getSubset()` has been integrated into the respective getters, i.e., to get a subset of points, use `getPoints(x, fid == 1)`, etc.


# geometr 0.1.0

- initial realease
