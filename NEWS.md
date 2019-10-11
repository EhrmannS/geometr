# geometr 0.2.0 - grid update

This minor version introduces the geom type 'grid', which is meant to store raster data. It stores as @point slot a compact version that gives (in both x and y dimension) the three values origin, number of cells and cell distance/resolution. It stores values in run-length encoding (rle) (if that is smaller than non-compressed) and the @feature table can contain any number of columns, which would correspond to layers in a raster. It can store in the @group slot an attribute table per layer (column in the feature table).

- gc_geom() now has a method to convert Raster objects to grid geoms.
- getPoints()/getTable(slot = "point") extracts the compact grid-points version into the interoperable table this getter is supposed to hand out.
- getTable(slot = "feature") for grid geoms reconstructs fid for points and extracts rle tables, if they exist
- setTable()                (completely revised to handle the new data structure)

ToDo:
- setters need to be written
- makeObject needs to handle the grid geom differently than other geoms, more like a raster

# geometr 0.1.0

- initial realease
