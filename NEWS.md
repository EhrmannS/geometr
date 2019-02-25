# Version 0.6
### New functions

* `oWCLIM()`

# Version 0.7

This update comes with a first set of functions (getters and setters, geom-tools, obtain operators) now supporting also sf objects. This should enable a smoother workflow in conjunction with many of the next-gen spatial packages.

Moreover, raster-management is now carried out largely via gdalUtils. This is also in transition, not all obtain operators are based on it yet. Those that are, deposit the handled rasters in a folder 'rT_output' in rtPaths$root as GeoTiff. The files are named according to the dataseries, the distinguishing dataset variables and the extent.

Both, full sf-integration as well as full gdal-integration will mature during the development of this version.

### New functions

* `oESALC()`
