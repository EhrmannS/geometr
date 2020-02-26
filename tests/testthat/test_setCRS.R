context("setCRS")


test_that("setCRS of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords, window = window)
  output <- setCRS(x = aGeom, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

  expect_class(output, classes = "geom")
  expect_character(getCRS(output), any.missing = FALSE, pattern = "+proj=laea", len = 1)

  anSpGeom <- setCRS(x = aGeom, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  output <- setCRS(x = anSpGeom, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  expect_class(output, classes = "geom")
  expect_character(getCRS(output), any.missing = FALSE, pattern = "+proj=longlat", len = 1)

  output <- setCRS(x = output, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  expect_class(output, classes = "geom")
  expect_character(getCRS(output), any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("setCRS of a Spatial object", {
  x = c(1, 2, 3, 4, 5)
  y = c(3, 2, 5, 1, 4)
  aSpatial <- SpatialPoints(cbind(x, y))

  # setting a CRS on a Spatial* that hasn't had one before
  output <- setCRS(x = aSpatial, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  expect_class(output, classes = "SpatialPoints")
  expect_character(proj4string(output), any.missing = FALSE, pattern = "+proj=laea", len = 1)

  # setting a CRS on a Spatial* that had one before
  output <- setCRS(x = output, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  expect_class(output, classes = "SpatialPoints")
  expect_character(proj4string(output), any.missing = FALSE, pattern = "+proj=longlat", len = 1)
})

test_that("setCRS of an sf object", {
  input <- gtSF$polygon

  # setting a CRS on a sf that hasn't had one before
  output <- setCRS(x = input, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  expect_class(output, classes = "sf")
  expect_character(st_crs(output)$proj4string, any.missing = FALSE, pattern = "+proj=laea", len = 1)

  # setting a CRS on a sf that had one before
  output <- setCRS(x = output, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  expect_class(output, classes = "sf")
  expect_character(st_crs(output)$proj4string, any.missing = FALSE, pattern = "+proj=longlat", len = 1)
})

test_that("setCRS of a Raster", {
  aRaster <- raster(xmn=-110, xmx=-90, ymn=40, ymx=60, ncols=40, nrows=40)
  aRaster[] <- 1:ncell(aRaster)
  theCRS <- projection(aRaster)
  crs(aRaster) <- as.character(NA)

  # test when crs is missing
  output <- setCRS(x = aRaster, crs = theCRS)
  expect_class(crs(output), classes = "CRS")
  expect_character(crs(output)@projargs, any.missing = FALSE, pattern = "+proj=longlat", len = 1)

  # test to reproject an existing crs
  output <- setCRS(x = output, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  expect_class(crs(output), classes = "CRS")
  expect_character(crs(output)@projargs, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})
