#' Set the attribute table(s) of a spatial object.
#'
#' @param x the object to which to assign an attribute table.
#' @param table [\code{data.frame(.)}]\cr the attribute table.
#' @details If \code{table} does not have columns in common with \code{x}, the
#'   new columns are simply bound to the original attribute table (if possible).
#'   If there are common columns, they are joined.
#' @return The object \code{x} with an updated attribute table.
#' @name setTable
#' @rdname setTable
NULL

# generic ----
#' @rdname setTable
#' @name setTable
#' @export
if(!isGeneric("setTable")){
  setGeneric(name = "setTable",
             def = function(x, table, ...){
               standardGeneric("setTable")
             }
  )
}

# geom ----
#' @rdname setTable
#' @param slot [\code{character(1)}]\cr the slot (of \code{geom}) for which to
#'   set the attribute table, either \code{"vert"}, \code{"feat"} or
#'   \code{"group"}.
#' @examples
#' # set table of a geom
#' # individual attributes per point/line/polygon feature
#' getTable(gtGeoms$point)
#' newAttr <- setTable(x = gtGeoms$point,
#'                     slot = "vert",
#'                     table = data.frame(attr = letters[c(1:12)]))
#' getTable(x = newAttr, slot = "vert")
#'
#' newAttr2 <- setTable(x = newAttr,
#'                      slot = "feat",
#'                      table = data.frame(gid = c(1:3), attr = letters[1:3]))
#' getTable(x = newAttr2, slot = "feat")
#' newAttr2
#' @importFrom dplyr left_join select everything
#' @importFrom tibble tibble
#' @export
setMethod(f = "setTable",
          signature = "geom",
          definition = function(x, table = NULL, slot = "feat"){
            assertDataFrame(x = table)
            assertChoice(x = slot, choices = c("vert", "feat", "group"))
            if(slot == "vert"){
              if(any(colnames(table) %in% colnames(x@vert))){
                x@vert <- left_join(x@vert, table)
              } else{
                x@vert <- bind_cols(x@vert, table)
              }
            } else if(slot == "feat"){
              if(any(colnames(table) %in% colnames(x@feat))){
                x@feat <- left_join(x@feat, table)
              } else{
                x@feat <- bind_cols(x@feat, table)
              }
            } else {
              if(any(colnames(table) %in% colnames(x@group))){
                x@group <- left_join(x@group, table)
              } else{
                x@group <- bind_cols(x@group, table)
              }
            }

            return(x)
          }
)

# Spatial ----
#' @rdname setTable
#' @importFrom dplyr left_join bind_cols
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom sp SpatialPointsDataFrame SpatialPixelsDataFrame
#'   SpatialMultiPointsDataFrame SpatialLinesDataFrame SpatialPolygonsDataFrame
#' @export
setMethod(f = "setTable",
          signature = "Spatial",
          definition = function(x, table = NULL){
            assertDataFrame(table)
            assertTRUE(length(x) == dim(table)[1])

            if(grepl("DataFrame", class(x))){
              if(any(colnames(table) %in% colnames(x@data))){
                x@data <- left_join(x@data, table)
              } else{
                x@data <- bind_cols(x@data, table)
              }
              out <- x
            } else{
              if(class(x) == "SpatialPoints"){
                out <- SpatialPointsDataFrame(coords = x, data = table)
              } else if(class(x) == "SpatialPixels"){
                out <- SpatialPixelsDataFrame(points = x, data = table)
              } else if(class(x) == "SpatialMultiPoints"){
                out <- SpatialMultiPointsDataFrame(coords = x, data = table)
              } else if(class(x) == "SpatialLines"){
                out <- SpatialLinesDataFrame(sl = x, data = table, match.ID = FALSE)
              } else if(class(x) == "SpatialPolygons"){
                out <- SpatialPolygonsDataFrame(Sr = x, data = table)
              }
            }
            return(out)
          }
)

# sf ----
#' @rdname setTable
#' @examples
#'
#' # set table of an sf
#' sfObj <- gtSF$polygon
#'
#' # ... with common columns
#' myAttributes <- data.frame(a = c(2, 1), attr = letters[1:2])
#' setTable(x = sfObj, table = myAttributes)
#'
#' # ... without common columns
#' setTable(x = sfObj, table = myAttributes[2])
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom sf st_sf st_geometry
#' @importFrom dplyr left_join bind_cols
#' @export
setMethod(f = "setTable",
          signature = signature("sf"),
          definition = function(x, table = NULL){
            assertDataFrame(table)
            assertTRUE(nrow(x) == nrow(table))
            if(any(colnames(table) %in% colnames(x))){
              out <- left_join(x, table)
            } else{
              out <- st_sf(bind_cols(table, x))
            }
            return(out)
          }
)

# sfc ----
#' @rdname setTable
#' @examples
#'
#' # set table to an sfc (no join possible) and transform it to sf thereby
#' sfcObj = gtSF$polygon$geometry
#' setTable(x = sfcObj, table = myAttributes)
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom sf st_sf
#' @importFrom dplyr left_join bind_cols
#' @export
setMethod(f = "setTable",
          signature = signature("sfc"),
          definition = function(x, table = NULL){
            assertDataFrame(table)
            temp <- st_sf(geom = x)
            assertTRUE(nrow(temp) == nrow(table))
            if(any(colnames(table) %in% colnames(temp))){
              out <- left_join(temp, table)
            } else{
              out <- st_sf(bind_cols(table, temp))
            }
            return(out)
          }
)

# ppp ----
#' @rdname setTable
#' @examples
#'
#' table <- data.frame(attr = LETTERS[1:20],
#'                     colour = topo.colors(20))
#' # setTable(gtPPP$...)
#' @importFrom spatstat ppp
#' @export
setMethod(f = "setTable",
          signature = "ppp",
          definition = function(x, table = NULL){
            assertDataFrame(x = table, nrows = length(x$x))
            temp <- x

            out <- ppp(x = temp$x, y = temp$y,
                       window = temp$window,
                       marks = table)
            return(out)
          }
)

# RasterLayer ----
#' @rdname setTable
#' @importFrom raster ratify
#' @importFrom checkmate assertDataFrame
#' @export
setMethod(f = "setTable",
          signature = "RasterLayer",
          definition = function(x, table = NULL){
            assertDataFrame(x = table)
            temp <- ratify(x)
            nIDs <- length(temp@data@attributes[[1]][,1])
            stopifnot(dim(table)[1] == nIDs)
            temp@data@attributes <- list(table)
            return(temp)
          }
)