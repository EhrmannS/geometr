#' Set the attribute table(s) of a spatial object.
#'
#' @param x the object to which to assign an attribute table.
#' @param table [\code{data.frame(.)}]\cr the attribute table.
#' @details If \code{table} does not have columns in common with \code{x}, the
#'   new columns are simply bound to the original attribute table (if possible).
#'   If there are common columns, they are joined.
#' @return The object \code{x} with an updated attribute table.
#' @family setters
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
#'   set the attribute table, either \code{"point"}, \code{"feature"} or
#'   \code{"group"}.
#' @examples
#' # set table of a geom
#' # individual attributes per point/line/polygon feature
#' getTable(gtGeoms$point)
#' newAttr <- setTable(x = gtGeoms$point,
#'                     slot = "point",
#'                     table = data.frame(attr = letters[c(1:12)]))
#' getTable(x = newAttr, slot = "point")
#'
#' newAttr2 <- setTable(x = newAttr,
#'                      slot = "feature",
#'                      table = data.frame(gid = c(1:3), attr = letters[1:3]))
#' getTable(x = newAttr2, slot = "feature")
#' newAttr2
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "setTable",
          signature = "geom",
          definition = function(x, table = NULL, slot = "feature"){
            assertDataFrame(x = table)
            assertChoice(x = slot, choices = c("point", "feature", "group"))
            if(slot == "point"){
              if(any(colnames(table) %in% colnames(x@point))){
                temp <- merge(x@point, table, all.x = TRUE)
                temp <- .updateOrder(input = temp)
              } else{
                temp <- cbind(x@point, table)
              }
              x@point <- as_tibble(temp)
            } else if(slot == "feature"){
              if(any(colnames(table) %in% colnames(x@feature))){
                temp <- merge(x@feature, table, all.x = TRUE)
                temp <- .updateOrder(input = temp)
              } else{
                temp <- cbind(x@feature, table)
              }
              x@feature <- as_tibble(temp)
            } else {
              if(any(colnames(table) %in% colnames(x@group))){
                temp <- merge(x@group, table, all.x = TRUE)
                temp <- .updateOrder(input = temp)
              } else{
                temp <- cbind(x@group, table)
              }
              x@group <- as_tibble(temp)
            }

            cln <- colnames(table)
            if(length(cln) > 1){
              hist <- paste0("the ", slot, " attribute table was joined with the variables (", paste(cln, collapse = ", "), ")")
            } else {
              hist <- paste0("the ", slot, " attribute table was joined with the variable ", cln)
            }
            x@history <- c(getHistory(x = x), list())

            return(x)
          }
)

# Spatial ----
#' @rdname setTable
#' @examples
#'
#' # set table of an Spatial object
#' spObj <- gtSP$SpatialPolygonsDataFrame
#'
#' # ... with common columns
#' myAttributes <- data.frame(a = c(2, 1), attr = letters[1:2])
#' setTable(x = spObj, table = myAttributes)
#'
#' # ... without common columns
#' setTable(x = spObj, table = myAttributes[2])
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
                x@data <- merge(x@data, table, all.x = TRUE)
              } else{
                x@data <- cbind(x@data, table)
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
#' @export
setMethod(f = "setTable",
          signature = signature("sf"),
          definition = function(x, table = NULL){
            assertDataFrame(table)
            assertTRUE(nrow(x) == nrow(table))
            if(any(colnames(table) %in% colnames(x))){
              out <- merge(x = x, y = table, all.x = TRUE)
            } else{
              temp <- x
              st_geometry(temp) <- NULL
              temp <- cbind(temp, table)
              out <- merge(x = x, y = temp, all.x = TRUE)
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
#' @export
setMethod(f = "setTable",
          signature = signature("sfc"),
          definition = function(x, table = NULL){
            assertDataFrame(table)

            out <- st_sf(geom = x, table)
            return(out)
          }
)

# ppp ----
#' @rdname setTable
#' @examples
#'
#' table <- data.frame(attr = LETTERS[1:15],
#'                     colour = topo.colors(15))
#' setTable(gtPPP, table = table)
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