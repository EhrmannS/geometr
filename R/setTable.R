#' Set the attribute table of a spatial object.
#' @param x the object to which to assign \code{table}.
#' @param table [\code{data.frame(.)}]\cr the attribute table.
#' @details If \code{table} does not have columns in common with \code{x}, the
#'   new columns are simply bound to the original attribute table. If there are
#'   common coloums, they are joined.
#'
#'   If a \code{geom} gets assigned a table with individual values per feature
#'   (\code{fid}), and not per group (\code{gid}), groups are by default
#'   reassigned per individual value. This is neccessary as \code{geom}s can
#'   have individual values per feature. An \code{sf} can consist of MULTI*
#'   features, where it is not possible to store feature-specific information,
#'   hence \code{regroup = TRUE} asserts that features with individual values
#'   are not cast to a MULTI* feature - and thereby lose the information they
#'   carry - by modifying \code{gid} accordingly.
#'
#' @name setTable
#' @rdname setTable
NULL

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

#' @rdname setTable
#' @param regroup [\code{logical(1)}]\cr rearrange groups when new attributes
#'   are not unique per group but per feature (\code{TRUE}, default) or
#'   suppress this behaviour (\code{FALSE}).
#' @examples
#' # set table of a geom
#' # individual attributes per point/line/polygon feature
#' getTable(gtGeoms$point)
#' x1 <- setTable(x = gtGeoms$point,
#'                table = data.frame(attr = letters[c(1:5, 5:11)]))
#' getTable(x1)
#' x2 <- setTable(x = gtGeoms$point,
#'                table = data.frame(gid = c(1:3), attr = letters[1:3]))
#' getTable(x2)
#' @importFrom dplyr left_join select everything
#' @importFrom tibble tibble
#' @export
setMethod(f = "setTable",
          signature = "geom",
          definition = function(x, table, regroup = TRUE){
            assertDataFrame(table)
            attr <- table[!names(table) %in% c("gid", "fid")]
            if(any(colnames(table) %in% colnames(x@attr))){
              x@attr <- left_join(x@attr, table)
            } else{
              x@attr <- bind_cols(x@attr, table)
            }
            if(regroup){
              # regroup if there are individual values per feature
              if(dim(unique(attr))[1] != length(unique(x@attr$gid))){
                names <- colnames(attr)
                newGid <- tibble(seq_along(unique(attr[,1])), unique(attr[,1]))
                colnames(newGid) <- c("gid", names)
                x@attr <- left_join(x@attr[!names(x@attr) %in% c("gid")], newGid)
                x@attr <- select(x@attr, "fid", "gid", everything())
              }
            }

            return(x)
          }
)

#' @rdname setTable
#' @importFrom dplyr left_join bind_cols
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom sp SpatialPointsDataFrame SpatialPixelsDataFrame
#'   SpatialMultiPointsDataFrame SpatialLinesDataFrame SpatialPolygonsDataFrame
#' @export
setMethod(f = "setTable",
          signature = "Spatial",
          definition = function(x, table){
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
          definition = function(x, table){
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

#' @rdname setTable
#' @examples
#'
#' # set table to an sfc (no join possible)
#' sfcObj = gtSF$polygon$geometry
#' setTable(x = sfcObj, table = myAttributes)
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom sf st_sf
#' @importFrom dplyr left_join bind_cols
#' @export
setMethod(f = "setTable",
          signature = signature("sfc"),
          definition = function(x, table){
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

#' @rdname setTable
#' @importFrom raster ratify
#' @importFrom checkmate assertDataFrame
#' @export
setMethod(f = "setTable",
          signature = "RasterLayer",
          definition = function(x, table){
            assertDataFrame(table)
            temp <- ratify(x)
            nIDs <- length(temp@data@attributes[[1]][,1])
            stopifnot(dim(table)[1] == nIDs)
            temp@data@attributes <- list(table)
            return(temp)
          }
)