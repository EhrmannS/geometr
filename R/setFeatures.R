#' Set the table of feature attributes
#'
#' @param x the object to which to assign an attribute table.
#' @param table [\code{data.frame(.)}]\cr the attribute table.
#' @return The object \code{x} with an updated attribute table.
#' @family setters
#' @name setFeatures
#' @rdname setFeatures
NULL

# generic ----
#' @rdname setFeatures
#' @name setFeatures
#' @export
if(!isGeneric("setFeatures")){
  setGeneric(name = "setFeatures",
             def = function(x, table, ...){
               standardGeneric("setFeatures")
             }
  )
}

# any ----
#' @rdname setFeatures
#' @export
setMethod(f = "setFeatures",
          signature = "ANY",
          definition = function(x){
            warning(paste0("I can't set feature attributes to an object of class '", paste0(class(x), collapse = ", "), "'."))
          }
)

# geom ----
#' @rdname setFeatures
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "setFeatures",
          signature = "geom",
          definition = function(x, table = NULL){
            if(!any(names(table) %in% c("fid", "gid"))){
              if(dim(table)[1] != dim(x@point)[1]){
                stop("'table' must either contain the columns 'fid' or 'gid' or be of the same length as 'x'.")
              }
            }
            if(x@type == "grid"){

            } else {
              theFeatures <- getFeatures(x = x)

              if(any(colnames(table) %in% colnames(theFeatures))){
                temp <- merge(theFeatures, table, all.x = TRUE)
                temp <- .updateOrder(input = temp)
              } else{
                temp <- cbind(theFeatures, table)
              }
              x@feature <- list(geometry = as_tibble(temp))
            }

            cln <- colnames(table)
            if(length(cln) > 1){
              hist <- paste0("the 'feature' attribute table was joined with the variables (", paste(cln, collapse = ", "), ")")
            } else {
              hist <- paste0("the 'feature' attribute table was joined with the variable ", cln)
            }
            x@history <- c(getHistory(x = x), list())

            return(x)
          }
)

# Spatial ----
#' @rdname setFeatures
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom sp SpatialPointsDataFrame SpatialPixelsDataFrame
#'   SpatialMultiPointsDataFrame SpatialLinesDataFrame SpatialPolygonsDataFrame
#' @export
setMethod(f = "setFeatures",
          signature = "Spatial",
          definition = function(x, table = NULL){
            assertDataFrame(x = table)

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
#' @rdname setFeatures
#' @importFrom checkmate assertDataFrame assertTRUE
#' @importFrom sf st_sf st_geometry
#' @export
setMethod(f = "setFeatures",
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
#' @rdname setFeatures
#' @importFrom checkmate assertDataFrame
#' @importFrom sf st_sf
#' @export
setMethod(f = "setFeatures",
          signature = signature("sfc"),
          definition = function(x, table = NULL){
            assertDataFrame(table)

            out <- st_sf(geom = x, table)
            return(out)
          }
)

# ppp ----
#' @rdname setFeatures
#' @importFrom spatstat ppp
#' @export
setMethod(f = "setFeatures",
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