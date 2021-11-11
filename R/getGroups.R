#' Get the table of group attributes
#'
#' @param x the object from which to derive the attribute table.
#' @details This table contains at least the column 'gid'.
#'
#'   When this function is called on "ANY" object, it is first tested whether
#'   that object has features (\code{\link{getFeatures}}), from which the groups
#'   can be reconstructed. If this is not the case, \code{NULL} is returned.
#' @return A tibble (or a list of tibbles per layer) of the group attributes of
#'   \code{x}.
#' @family getters
#' @examples
#'
#' getGroups(gtGeoms$polygon)
#'
#' gc_sp(gtGeoms$line) %>%
#'   getGroups()
#'
#' gc_sf(gtGeoms$line) %>%
#'   getGroups()
#'
#' # for gridded objects, groups are unique pixel values, and their
#' # attributes are in the raster attribute table (RAT)
#' gc_raster(gtGeoms$grid$categorical) %>%
#'   getGroups()
#'
#' gc_terra(gtGeoms$grid$categorical) %>%
#'   getGroups()
#' @name getGroups
#' @rdname getGroups
NULL

# generic ----
#' @rdname getGroups
#' @name getGroups
#' @export
setGeneric(name = "getGroups",
           def = function(x){
             standardGeneric("getGroups")
           }
)


# any ----
#' @rdname getGroups
#' @export
setMethod(f = "getGroups",
          signature = "ANY",
          definition = function(x){
            theFeatures <- getFeatures(x = x)
            if(!is.null(theFeatures)){
              tibble(gid = sortUniqueC(theFeatures$gid))
            } else {
              theFeatures
            }
          }
)

# geom ----
#' @rdname getGroups
#' @importFrom tibble as_tibble
#' @export
setMethod(f = "getGroups",
          signature = "geom",
          definition = function(x){

            theType <- getType(x = x)[1]

            if(theType == "grid"){
              theNames <- geometr::getNames(x = x)
              theFeatures <- getFeatures(x)
              theGroups <- x@group

              vals <- unlist(theFeatures["gid"], use.names = FALSE)
              if(is.numeric(vals)){
                sbs <- sortUniqueC(vals)

                if(all(sbs %in% theGroups$gid)){
                  tab <- theGroups[theGroups$gid %in% sbs,]
                } else {
                  tab <- tibble(gid = sbs)
                }
              } else {
                tab <- tibble(gid = integer())
              }

              if(length(theNames) > 1){
                out <- setNames(list(tab), theNames)
              } else {
                out <- tab
              }

            } else {
              out <- x@group
            }

            return(out)
          }
)

# raster ----
#' @rdname getGroups
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getGroups",
          signature = "Raster",
          definition = function(x){
            if(class(x) == "RasterBrick"){
              tab <- x@data@attributes

              out <- lapply(seq_along(tab), function(y){
                t <- tab[[y]]
                if(length(t) == 0){
                  tibble(gid = integer())
                } else {
                  names <- colnames(t[[1]])
                  names[which(names == "id")] <- "gid"
                  temp <- as_tibble(t[[1]])
                  names(temp) <- names
                  temp
                }
              })
              names(out) <- names(x)
            } else {
              out <- NULL
              for(i in 1:dim(x)[3]){
                temp <- x[[i]]@data@attributes
                if(length(temp) != 0){
                  names <- names(temp[[1]])
                  names[which(names == "id")] <- "gid"
                  tab <- as_tibble(temp[[1]])
                  names(tab) <- names
                } else {
                  tab <- tibble(gid = integer())
                }
                if(dim(x)[3] == 1){
                  out <- tab
                } else {
                  out <- c(out, setNames(list(tab), names(x)[i]))
                }
              }
            }
            return(out)
          }
)

# terra ----
#' @rdname getGroups
#' @importFrom terra cats
#' @importFrom tibble tibble as_tibble
#' @export
setMethod(f = "getGroups",
          signature = "SpatRaster",
          definition = function(x){

            temp <- cats(x)

            out <- NULL
            for(i in seq_along(temp)){

              tab <- temp[[i]]
              tab <- as_tibble(tab)
              tab <- tab[!is.na(tab[[names(x)[1]]]),]
              names <- names(tab)
              names[which(names == "ID")] <- "gid"
              names(tab) <- names

              if(dim(x)[3] == 1){
                out <- tab
              } else {
                out <- c(out, setNames(list(tab), names(x)[i]))
              }

            }
            return(out)

          }
)