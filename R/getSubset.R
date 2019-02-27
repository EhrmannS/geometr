#' Get the subset of a spatial object.
#' @param x object to \code{subset}.
#' @param slot [\code{character(1)}]\cr the slot in which to determine a subset,
#'   either \code{"table"} or \code{"coords"}.
#' @param subset [\code{integerish(.)} | \code{logical(.)} |
#'   \code{character(1)}]\cr coordinates to keep.
#' @name getSubset
#' @rdname getSubset
NULL

#' @rdname getSubset
#' @export
if(!isGeneric("getSubset")){
  setGeneric(name = "getSubset",
             def = function(x, ...){
               standardGeneric("getSubset")
             }
  )
}

#' @rdname getSubset
#' @export
setMethod(f = "getSubset",
          signature = signature("geom"),
          definition = function(x, ..., slot = "table"){
            assertCharacter(x = slot, len = 1, any.missing = FALSE)
            assertChoice(x = slot, choices = c("table", "coords"))
            subset <- dots(...)
            if(slot == "table"){
              matches <- eval(parse(text = subset), envir = x@attr)
              x@attr <- x@attr[matches,]
              x@coords <- x@coords[x@coords$fid %in% x@attr$fid,]
            } else{
              matches <- eval(parse(text = subset), envir = x@coords)
              x@coords <- x@coords[matches,]
              x@attr <- x@attr[x@attr$fid %in% x@coords$fid,]
            }
            nVids <- sapply(unique(x@coords$fid), function(i){
              length(x@coords$vid[x@coords$fid == i])
            })
            x@attr$n <- nVids

            return(x)
          }
)

#' @rdname getSubset
#' @export
setMethod(f = "getSubset",
          signature = signature("Spatial"),
          definition = function(x, ...){
            # if(!missing(attr)){
            #   if(is.logical(attr)){
            #     stopifnot(dim(x)[1] == length(attr))
            #     matches <- attr
            #   } else if(is.numeric(attr)){
            #     matches <- attr
            #   } else if(is.character(attr)){
            #     matches <- eval(parse(text = attr), envir = x)
            #   }
            #   x <- x[matches,]
            # }
            return(x)
          }
)

#' @rdname getSubset
#' @examples
#'
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' (aSfc <- getSubset(nc, attr = "NAME %in% c('Ashe', 'Surry')"))
#' @export
setMethod(f = "getSubset",
          signature = signature("sf"),
          definition = function(x, ...){
            subset <- dots(...)
            matches <- eval(parse(text = subset), envir = x)
            x <- x[matches,]
            return(x)
          })