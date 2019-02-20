#' Get the subset
#'
#' Get the subset of a spatial object.
#' @name getSubset
NULL

#' @rdname getSubset
#' @param x object to \code{subset}.
#' @param attr [\code{integerish(.)} | \code{logical(.)} |
#'   \code{character(1)}]\cr rows of the attribute table to keep.
#' @param coords [\code{integerish(.)} | \code{logical(.)} |
#'   \code{character(1)}]\cr coordinates to keep.
#' @param ... other arguments.
#' @export
if(!isGeneric("getSubset")){
  setGeneric(name = "getSubset",
             def = function(x, attr, coords, ...){
               standardGeneric("getSubset")
             }
  )
}

#' @rdname getSubset
#' @export
setMethod(f = "getSubset",
          signature = signature("geom"),
          definition = function(x, attr, coords){
            if(!missing(attr)){
              if(is.logical(attr)){
                stopifnot(dim(x@attr)[1] == length(attr))
                matches <- attr
              } else if(is.numeric(attr)){
                matches <- attr
              } else if(is.character(attr)){
                matches <- eval(parse(text = attr), envir = x@attr)
              }
              x@attr <- x@attr[matches,]
              x@coords <- x@coords[x@coords$fid %in% x@attr$fid,]
            }
            if(!missing(coords)){
              if(is.logical(coords)){
                stopifnot(dim(x@coords)[1] == length(coords))
                matches <- coords
              } else if(is.numeric(coords)){
                matches <- coords
              } else if(is.character(coords)){
                matches <- eval(parse(text = coords), envir = x@coords)
              }
              x@coords <- x@coords[matches,]
              x@attr <- x@attr[x@attr$fid %in% x@coords$fid,]

              nVids <- sapply(unique(x@coords$fid), function(i){
                length(x@coords$vid[x@coords$fid == i])
              })
              x@attr$n <- nVids
            }
            return(x)
          })

#' @rdname getSubset
#' @export
setMethod(f = "getSubset",
          signature = signature("Spatial"),
          definition = function(x, attr){
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
          })

#' @rdname getSubset
#' @examples
#'
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' (aSfc <- getSubset(nc, attr = "NAME %in% c('Ashe', 'Surry')"))
#' @export
setMethod(f = "getSubset",
          signature = signature("sf"),
          definition = function(x, attr){
            if(!missing(attr)){
              if(is.logical(attr)){
                stopifnot(dim(x)[1] == length(attr))
                matches <- attr
              } else if(is.numeric(attr)){
                matches <- attr
              } else if(is.character(attr)){
                matches <- eval(parse(text = attr), envir = x)
              }
              x <- x[matches,]
            }
            return(x)
          })