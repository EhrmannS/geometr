#' Determin number of vertices
#'
#' @param x [\code{geom}]\cr object from which to determine \code{length}.

setMethod(f = "length",
          signature = "geom",
          definition = function(x){
            dim(x@coords)[1]
          })