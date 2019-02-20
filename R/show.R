#' Print geom in the console
#'
#' @param object [\code{geom}]\cr object to \code{show}.

setMethod(f = "show",
          signature = "geom",
          definition = function(object){
            cat("class      : ", class(object), "\n", sep = "")
            cat("type       : ", object@type, "\n", sep = "")
            cat("features   : ", length(unique(object@coords$fid)), "  (", length(object), " vertices)\n", sep = "")
            cat("window     : ", min(object@window$x), ", ", max(object@window$x), ", ", min(object@window$y), ", ", max(object@window$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("extent     : ", min(object@coords$x), ", ", max(object@coords$x), ", ", min(object@coords$y), ", ", max(object@coords$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("scale      : ", object@scale, "\n", sep = "")
            cat("crs        : ", object@crs, "\n", sep = "")
            cat("attributes : ", length(object@attr), "  (", paste0(names(object@attr)[!names(object@attr) %in% c("x", "y")], collapse = ", "), ")\n", sep = "")
          })