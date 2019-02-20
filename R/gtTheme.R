#' Theme class (S4) and methods
#'
#' An \code{gtTheme} stores the \code{\link{visualise}} compatible theme to plot
#' rasters and geoms. While you can assign values to the slots manually, it
#' makes more sense to use \code{\link{setTheme}}, which carries out all the
#' checks and makes sure that names of the parameters are properly matched.
#' @slot title [\code{named list(3)}]\cr properties of the title.
#' @slot box [\code{named list(4)}]\cr properties of the bounding box.
#' @slot xAxis [\code{named list(5)}]\cr properties of the x-axis, its labels
#'   and ticks.
#' @slot yAxis [\code{named list(5)}]\cr properties of the y-axis, its labels
#'   and ticks.
#' @slot grid [\code{named list(5)}]\cr properties of the major and minor grid.
#' @slot legend [\code{named list(10)}]\cr properties of the legend, its title,
#'   labels, ticks and bounding box.
#' @slot geom [\code{named list(7)}]\cr properties of a geom.
#' @slot raster [\code{named list(2)}]\cr properties of a raster.

themeClass <- setClass(Class = "gtTheme",
                       slots = c(title = "list",
                                 box = "list",
                                 xAxis = "list",
                                 yAxis = "list",
                                 grid = "list",
                                 legend = "list",
                                 geom = "list",
                                 raster = "list"
                       )
)


#' Print gtTheme in the console
#'
#' @param object [\code{gtTheme}]\cr object to \code{show}.
#' @importFrom crayon green yellow red cyan
#' @importFrom cli symbol

setMethod(f = "show",
          signature = "gtTheme",
          definition = function(object){
            cat(ifelse(object@title$plot,
                       paste0(green(symbol$tick), yellow(" title    "), " in ", object@title$colour, " with fontsize ", object@title$fontsize),
                       paste0(red(symbol$cross), yellow(" title    "))), "\n")
            cat(ifelse(object@box$plot,
                       paste0(green(symbol$tick), yellow(" box      "), " in ", object@box$colour, " with ", object@box$linewidth, " wide ", object@box$linetype, " lines"),
                       paste0(red(symbol$cross), yellow(" box      "))),"\n")
            cat(ifelse(object@xAxis$plot,
                       paste0(green(symbol$tick), yellow(" xAxis    "), " with ", object@xAxis$bins, " bins and a margin of ", object@xAxis$margin, "\n",
                              ifelse(object@xAxis$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "'", object@xAxis$label$title, "' in ", object@xAxis$label$colour, " with fontsize ", object@xAxis$label$fontsize, ifelse(object@xAxis$label$rotation != 0, paste0(" and a rotation of ", object@xAxis$label$rotation), "")),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",
                              ifelse(object@xAxis$ticks$plot,
                                     paste0(green(symbol$tick), yellow("  - ticks  "), "in ", object@xAxis$ticks$colour, " with fontsize ", object@xAxis$ticks$fontsize, " rounded to ", object@xAxis$ticks$digits, ifelse(object@xAxis$ticks$digits == 1, " digit", " digits")),
                                     paste0(red(symbol$cross), yellow("  - ticks  ")))),
                       paste0(red(symbol$cross), yellow(" xAxis    "))), "\n")
            cat(ifelse(object@yAxis$plot,
                       paste0(green(symbol$tick), yellow(" yAxis    "), " with ", object@yAxis$bins, " bins and a margin of ", object@yAxis$margin, "\n",
                              ifelse(object@yAxis$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "'", object@yAxis$label$title, "' in ", object@yAxis$label$colour, " with fontsize ", object@yAxis$label$fontsize, ifelse(object@yAxis$label$rotation != 0, paste0(" and a rotation of ", object@yAxis$label$rotation), "")),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",
                              ifelse(object@yAxis$ticks$plot,
                                     paste0(green(symbol$tick), yellow("  - ticks  "), "in ", object@yAxis$ticks$colour, " with fontsize ", object@yAxis$ticks$fontsize, " rounded to ", object@yAxis$ticks$digits, ifelse(object@yAxis$ticks$digits == 1, " digit", " digits")),
                                     paste0(red(symbol$cross), yellow("  - ticks  ")))),
                       paste0(red(symbol$cross), yellow(" yAxis    "))), "\n")
            cat(ifelse(object@grid$plot,
                       paste0(green(symbol$tick), yellow(" grid     "), " in ", object@grid$colour, " with ", object@grid$linewidth, " wide ", object@grid$linetype, " lines"),
                       paste0(red(symbol$cross), yellow(" grid     "))), "\n")
            cat(ifelse(object@legend$plot,
                       paste0(green(symbol$tick), yellow(" legend   "), " with values ordered ", ifelse(object@legend$ascending, "ascending", "descending"), " in ", object@legend$bins, " bins and a relative height of ", object@legend$sizeRatio, "\n",
                              ifelse(object@legend$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "in ", object@legend$label$colour, " with fontsize ", object@legend$label$fontsize),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",
                              ifelse(object@legend$box$plot,
                                     paste0(green(symbol$tick), yellow("  - box    "), "in ", object@legend$box$colour, " with ", object@legend$box$linewidth, " wide ", object@legend$box$linetype, " lines"),
                                     paste0(red(symbol$cross), yellow("  - box    ")))),
                       paste0(red(symbol$cross), yellow(" legend    "))), "\n")
            cat(paste0(green(symbol$tick), yellow(" geom     "), " with ", object@geom$scale$x, "-colour scaled to ", cyan(object@geom$scale$to), ", ", object@geom$linewidth, " wide ", object@geom$linetype, " lines and ", object@geom$pointsize, " wide points of type ", object@geom$pointsymbol, "\n"))
            cat(paste0(green(symbol$tick), yellow(" raster   "), " with colours scaled to ", cyan(object@raster$scale)))
          }
)