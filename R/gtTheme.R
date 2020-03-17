#' Theme class (S4) and methods
#'
#' An \code{gtTheme} stores a theme to \code{\link{visualise}} vector and raster
#' objects. It is recommended to use \code{\link{setTheme}} to modify a
#' \code{gtTheme}, because it carries out all the checks and makes sure that
#' names of the parameters are properly matched.
#' @slot title [\code{named list(3)}]\cr properties of the title.
#' @slot box [\code{named list(4)}]\cr properties of the bounding box.
#' @slot xAxis [\code{named list(5)}]\cr properties of the x-axis, its labels
#'   and ticks.
#' @slot yAxis [\code{named list(5)}]\cr properties of the y-axis, its labels
#'   and ticks.
#' @slot grid [\code{named list(5)}]\cr properties of the major and minor grid.
#' @slot legend [\code{named list(10)}]\cr properties of the legend, its title,
#'   labels, ticks and bounding box.
#' @slot vector [\code{named list(7)}]\cr properties of a vector object.
#' @slot raster [\code{named list(2)}]\cr properties of a raster object.

themeClass <- setClass(Class = "gtTheme",
                       slots = c(title = "list",
                                 box = "list",
                                 xAxis = "list",
                                 yAxis = "list",
                                 grid = "list",
                                 legend = "list",
                                 scale = "list",
                                 vector = "list",
                                 raster = "list"
                       )
)


#' Print gtTheme in the console
#'
#' @param object [\code{gtTheme}]\cr object to \code{show}.
#' @importFrom crayon green yellow red cyan

setMethod(f = "show",
          signature = "gtTheme",
          definition = function(object){
            cat(ifelse(object@title$plot,
                       paste0(green('\u2714'), yellow(" title    "), " in ", cyan(object@title$colour), " with fontsize ", cyan(object@title$fontsize)),
                       paste0(red('\u2716'), yellow(" title    "))), "\n")
            cat(ifelse(object@box$plot,
                       paste0(green('\u2714'), yellow(" box      "), " in ", cyan(object@box$linecol), " with ", cyan(object@box$linewidth), " wide ", cyan(object@box$linetype), " lines"),
                       paste0(red('\u2716'), yellow(" box      "))),"\n")
            cat(ifelse(object@xAxis$plot,
                       paste0(green('\u2714'), yellow(" xAxis    "), " with ", cyan(object@xAxis$bins), " bins and a margin of ", cyan(object@xAxis$margin*100), "%\n",
                              ifelse(object@xAxis$label$plot,
                                     paste0(green('\u2714'), yellow("  - label  "), cyan(object@xAxis$label$title), " in ", cyan(object@xAxis$label$colour), " with fontsize ", cyan(object@xAxis$label$fontsize), ifelse(object@xAxis$label$rotation != 0, paste0(" and a rotation of ", cyan(object@xAxis$label$rotation), "\uB0"), "")),
                                     paste0(red('\u2716'), yellow("  - label  "))), "\n",
                              ifelse(object@xAxis$ticks$plot,
                                     paste0(green('\u2714'), yellow("  - ticks  "), "in ", cyan(object@xAxis$ticks$colour), " with fontsize ", cyan(object@xAxis$ticks$fontsize), " rounded to ", cyan(object@xAxis$ticks$digits), ifelse(object@xAxis$ticks$digits == 1, " digit", " digits")),
                                     paste0(red('\u2716'), yellow("  - ticks  ")))),
                       paste0(red('\u2716'), yellow(" xAxis    "))), "\n")
            cat(ifelse(object@yAxis$plot,
                       paste0(green('\u2714'), yellow(" yAxis    "), " with ", cyan(object@yAxis$bins), " bins and a margin of ", cyan(object@yAxis$margin*100), "%\n",
                              ifelse(object@yAxis$label$plot,
                                     paste0(green('\u2714'), yellow("  - label  "), cyan(object@yAxis$label$title), " in ", cyan(object@yAxis$label$colour), " with fontsize ", cyan(object@yAxis$label$fontsize), ifelse(object@yAxis$label$rotation != 0, paste0(" and a rotation of ", cyan(object@yAxis$label$rotation), "\uB0"), "")),
                                     paste0(red('\u2716'), yellow("  - label  "))), "\n",
                              ifelse(object@yAxis$ticks$plot,
                                     paste0(green('\u2714'), yellow("  - ticks  "), "in ", cyan(object@yAxis$ticks$colour), " with fontsize ", cyan(object@yAxis$ticks$fontsize), " rounded to ", cyan(object@yAxis$ticks$digits), ifelse(object@yAxis$ticks$digits == 1, " digit", " digits")),
                                     paste0(red('\u2716'), yellow("  - ticks  ")))),
                       paste0(red('\u2716'), yellow(" yAxis    "))), "\n")
            cat(ifelse(object@grid$plot,
                       paste0(green('\u2714'), yellow(" grid     "), " in ", cyan(object@grid$colour), " with ", cyan(object@grid$linewidth), " wide ", cyan(object@grid$linetype), " lines"),
                       paste0(red('\u2716'), yellow(" grid     "))), "\n")
            cat(ifelse(object@legend$plot,
                       paste0(green('\u2714'), yellow(" legend   "), " with values ordered ", cyan(ifelse(object@legend$ascending, "ascending", "descending")), " in ", cyan(object@legend$bins), " bins and a relative height of ", cyan(object@legend$sizeRatio), "\n",
                              ifelse(object@legend$label$plot,
                                     paste0(green('\u2714'), yellow("  - label  "), "in ", cyan(object@legend$label$colour), " with fontsize ", cyan(object@legend$label$fontsize)),
                                     paste0(red('\u2716'), yellow("  - label  "))), "\n",
                              ifelse(object@legend$box$plot,
                                     paste0(green('\u2714'), yellow("  - box    "), "in ", cyan(object@legend$box$colour), " with ", cyan(object@legend$box$linewidth), " wide ", cyan(object@legend$box$linetype), " lines"),
                                     paste0(red('\u2716'), yellow("  - box    ")))),
                       paste0(red('\u2716'), yellow(" legend    "))), "\n")
            cat(paste0(green('\u2714'), yellow(" scale    "), " with ", cyan(object@scale$param), " scaled to ", cyan(object@scale$to), "\n"))
            cat(paste0(green('\u2714'), yellow(" objects  "), " of ", cyan(object@vector$linewidth), " wide ", cyan(object@vector$linetype), " lines or ", cyan(object@vector$pointsize), " wide points of type ", cyan(object@vector$pointsymbol), "\n"))
          }
)