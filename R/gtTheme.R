#' Theme class (S4) and methods
#'
#' An \code{gtTheme} stores a theme to \code{\link{visualise}} vector and raster
#' objects. It is recommended to use \code{\link{setTheme}} to modify a
#' \code{gtTheme}, because it carries out all the checks and makes sure that
#' names of the parameters are properly matched.
#' @slot title [\code{named list(3)}]\cr properties of the title.
#' @slot box [\code{named list(5)}]\cr properties of the bounding box.
#' @slot xAxis [\code{named list(5)}]\cr properties of the x-axis, its labels
#'   and ticks.
#' @slot yAxis [\code{named list(5)}]\cr properties of the y-axis, its labels
#'   and ticks.
#' @slot grid [\code{named list(5)}]\cr properties of the major and minor grid.
#' @slot legend [\code{named list(9)}]\cr properties of the legend, its title,
#'   labels, ticks and bounding box.
#' @slot scale [\code{names list(6)}]\cr properties of scaling parameters to
#'   attributes/variables.
#' @slot parameters [\code{named list(7)}]\cr parameters of the plot object.

themeClass <- setClass(Class = "gtTheme",
                       slots = c(title = "list",
                                 box = "list",
                                 xAxis = "list",
                                 yAxis = "list",
                                 grid = "list",
                                 legend = "list",
                                 scale = "list",
                                 parameters = "list"
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
                       paste0(green('\u2714'), yellow(" box      "), " in ", cyan(object@box$linecol), " with ", cyan(object@box$linewidth), " wide lines of type ", cyan(object@box$linetype)),
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
                       paste0(green('\u2714'), yellow(" grid     "), " in ", cyan(object@grid$colour), " with ", cyan(object@grid$linewidth), " wide lines of type ", cyan(object@grid$linetype)),
                       paste0(red('\u2716'), yellow(" grid     "))), "\n")
            cat(ifelse(object@legend$plot,
                       paste0(green('\u2714'), yellow(" legend   "), " with values ordered ", cyan(ifelse(object@legend$ascending, "ascending", "descending")), " in ", cyan(object@legend$bins), " bins and a relative height of ", cyan(object@legend$yRatio), "\n",
                              ifelse(object@legend$label$plot,
                                     paste0(green('\u2714'), yellow("  - label  "), "in ", cyan(object@legend$label$colour), " with fontsize ", cyan(object@legend$label$fontsize)),
                                     paste0(red('\u2716'), yellow("  - label  "))), "\n",
                              ifelse(object@legend$box$plot,
                                     paste0(green('\u2714'), yellow("  - box    "), "in ", cyan(object@legend$box$colour), " with ", cyan(object@legend$box$linewidth), " wide lines of type ", cyan(object@legend$box$linetype)),
                                     paste0(red('\u2716'), yellow("  - box    ")))),
                       paste0(red('\u2716'), yellow(" legend    "))), "\n")
            linecols <- ifelse("linecol" %in% object@scale$param,
                               paste0(cyan(paste0("[", paste0(object@parameters$colours, collapse = ","), "]")), yellow(ifelse("linecol" %in% object@scale$param, paste0("(", object@scale$to, ")"), "(-)"))),
                               paste0(cyan("black")))
            fillcols <- ifelse("fillcol" %in% object@scale$param,
                               paste0(cyan(paste0("[", paste0(object@parameters$colours, collapse = ","), "]")), yellow(ifelse("fillcol" %in% object@scale$param, paste0("(", object@scale$to, ")"), "(-)"))),
                               paste0(cyan("empty")))
            linewidths <- ifelse(length(object@parameters$linewidth) > 1,
                                 paste0(cyan(paste0("[", paste0(object@parameters$linewidth, collapse = ","), "]")), yellow(ifelse("linewidth" %in% object@scale$param, paste0("(", object@scale$to, ")"), "(-)")), " wide lines"),
                                 paste0(cyan(object@parameters$linewidth), " wide lines"))
            linetypes <- ifelse(length(object@parameters$linetype) > 1,
                                paste0(cyan(paste0("[", paste0(object@parameters$linetype, collapse = ","), "]")), yellow(ifelse("linetype" %in% object@scale$param, paste0("(", object@scale$to, ")"), "(-)"))),
                                paste0(cyan(object@parameters$linetype)))
            pointsizes <- ifelse(length(object@parameters$pointsize) > 1,
                                 paste0(cyan(paste0("[", paste0(object@parameters$pointsize, collapse = ","), "]")), yellow(ifelse("pointsize" %in% object@scale$param, paste0("(", object@scale$to, ")"), "(-)")), " wide points"),
                                 paste0(cyan(object@parameters$pointsize), " wide points"))
            pointsymbols <- ifelse(length(object@parameters$pointsymbol) > 1,
                                   paste0(cyan(paste0("[", paste0(object@parameters$pointsymbol, collapse = ","), "]")), yellow(ifelse("pointsymbol" %in% object@scale$param, paste0("(", object@scale$to, ")"), "(-)"))),
                                   cyan(object@parameters$pointsymbol))
            cat(paste0(green('\u2714'), yellow(" objects  "), " with line-colour ", linecols, " and fill-colour ", fillcols, "\n            with ", linewidths, " of type ", linetypes, "\n            or ", pointsizes, " of type ", cyan(pointsymbols), "\n"))
          }
)