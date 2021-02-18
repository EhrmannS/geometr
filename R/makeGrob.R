#' Make the grob of a plot
#'
#' @param x the object to transform to class \code{grob}.
#' @param theme [\code{gtTheme(1)}]\cr the theme from which to take parameters.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#'   a \code{\link{polylineGrob}}, a \code{\link{pathGrob}} or a
#'   \code{\link{rasterGrob}}.
#' @importFrom rlang exprs rep_along
#' @importFrom grDevices colorRampPalette colors
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertNames assertSubset assertList
#' @importFrom dplyr left_join group_by mutate
#' @importFrom grid gpar unit pointsGrob gList pathGrob polylineGrob clipGrob
#'   rasterGrob

.makeGrob <- function(x, theme = gtTheme){

  # capture display arguments
  featureType <- getType(x = x)
  params <- theme@parameters

  if(featureType[1] != "grid") {

    # rescale values between 0 and 1
    x <- gt_scale(obj = x, range = tibble(x = c(0, 1), y = c(0, 1)))

    point <- getPoints(x = x)
    ids <- unique(point$fid)

    if(featureType[1] %in% "point"){

      out <- pointsGrob(x = unit(point$x, "npc"),
                        y = unit(point$y, "npc"),
                        pch = params$pointsymbol,
                        name = ids,
                        size = unit(params$pointsize, "char"),
                        gp = gpar(
                          col = params$linecol,
                          fill = params$fillcol))

    } else if(featureType[1] %in% "line"){

      out <- polylineGrob(x = unit(point$x, "npc"),
                          y = unit(point$y, "npc"),
                          id = as.numeric(as.factor(point$fid)),
                          name = ids,
                          gp = gpar(col = params$linecol,
                                    lty = params$linetype,
                                    lwd = params$linewidth))

    } else if(featureType[1] %in% "polygon"){

      dups <- group_by(.data = point, fid, x, y)
      dups <- mutate(.data = dups,
                     is_dup = duplicated(x) & duplicated(y),
                     is_odd = seq_along(fid) %% 2 == 0,
                     dup = as.integer(is_dup & is_odd))
      dups <- dups[["dup"]]
      dups <- c(0, dups[-length(dups)])
      vids <- 1 + cumsum(dups)

      out <- pathGrob(x = point$x,
                      y = point$y,
                      id = vids,
                      pathId = point$fid,
                      rule = "evenodd",
                      name = ids,
                      gp = gpar(
                        col = params$linecol,
                        fill = params$fillcol,
                        lty = params$linetype,
                        lwd = params$linewidth))
    }

  } else {

    theRows <- theme@scale$rows
    theCols <- theme@scale$cols
    theColours <- params$fillcol

    out <- rasterGrob(x = unit(0, "npc"),
                      y = unit(0, "npc"),
                      width = unit(1, "npc"),
                      height = unit(1, "npc"),
                      hjust = 0,
                      vjust = 0,
                      image = matrix(data = theColours, nrow = theRows, ncol = theCols, byrow = TRUE),
                      name = "theRaster",
                      interpolate = FALSE)
  }

  if(is(out) != "gList"){
    out <- gList(out)
  }

  return(out)
}

