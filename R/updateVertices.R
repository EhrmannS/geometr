#' Update the vertices
#'
#' Set the vertices in a table so that they are valid for a geom.
#' @param input [\code{data.frame(1)}]\cr a table of vertices which should be
#'   brought into the correct form.
#' @importFrom checkmate assertNames assertDataFrame
#' @importFrom dplyr bind_cols group_by mutate distinct ungroup add_row
#'   bind_rows
#' @importFrom utils tail

.updateVertices <- function(input = NULL){

  # check arguments
  names(input) <- tolower(names(input))
  assertNames(x = names(input), must.include = c("x", "y"), subset.of = c("x", "y", "fid"))
  assertDataFrame(x = input, min.rows = 2)

  # if no fid is specified, treat it as if all vertices are part of the same feature
  if(!"fid" %in% names(input)){
    input <- bind_cols(input, fid = rep(1, dim(input)[1]))
  }

  newRings <- oldRings <- NULL
  for(i in unique(input$fid)){
    temp <- input[input$fid == i,]
    temp$ring <- NA
    verts <- temp
    verts$seq <- seq_along(temp$fid)

    # determine the duplicated vertices that enclose other vertices per ring
    dups <- duplicated(temp, fromLast = TRUE) + duplicated(temp)
    if(any(dups > 0)){
      bounds <- verts[which(as.logical(dups)),]
      bounds <- group_by(.data = bounds, x, y, fid)
      bounds <- mutate(.data = bounds, min = min(seq), max = max(seq), seq = NULL)
      bounds <- distinct(.data = bounds)
      bounds <- ungroup(bounds)

      # label vertices
      lab <- el <- seq <- NULL
      full <- seq(from = min(bounds$min), to = max(bounds$max), by = 1)
      for(j in seq_along(bounds$fid)){
        lab <- c(lab, seq(from = bounds[j, ]$min, bounds[j, ]$max, by = 1))
        el <- c(el, rep(j, bounds[j, ]$max - bounds[j, ]$min + 1))
      }
      seq[lab] <- el
      temp$ring[lab] <- seq
      lastRing <- max(temp$ring, na.rm = TRUE)

    } else {
      temp$ring <- NA
      lastRing <- 0
    }

    # get values that are not yet part of a closed ring
    missingRing <- which(is.na(temp$ring))
    if(length(missingRing) > 0){
      oldRings <- bind_rows(oldRings, temp[-missingRing,])
    } else {
      oldRings <- bind_rows(oldRings, temp)
    }
    ind <- c(missingRing[-1], tail(missingRing, 1)+1) - missingRing

    # split up into list of separate rings
    splitBy <- NULL
    k <- 1
    for(j in seq_along(ind)){
      if(ind[j] > 1){
        splitBy <- c(splitBy, k)
        k <- k + 1
      } else {
        splitBy <- c(splitBy, k)
      }
    }
    missingRings <- split(missingRing, splitBy)

    # go through all rings and check them ...
    newRing <- NULL
    for(j in seq_along(missingRings)){
      aRing <- temp[missingRings[[j]], ]
      aRing$ring <- lastRing + 1

      dup <- duplicated(aRing, fromLast = TRUE)
      if(!dup[1]){
        aRing <- add_row(aRing, x = aRing$x[1], y = aRing$y[1], fid = aRing$fid[1], ring = lastRing + 1)
      }

      newRing <- bind_rows(newRing, aRing)
    }
    newRings <- bind_rows(newRings, newRing)
  }

  out <- bind_rows(oldRings, newRings)

  # go through each ring other than ring 1 and check whether they are inside
  # ring one
  parent <- out[out$ring == 1,]
  for(i in unique(out$ring)){
    if(i == 1){
      next
    }
    inside <- pointInGeomC(vert = as.matrix(out[out$ring == i,][c("x", "y")]),
                           geom = as.matrix(parent[c("x", "y")]),
                           invert = FALSE)

    if(any(inside != 1)){
      stop("some of the vertices are not within the outer ring.")
    }
  }
  out$ring <- NULL

  return(out)
}