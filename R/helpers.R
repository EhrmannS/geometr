#' Convert degree to radians
#' @param degree [numeric(1)][numeric]\cr a degree value to convert to radians.
#' @importFrom checkmate assertNumeric

.rad <- function(degree){

  assertNumeric(x = degree)

  (degree * pi)/180
}


#' Get the number of decimal places
#' @param x [numeric(1)][numeric]\cr the number for which to derive decimal
#'   places.
#' @importFrom checkmate assertNumeric

.getDecimals <- function(x) {
  # https://stackoverflow.com/a/5173906/4506642

  assertNumeric(x = x)

  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }

}


#' Build a regular polygon
#'
#' @param pts description
#' @param vrt description
#' @importFrom checkmate assertDataFrame assertNames assertIntegerish
#' @importFrom tibble tibble

.makeRegular <- function(pts, vrt){

  assertDataFrame(x = pts, types = "numeric", any.missing = FALSE, min.cols = 2, min.rows = 2)
  assertNames(x = colnames(pts), must.include = c("x", "y", "fid"))
  assertIntegerish(x = vrt, len = 1, any.missing = FALSE)

  out <- tibble(x = double(), y = double(), fid = integer())
  for(i in seq_along(unique(pts$fid))){

    # the first two points of the i-th unique fid
    tempPts <- pts[c(1:2),pts$fid == unique(pts$fid)[i]]

    openingAngle <- atan((tempPts$x[1] - tempPts$x[2]) / (tempPts$y[1] - tempPts$y[2])) * 180 / pi

    angle <- 360/vrt
    angles <- seq(from = 90, to = 360-angle+90, by = angle) - openingAngle
    radius <- dist(tempPts[c(1:2),])
    cx <- tempPts$x[1] + radius*cos(.rad(angles))
    cy <- tempPts$y[1] + radius*sin(.rad(angles))

    tempOut <-  tibble(x = cx, y = cy, fid = i)
    out <- rbind(out, tempOut)

  }

  return(out)
}


#' Update the window
#'
#' Set a window to the minimum/maximum values of input vertices.
#' @param input [data.frame(1)][data.frame]\cr a table of vertices for which a new
#'   window should be derived.
#' @param window [data.frame(2)][data.frame]\cr in case the reference window
#'   deviates from the bounding box of \code{crds}, specify here the minimum and
#'   maximum values in columns \code{x} and \code{y}.
#' @return A new window that has the extent of input.
#' @importFrom checkmate assertNames assertDataFrame

.updateWindow <- function(input = NULL, window = NULL){

  # check arguments
  if(!testDataFrame(x = input, min.rows = 2)){
    return(window)
  }
  names(input) <- tolower(names(input))
  assertNames(x = names(input), must.include = c("x", "y"))
  names(window) <- tolower(names(window))
  assertNames(x = names(window), must.include = c("x", "y"))
  assertDataFrame(x = window, nrows = 2)

  if(min(input$x) != min(window$x)){
    window$x[window$x %in% min(window$x)] <- min(input$x)
  }
  if(max(input$x) != max(window$x)){
    window$x[window$x %in% max(window$x)] <- max(input$x)
  }
  if(min(input$y) != min(window$y)){
    window$y[window$y %in% min(window$y)] <- min(input$y)
  }
  if(max(input$y) != max(window$y)){
    window$y[window$y %in% max(window$y)] <- max(input$y)
  }
  return(window)
}


#' Update column order
#'
#' Set the order of the table columns to \code{c("fid", "gid", rest)}
#' @param input [data.frame(1)][data.frame]\cr a table that contains at least the
#'   columns \code{fid} and \code{gid}.
#' @return A new table where the columns have the correct order.

.updateOrder <- function(input = NULL){

  # check arguments
  targetCols <- c("fid", "gid")
  targetCols <- targetCols[targetCols %in% names(input)]

  out <- input[c(targetCols, names(input)[!names(input) %in% targetCols])]

  return(out)
}


#' Update the vertices
#'
#' Set the vertices in a table so that they are valid for a geom.
#' @param input [data.frame(1)][data.frame]\cr a table of vertices which should be
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
    inside <- pointInPolyCpp(vert = as.matrix(out[out$ring == i,][c("x", "y")]),
                             geom = as.matrix(parent[c("x", "y")]),
                             invert = FALSE)

    if(any(inside != 1)){
      stop("some of the vertices are not within the outer ring.")
    }
  }
  out$ring <- NULL

  return(out)
}


#' Test window for consistency
#'
#' @param x [data.frame(2)][data.frame]\cr the minimum and maximum values in
#'   columns \code{x} and \code{y}.
#' @param ... additional arguments.
#' @importFrom checkmate testDataFrame assertNames
#' @importFrom rlang exprs
#' @importFrom tibble as_tibble

.testWindow <- function(x, ...){

  args <- exprs(..., .named = TRUE)

  if(testDataFrame(x = x, types = "numeric", any.missing = FALSE, ncols = 2)){
    colnames(x) <- tolower(colnames(x))
    assertNames(names(x), permutation.of = c("x", "y"), .var.name = "window->names(x)")
    return(x)
  } else {
    if("verbose" %in% names(args)){
      assertLogical(x = args$verbose)
      if(args$verbose){
        message("'window' is not a data.frame.")
      }
    }
    return(NULL)
  }

}


#' Test points to not contain NA
#'
#' @param x [data.frame(2)][data.frame]\cr the points to be tested.
#' @param ... additional arguments.
#' @importFrom checkmate assertNames

.testPoints <- function(x, ...){

  assertNames(names(x), must.include = c("x", "y"), .var.name = "points->names(x)")

  if(all(is.na(x$x)) | all(is.na(x$y))){
    return(NULL)
  } else {
    return(x)
  }

}