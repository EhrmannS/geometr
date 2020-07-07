#' Geometry class (S4) and methods
#'
#' A \code{geom} stores a table of points, a table of feature to which the
#' points are associated and a table of groups, to which features are
#' associated. A \code{geom} can be spatial, but is not by default. A
#' \code{geom} can either have absolute or relative values, where relative
#' values specify the point position relative to the \code{window} slot.
#'
#' A \code{geom} is one of three geometry objects: \itemize{ \item
#' \code{"point"}, when none of the points are connected to other points, \item
#' \code{"line"}, where points with the same \code{fid} are connected following
#' the sequence of their order, without the line closing in itself and \item
#' \code{"polygon"}, where points with the same \code{fid} are connected
#' following the sequence of their order and the line closes in on itself due to
#' first and last point being the same. Moreover, \code{polygon} objects can
#' contain holes.}
#'
#' The data model for storing points follows the spaghetti model. Points are
#' stored as a sequence of x and y values, associated to a feature ID. The
#' feature ID relates coordinates to features and thus common attributes. Points
#' and Lines are implemented straightforward in this model, but polygons, which
#' may contain holes, are a bit trickier. In \code{geometr} they are implemented
#' as follows: \enumerate{ \item All points with the same \code{fid} make up
#' one polygon, irrespective of it containing holes or not. \item The outer
#' path/ring of a polygon is composed of all points until a duplicated of its
#' first point occurs. This signals that all following points are part of
#' another path/ring, which must be inside the outer path and which consists of
#' all points until a duplicate of it's first point occurs. \item This
#' repeats until all points of the feature are processed.}
#'
#' Moreover, a \code{geom} does not have the slot \emph{extent}, which
#' characterises the minimum and maximum value of the point coordinates and
#' which is thus derived "on the fly" from the points. Instead it has a
#' \emph{reference window}, which is sort of a second extent that may be bigger
#' (or smaller) than \code{extent} and which determines the relative position of
#' the points when plotting.
#'
#' @slot type [\code{character(1)}]\cr the type of feature, either
#'   \code{"point"}, \code{"line"}, \code{"polygon"} or \code{"grid"}.
#' @slot point [\code{data.frame(1)}]\cr the \code{fid} (feature ID), \code{x}
#'   and \code{y} coordinates per point and optional arbitrary point
#'   attributes.
#' @slot feature [\code{data.frame(1)}]\cr \code{fid} (feature ID), \code{gid}
#'   (group ID) and optional arbitrary feature attributes.
#' @slot group [\code{data.frame(1)}]\cr \code{gid} (group ID) and optional
#'   arbitrary group attributes.
#' @slot window [\code{data.frame(1)}]\cr the minimum and maximum value in x and
#'   y dimension of the reference window in which the \code{geom} dwells.
#' @slot scale [\code{character(1)}]\cr whether the point coordinates are
#'   stored as \code{"absolute"} values, or \code{"relative"} to \code{window}.
#' @slot crs [\code{character(1)}]\cr the coordinate reference system in proj4
#'   notation.
#' @slot history [\code{list(.)}]\cr a list of steps taken to derive the
#'   \code{geom} in focus.

geom <- setClass(Class = "geom",
                 slots = c(type = "character",
                           point = "data.frame",
                           feature = "list",
                           group = "list",
                           window = "data.frame",
                           scale = "character",
                           crs = "character",
                           history = "list"
                 )
)

setValidity("geom", function(object){

  errors = character()

  if(!.hasSlot(object = object, name = "type")){
    errors = c(errors, "the geom does not have a 'type' slot.")
  } else {
    if(!any(object@type %in% c("point", "line", "polygon", "grid"))){
      errors = c(errors, "the geom must either be of type 'point', 'line', 'polygon' or 'grid'.")
    } else if(object@type == "point"){
      if(dim(object@point)[1] < 1){
        errors = c(errors, "a geom of type 'point' must have at least 1 point.")
      }
    } else if(object@type == "line"){
      if(dim(object@point)[1] < 2){
        errors = c(errors, "a geom of type 'line' must have at least 2 points.")
      }
    } else if(object@type == "polygon"){
      if(dim(object@point)[1] < 3){
        errors = c(errors, "a geom of type 'polygon' must have at least 3 points.")
      }
    } else if(object@type == "grid"){
      if(dim(object@point)[1] != 3){
        errors = c(errors, "a geom of type 'grid' must have three rows ('origin' and 'cell number' extent and 'cell size').")
      }
    }
  }

  if(!.hasSlot(object = object, name = "point")){
    errors = c(errors, "the geom does not have a 'point' slot.")
  } else {
    if(!is.data.frame(object@point)){
      errors = c(errors, "the slot 'point' is not a data.frame.")
    }
    if(object@type == "grid"){
      if(!all(c("x" ,"y") %in% names(object@point))){
        errors = c(errors, "the geom must have a grid table with the columns 'x' and 'y'.")
      }
    } else {
      if(!all(c("fid", "x" ,"y") %in% names(object@point))){
        errors = c(errors, "the geom must have a point table with the columns 'x', 'y' and 'fid'.")
      }
    }
  }

  if(!.hasSlot(object = object, name = "feature")){
    errors = c(errors, "the geom does not have a 'feature' slot.")
  } else {
    if(!is.list(object@feature)){
      errors = c(errors, "the slot 'feature' is not a list")
    }
    if(is.null(names(object@feature))){
      errors = c(errors, "the slot 'feature' must contain named lists.")
    }
    if(object@type != "grid"){
      for(i in seq_along(object@feature)){
        if(!all(c("fid", "gid") %in% names(object@feature[[i]]))){
          errors = c(errors, "the geom must have a features table with at least the columns 'fid' and 'gid'.")
        }
      }
    }
  }

  if(!.hasSlot(object = object, name = "group")){
    errors = c(errors, "the geom does not have a 'group' slot.")
  } else {
    if(!is.list(object@group)){
      errors = c(errors, "the slot 'group' is not a list.")
    }
    if(is.null(names(object@group))){
      errors = c(errors, "the slot 'group' must contain named lists.")
    }
    for(i in seq_along(object@group)){
      if(!all(c("gid") %in% names(object@group[[i]]))){
        errors = c(errors, "the geom must have a group table with the column 'gid'.")
      }
    }
  }

  if(!.hasSlot(object = object, name = "window")){
    errors = c(errors, "the geom does not have a 'window' slot.")
  } else {
    if(!is.data.frame(object@window)){
      errors = c(errors, "the slot 'window' is not a data.frame.")
    }
    if(!all(c("x" ,"y") %in% names(object@window))){
      errors = c(errors, "the geom must have a window table with columns 'x' and 'y'.")
    }
  }

  if(!.hasSlot(object = object, name = "scale")){
    errors = c(errors, "the geom does not have a 'scale' slot.")
  } else {
    if(!is.character(object@scale)){
      errors = c(errors, "the slot 'scale' is not a character vector.")
    } else {
      # if(object@type == "grid"){
      #   if(!any(object@scale %in% c("continuous", "categorical"))){
      #     errors = c(errors, "the scale must either be 'continuous' or 'categorical'.")
      #   }
      # } else {
      if(!any(object@scale %in% c("absolute", "relative"))){
        errors = c(errors, "the scale must either be 'absolute' or 'relative'.")
      }
      # }

    }
  }

  if(!.hasSlot(object = object, name = "crs")){
    errors = c(errors, "the geom does not have a 'crs' slot.")
  } else {
    if(!is.character(object@crs)){
      errors = c(errors, "the slot 'crs' is not a character vector.")
    }
  }

  if(!.hasSlot(object = object, name = "history")){
    errors = c(errors, "the geom does not have a 'history' slot.")
  } else {
    if(!is.list(object@history)){
      errors = c(errors, "the slot 'history' is not a list.")
    }
  }

  if(length(errors) == 0){
    return(TRUE)
  } else {
    return(errors)
  }

})

#' Print geom in the console
#'
#' @param object [\code{geom}]\cr object to \code{show}.
#' @importFrom utils head
#' @importFrom crayon yellow red cyan

setMethod(f = "show",
          signature = "geom",
          definition = function(object){

            theType <- object@type
            thePoints <- getPoints(x = object)
            theFeatures <- getFeatures(x = object)
            theGroups <- getGroups(x = object)

            vertAttribs <- length(thePoints)
            featureAttribs <- length(theFeatures)
            groupAttribs <- length(theGroups)

            myAttributes <- NULL
            points <- feats <- groups <- FALSE

            if(is.na(object@crs)){
              myCrs <- "cartesian"
            } else {
              myCrs <- object@crs
            }

            if(theType == "grid"){

              if(!is.data.frame(theFeatures)){
                theFeats <- names(theFeatures)
                for(i in seq_along(theFeatures)){
                  theLayer <- theGroups[[i]]
                  theName <- names(theGroups)[i]

                  if(dim(theLayer)[1] != 0){
                    myAttributes <- c(myAttributes, paste0(" {", theName, "} ",
                                                           ifelse(featureAttribs <= 9,
                                                                  paste0(paste0(names(theLayer)[!names(theLayer) %in% c("gid")], collapse = ", "), "\n"),
                                                                  paste0(paste0(c(head(names(theLayer)[!names(theLayer) %in% c("gid")], 9), "..."), collapse = ", "), "\n")
                                                           )))
                  }
                }
              } else {
                theFeats <- names(object@feature)
                theLayer <- theGroups
                if(!is.null(theLayer)){
                  if(!all(names(thePoints) %in% c("gid"))){
                    myAttributes <- c(myAttributes, paste0(" ", ifelse(featureAttribs <= 9,
                                                                       paste0(paste0(names(theLayer)[!names(theLayer) %in% c("gid")], collapse = ", "), "\n"),
                                                                       paste0(paste0(c(head(names(theLayer)[!names(theLayer) %in% c("gid")], 9), "..."), collapse = ", "), "\n")
                    )))
                  }
                }
              }

              if(length(unique(theFeats)) == 1){
                myFeat <- "layer"
              } else {
                myFeat <- "layers"
              }
              myUnits <- "cells"
              geomGroups <- ""

            } else {
              theGrps <- theGroups$gid
              if(length(unique(theGrps)) == 1){
                myGrp <- "group"
              } else {
                myGrp <- "groups"
              }
              theFeats <- theFeatures$fid
              if(length(unique(theFeats)) == 1){
                myFeat <- "feature"
              } else {
                myFeat <- "features"
              }
              myUnits <- "points"
              geomGroups <- paste0(length(unique(theGrps)), " ", myGrp, " | ")

              if(!all(names(thePoints) %in% c("x", "y", "fid"))){
                myAttributes <- c(myAttributes, paste0(" (points) ",
                                                       ifelse(vertAttribs <= 9,
                                                              paste0(paste0(names(thePoints)[!names(thePoints) %in% c("x", "y", "fid")], collapse = ", "), "\n"),
                                                              paste0(paste0(c(head(names(thePoints)[!names(thePoints) %in% c("x", "y", "fid")], 9), "..."), collapse = ", "), "\n")
                                                       )))
                points <- TRUE
              }
              if(!all(names(theFeatures) %in% c("fid", "gid"))){
                if(points){
                  featureString <- "           (features) "
                } else {
                  featureString <- " (features) "
                }
                myAttributes <- c(myAttributes, paste0(featureString,
                                                       ifelse(featureAttribs <= 9,
                                                              paste0(paste0(names(theFeatures)[!names(theFeatures) %in% c("fid", "gid")], collapse = ", "), "\n"),
                                                              paste0(paste0(c(head(names(theFeatures)[!names(theFeatures) %in% c("fid", "gid")], 9), "..."), collapse = ", "), "\n")
                                                       )))
                feats <- TRUE
              }
              if(!all(names(theGroups) %in% c("gid"))){
                if(feats | points){
                  groupString <- "           (groups) "
                } else {
                  groupString <- " (groups) "
                }
                myAttributes <- c(myAttributes, paste0(groupString,
                                                       ifelse(groupAttribs <= 9,
                                                              paste0(paste0(names(theGroups)[!names(theGroups) %in% c("gid")], collapse = ", "), "\n"),
                                                              paste0(paste0(c(head(names(theGroups)[!names(theGroups) %in% c("gid")], 9), "..."), collapse = ", "), "\n")
                                                       )))
              }
            }
            if(is.null(myAttributes)){
              myAttributes <- " --\n"
            }


            cat(yellow(class(object)), "        ", object@type, "\n", sep = "")
            cat("            ", geomGroups, length(unique(theFeats)), " ", myFeat, " | ", length(thePoints$fid), " ", myUnits, "\n", sep = "")
            cat(yellow("crs         "), myCrs, "\n", sep = "")
            cat(yellow("attributes "), myAttributes, sep = "")
            if(!theType == "grid"){
              # make a tiny map
              tinyMap <- .makeTinyMap(geom = object)
              cat(yellow("tiny map  "), tinyMap)
            } else {
              theRes <- getRes(object)
              theExt <- getExtent(object)
              cat(yellow("resolution "), as.numeric(theRes), "(x, y)\n")
              cat(yellow("extent     "), c(theExt$x, theExt$y), "(xmin, xmax, ymin, ymax)")
            }
          }
)
