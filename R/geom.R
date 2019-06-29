#' Geometry class (S4) and methods
#'
#' A \code{geom} stores a table of vertices, a table of feature to which the
#' vertices are associated and a table of groups, to which features are
#' associated. A \code{geom} can be spatial, but is not by default. A
#' \code{geom} can either have absolute or relative values, where relative
#' values specify the vertex position relative to the \code{window} slot.
#'
#' A \code{geom} is one of three geometry objects: \itemize{ \item
#' \code{"point"}, when none of the vertices are connected to other vertices,
#' \item \code{"line"}, where vertices with the same \code{fid} are connected
#' following the sequence of their order, without the line closing in itself and
#' \item \code{"polygon"}, where vertices with the same \code{fid} are connected
#' following the sequence of their order and the line thus closes in on itself.
#' Moreover, \code{polygon} objects can contain holes.}
#'
#' The data model for storing vertices follows the spaghetti model. Vertices are
#' stored as a sequence of x and y values, associated to a feature ID. The
#' feature ID relates coordinates to features and thus common attributes. Points
#' and Lines are implemented straightforward in this model, but polygons, which
#' may contain holes, are a bit trickier. In \code{geometr} they are implemented
#' as follows: \enumerate{ \item All vertices with the same \code{fid} make up
#' one polygon, irrespective of it containing holes or not. \item The outer
#' path/ring of a polygon is composed of all vertices until a duplicated of its
#' first vertex occurs. This signals that all following vertices are part of
#' another path/ring, which must be inside the outer path and which consists of
#' all vertices until a duplicate of it's first vertex occurs. \item This
#' repeats until all vertices of the feature are processed.}
#'
#' Moreover, a \code{geom} does not have the slot \emph{extent}, which
#' characterises the minimum and maximum value of the vertex coordinates and
#' which is thus derived "on the fly" from the vertices. Instead it has a
#' \emph{reference window}, which is sort of a second extent that may be bigger
#' (or smaller) than \code{extent} and which determines the relative position of
#' the vertices when plotting.
#'
#' @slot type [\code{character(1)}]\cr the type of feature, either
#'   \code{"point"}, \code{"line"} or \code{"polygon"}.
#' @slot vert [\code{data.frame(1)}]\cr the \code{fid} (feature ID), \code{x}
#'   and \code{y} coordinates per vertex and optional arbitrary vertex
#'   attributes.
#' @slot feat [\code{data.frame(1)}]\cr \code{fid} (feature ID), \code{gid}
#'   (group ID) and optional arbitrary feature attributes.
#' @slot group [\code{data.frame(1)}]\cr \code{gid} (group ID) and optional
#'   arbitrary group attributes.
#' @slot window [\code{data.frame(1)}]\cr the minimum and maximum value in x and
#'   y dimension of the reference window in which the \code{geom} dwells.
#' @slot scale [\code{character(1)}]\cr whether the vertex coordinates are
#'   stored as \code{"absolute"} values, or \code{"relative"} to \code{window}.
#' @slot crs [\code{character(1)}]\cr the coordinate reference system in proj4
#'   notation.
#' @slot history [\code{list(.)}]\cr a list of steps taken to derive the
#'   \code{geom} in focus.

geom <- setClass(Class = "geom",
                 slots = c(type = "character",
                           vert = "data.frame",
                           feat = "data.frame",
                           group = "data.frame",
                           window = "data.frame",
                           scale = "character",
                           crs = "character",
                           history = "list"
                 )
)

setValidity("geom", function(object){

  errors = character()

  if(!.hasSlot(object = object, name = "vert")){
    errors = c(errors, "the geom does not have a 'vert' slot.")
  } else {
    if(!is.data.frame(object@vert)){
      errors = c(errors, "the slot 'vert' is not a data.frame.")
    }
    if(!all(c("fid", "x" ,"y") %in% names(object@vert))){
      errors = c(errors, "the geom must have a vertex table with the columns 'x', 'y' and 'fid'.")
    }
  }

  if(!.hasSlot(object = object, name = "feat")){
    errors = c(errors, "the geom does not have a 'feat' slot.")
  } else {
    if(!is.data.frame(object@feat)){
      errors = c(errors, "the slot 'feat' is not a data.frame.")
    }
    if(!all(c("fid", "gid") %in% names(object@feat))){
      errors = c(errors, "the geom must have a features table with the columns 'fid' and 'gid'.")
    }
  }

  if(!.hasSlot(object = object, name = "group")){
    errors = c(errors, "the geom does not have a 'group' slot.")
  } else {
    if(!is.data.frame(object@group)){
      errors = c(errors, "the slot 'group' is not a data.frame.")
    }
    if(!all(c("gid") %in% names(object@group))){
      errors = c(errors, "the geom must have a group table with the column 'gid'.")
    }
  }

  if(!.hasSlot(object = object, name = "type")){
    errors = c(errors, "the geom does not have a 'type' slot.")
  } else {
    if(!any(object@type %in% c("point", "line", "polygon"))){
      errors = c(errors, "the geom must either be of type 'point', 'line' or 'polygon'.")
    } else if(object@type == "point"){
      if(dim(object@vert)[1] < 1){
        errors = c(errors, "a geom of type 'point' must have at least 1 vertex.")
      }
    } else if(object@type == "line"){
      if(dim(object@vert)[1] < 2){
        errors = c(errors, "a geom of type 'line' must have at least 2 vertices.")
      }
    } else if(object@type == "polygon"){
      if(dim(object@vert)[1] < 3){
        errors = c(errors, "a geom of type 'polygon' must have at least 3 vertices.")
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
      if(!any(object@scale %in% c("absolute", "relative"))){
        errors = c(errors, "the scale must either be 'absolute' or 'relative'.")
      }
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
#' @importFrom crayon green yellow red cyan

setMethod(f = "show",
          signature = "geom",
          definition = function(object){
            vertAttribs <- length(object@vert)
            featureAttribs <- length(object@feat)
            groupAttribs <- length(object@group)
            if(length(unique(object@feat$fid)) == 1){
              myFeat <- "feature"
            } else {
              myFeat <- "features"
            }
            if(is.na(object@crs)){
              myCrs <- "cartesian"
            } else {
              myCrs <- object@crs
            }
            myAttributes <- NULL
            verts <- feats <- groups <- FALSE
            if(!all(names(object@vert) %in% c("x", "y", "fid"))){
              myAttributes <- c(myAttributes, paste0(" (vertices) ",
                                                     ifelse(vertAttribs <= 9,
                                                            paste0(paste0(names(object@vert)[!names(object@vert) %in% c("x", "y", "fid")], collapse = ", "), "\n"),
                                                            paste0(paste0(c(head(names(object@vert)[!names(object@vert) %in% c("x", "y", "fid")], 9), "..."), collapse = ", "), "\n")
                                                     )))
              verts <- TRUE
            }
            if(!all(names(object@feat) %in% c("fid", "gid"))){
              if(verts){
                theFeats <- "           (features) "
              } else {
                theFeats <- " (features) "
              }
              myAttributes <- c(myAttributes, paste0(theFeats,
                                                     ifelse(featureAttribs <= 9,
                                                            paste0(paste0(names(object@feat)[!names(object@feat) %in% c("fid", "gid")], collapse = ", "), "\n"),
                                                            paste0(paste0(c(head(names(object@feat)[!names(object@feat) %in% c("fid", "gid")], 9), "..."), collapse = ", "), "\n")
                                                     )))
              feats <- TRUE
            }
            if(!all(names(object@group) %in% c("gid"))){
              if(feats | verts){
                theGroups <- "           (groups) "
              } else {
                theGroups <- " (groups) "
              }
              myAttributes <- c(myAttributes, paste0(theGroups,
                                                     ifelse(groupAttribs <= 9,
                                                            paste0(paste0(names(object@group)[!names(object@group) %in% c("gid")], collapse = ", "), "\n"),
                                                            paste0(paste0(c(head(names(object@group)[!names(object@group) %in% c("gid")], 9), "..."), collapse = ", "), "\n")
                                                     )))
            }
            if(is.null(myAttributes)){
              myAttributes <- " --\n"
            }

            # make a tiny map
            tinyMap <- .makeTinyMap(geom = object)

            cat(yellow(class(object)), "        ", object@type, "\n", sep = "")
            cat("            ", length(unique(object@feat$fid)), " ", myFeat, " | ", length(object@vert$fid), " vertices\n", sep = "")
            cat(yellow("crs         "), myCrs, "\n", sep = "")
            cat(yellow("attributes"), myAttributes)
            cat(yellow("tiny map  "), tinyMap)
          }
)
