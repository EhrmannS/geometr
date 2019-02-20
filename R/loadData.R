#' Load \code{csv} files
#'
#' csv is the short form of \emph{Comma Separated Values}, which represents
#' simple tables of systematically delimited information. The files loaded with
#' this function should have the columns \code{x}, \code{y} and potentially
#' other columns that represent the attributes of these coordinates, for example
#' an \code{id}.
#'
#' A column \code{fid} can have a deviating value than \code{id} when several
#' features are part of the same object, such as multiple line features or holes
#' in polygons.
#' @param path [\code{character(1)}]\cr the local path of the file to load.
#' @return a \code{point geometry} of the coordinates
#' @family loaders
#' @importFrom checkmate assertFile
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @export

load_csv <- function(path){

  assertFile(path, access = "r", extension = "csv")
  out <- read_csv(path)
  colnames(out) <- tolower(colnames(out))
  assertNames(names(out), must.include = c("x", "y"))
  if(!"vid" %in% names(out)){
    theCoords <- tibble(vid = seq_along(out$x), x = out$x, y = out$y)
  } else{
    theCoords <- tibble(vid = out$vid, x = out$x, y = out$y)
  }
  if(!"fid" %in% names(out)){
    theCoords <- bind_cols(fid = seq_along(out$id), theCoords)
  } else{
    theCoords <- bind_cols(fid = out$fid, theCoords)
  }
  theData <- out[which(!colnames(out) %in% c("fid", "vid", "x", "y"))]
  theAttr <- tibble(fid = unique(theCoords$fid),
                    n = as.integer(table(theCoords$fid)))
  theAttr <- bind_cols(theAttr, theData)

  out <- new(Class = "geom",
             type = "point",
             coords = theCoords,
             attr = theAttr,
             window = tibble(x = rep(c(min(out$x), max(out$x)), each = 2), y = c(min(out$y), max(out$y), max(out$y), min(out$y))),
             scale = "absolute",
             crs = as.character(NA),
             history = list(paste0("geom has been loaded from ", path)))

  return(out)
}

#' Load \code{kml} files
#'
#' kml is the short form of \emph{Keyhole Markup Language}, which uses the
#' \code{XML} format.
#' @param path [\code{character(1)}]\cr the local path of the file to load.
#' @param layer [\code{character(1)}]\cr the layer name.
#' @return a geom of the contents of the kml file.
#' @family loaders
#' @importFrom checkmate assertFile
#' @importFrom stringr str_extract str_replace str_split
#' @importFrom tibble tibble
#' @export

load_kml <- function(path, layer = NULL){

  assertFile(path, access = "r", extension = "kml")
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE, null.ok = TRUE)

  txt <- suppressWarnings(readLines(path))
  txt <- txt[grep("<coordinates> *([^<]+?) *<\\/coordinates>", txt)]

  # get ids
  id <- str_extract(string = txt, pattern = "<name>.*?<\\/name>")
  id <- str_replace(string = id, pattern = "<name>", replacement = "")
  id <- str_replace(string = id, pattern = "</name>", replacement = "")
  uniqueID <- unique(id)

  # get coordinates
  coords <- str_extract(string = txt, pattern = "<coordinates>.*?<\\/coordinates>")
  coords <- str_replace(string = coords, pattern = "<coordinates>", replacement = "")
  coords <- str_replace(string = coords, pattern = "</coordinates>", replacement = "")
  coords <- str_split(string = coords, pattern = " ")
  coords <- coords[!duplicated(id)]
  nIds <- lengths(coords)
  coords <- unlist(coords)
  nCoords <- length(coords)
  coords <- as.numeric(unlist(str_split(coords, ",")))
  coords <- as_tibble(matrix(coords, nrow = nCoords, byrow = T))
  coords <- tibble(rep(uniqueID, nIds), fid = rep(uniqueID, nIds), coords$V1, coords$V2)
  colnames(coords) <- c("id", "fid", "x", "y")

  # what feature type is it?
  if(all(grepl("Point", txt))){
    type <- "point"
  } else if(all(grepl("LineString", txt))){
    type <- "line"
  } else if(all(grepl("Polygon", txt))){
    type <- "polygon"
  } else{
    stop("I was not able to determine the feature type of this 'kml'.")
  }

  out <- new(Class = "geom",
             type = type,
             coords = coords,
             attr = tibble(fid = unique(coords$id), n = 1),
             window = tibble(x = rep(c(min(coords$x), max(coords$x)), each = 2), y = c(min(coords$y), max(coords$y), max(coords$y), min(coords$y))),
             scale = "absolute",
             crs = as.character(NA),
             history = list(paste0("geom has been loaded from ", path)))

  return(out)
}
