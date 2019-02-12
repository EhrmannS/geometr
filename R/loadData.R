#' Import spatial datasets
#'
#' Meta function to import, i.e. load or download and unpack spatial datasets
#' and other files containing spatial data.
#' @param files [\code{character(.)} | \code{data.frame(1)}]\cr the files, as
#'   they are called in \code{localPath}. \code{files} also accepts the
#'   (subsetted) output of \code{\link{catalog}}, i.e. a \code{data.frame} with
#'   the two columns \code{original} and \code{abbr}.
#' @param dataset [\code{character(1)}]\cr character vector of the dataset for
#'   which \code{files} should be imported (see \code{\link{obtain}} for all
#'   datasets); in case this is given, \code{localPath} is taken from the
#'   internal default for the respective dataset (see
#'   \code{\link{setPaths}}).
#' @param layer [\code{character(.)} | \code{integerish(.)}]\cr vetor of the
#'   same length as \code{files} with name or position of the layer that shall
#'   be loaded.
#' @param localPath [\code{character(1)}]\cr the local path from where files
#'   shall be loaded. If the directory does not exist, it is created and the
#'   missing data are downloaded, given .
#' @param verbose [\code{logical(1)}]\cr should additional information be
#'   printed (\code{TRUE}) or should it be suppressed (\code{FALSE}, default)?
#' @param ... [various]\cr other arguments of the load operators.
#' @details \code{importData} checks whether the required files are available in
#'   \code{localPath} and if this is not given, in the working directory. If
#'   nothing is found there but a dataset is given, it attempts to download the
#'   files from the respective online path (see \code{rtPaths}). Please take a
#'   look at it and in case an online resource has been altered, you can adapt
#'   it here (see \code{\link{setPaths}}).
#' @return the file to load. If there is more than one file specified, a list of
#'   those files.
#' @seealso The specific load operators: \code{\link{load_kml}},
#'   \code{\link{load_csv}}, \code{\link{load_hdf}}, \code{load_tif},
#'   \code{load_dbf}, \code{load_shp}, \code{\link{load_svg}}
#' @examples
#' require(magrittr)
#' 
#' myLocations <- loadData(files = "locations.csv",
#'                    localPath = system.file("test_datasets", package="rasterTools")) %>%
#'   geomRectangle() %>%
#'   setCRS(crs = projs$laea)
#' @importFrom checkmate testCharacter testDataFrame assertCharacter assertNames
#'   testIntegerish assertDirectory assertLogical assertEnvironment assertFile
#'   assertDataFrame testDirectoryExists
#' @importFrom utils file_test txtProgressBar setTxtProgressBar
#' @importFrom raster raster res<- stack
#' @export

loadData <- function(files = NULL, layer = NULL, dataset = NULL, localPath = NULL,
                     verbose = FALSE, ...){

  # check arguments
  assertCharacter(dataset, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(!is.null(dataset) & is.null(localPath)){
    dataset <- tolower(dataset)
    localPath <- eval(parse(text = paste0("rtPaths$", dataset, "$local")))
  }
  if(!is.null(localPath)){
    if(!testDirectoryExists(localPath)){
      dir.create(localPath)
    }
  }

  if(!is.null(files)){
    filesIsChar <- testCharacter(files, any.missing = FALSE, min.len = 1)
    filesIsDF <- testDataFrame(files, types = "character", ncols = 2)
    if(filesIsDF){
      assertNames(names(files), must.include = c("original", "abbr"))
      files <- files$original
    } else{
      assertCharacter(files, min.len = 1)
    }
  } else{
    if(!is.null(localPath)){
      files <- list.files(path = localPath)
    } else{
      stop("please specify either 'files' or 'localPath' to load the respective data.")
    }
  }
  if(!is.null(layer)){
    layerIsInt <- testIntegerish(layer, len = 1, any.missing = FALSE)
    layerIsChar <- testCharacter(layer, len = 1, any.missing = FALSE)
  }
  assertLogical(verbose, any.missing = FALSE, len = 1)

  # define function that checks whether 'files' do in fact exist
  testFiles <- function(files, path){
    filesInLocalPath <- list.files(path = path)
    filePaths <- list.files(path = path, full.names = TRUE)
    filesInLocalPath <- filesInLocalPath[file_test('-f', filePaths)]
    # filePaths <- filePaths[file_test('-f', filePaths)]
    fileExists <- files %in% filesInLocalPath
    
    return(fileExists)
  }
  out <- list()

  # go through file and load each of them
  if(verbose){
    pb <- txtProgressBar(min = 0, max = length(files), style = 3, char=">", width = getOption("width")-14)
  }
  for(i in seq_along(files)){

    theFile <- files[i]

    fileExists <- testFiles(theFile, path = localPath)
    thePath <- paste0(localPath, "/", theFile)
    fileTemp <- strsplit(theFile, split = "[.]")[[1]]
    fileType <- fileTemp[length(fileTemp)]
    fileName <- paste0(fileTemp[!fileTemp %in% fileType], collapse = ".")

    # if 'file' does not exist, attempt to download it (in case a downloadDATASET method is given)
    if(!fileExists & !is.null(dataset)){
      args <- list(file = files[i], localPath = localPath)

      do.call(what = paste0("download", toupper(dataset)),
              args = args)
    }

    fileExistsNow <- testFiles(theFile, path = localPath)

    if(fileExistsNow){

      # manage layer names
      # if(is.null(layer)){
      #   layer <- fileName
      # }

      # manage the arguments
      if(fileType %in% c("csv", "tif")){
        args <- list(path = thePath)
      } else{
        args <- list(path =  thePath,
                     layer = layer[i],
                     ...)
      }

      # and call the file type specific load_* function
      # message(paste0("  ... loading ", theFile, " from the local path '", localPath, "'\n"))
      history <- list()
      out_temp <- do.call(what = paste0("load_", fileType),
                          args = args)
      history <- c(history, paste0("object has been loaded from '", localPath, "'"))

      # assign history tag in case it's a 'Raster*'
      if(inherits(out_temp, "Raster")){
        out_temp@history <- history
      }

      out <- c(out, setNames(list(out_temp), fileName))

    } else{
      out <- c(out, setNames(list(paste0("file was not found locally or online")), fileName))
    }
    if(verbose){
      setTxtProgressBar(pb, i)
    }

  }
  if(verbose){
    close(pb)
  }

  if(length(out) == 1){
    out <- out[[1]]
  }

  return(out)
}

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
#'
#' This is a helper to \code{\link{loadData}} and is not intended to be used on
#' its own.
#' @template path
#' @return a \code{point geometry} of the coordinates
#' @family loaders
#' @importFrom utils read.csv
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @export

load_csv <- function(path){

  assertFile(path, access = "r", extension = "csv")
  out <- read.csv(path)
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
#'
#' This is a helper to \code{\link{loadData}} and is not intended to be used on
#' its own.
#' @template path
#' @param layer [\code{character(1)}]\cr the layer name.
#' @return a geom of the contents of the kml file.
#' @family loaders
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

#' Load \code{hdf} files
#'
#' hdf is the short form of \emph{Hierarchical Data Format}, which is a
#' standardised file format to store scientific data. There are two commonly
#' used version of this format, hdf4 and hdf5. For now, \code{load_hdf} loads
#' only files of format hdf5. It is a wrapper of \link[gdalUtils]{gdalinfo}.
#'
#' This is a helper to \code{\link{loadData}} and is not intended to be used on
#' its own.
#' @template path
#' @param layer [\code{character(1)}]\cr the layer name.
#' @return \code{RasterLayer} of the loaded \code{hdf} file
#' @family loaders
#' @importFrom gdalUtils get_subdatasets
#' @importFrom raster stack raster
#' @importFrom utils glob2rx
#' @export

load_hdf <- function(path, layer = NULL){

  assertFile(path, access = "r", extension = "hdf")
  layerIsChar <- testCharacter(layer, any.missing = FALSE, min.len = 1, ignore.case = TRUE)

  subsets_name <- get_subdatasets(datasetname = path)
  
  files <- unlist(lapply(
    seq_along(subsets_name), function(i){
      parts <- strsplit(subsets_name[i], ":")[[1]]
      parts[length(parts)]
      
    }
  ))
  
  # select the layer(s)
  if(layerIsChar){
    layer <- which(files %in% layer)
  }
  if(length(layer) == 0 | all(!layer %in% seq_along(files))){
    message(paste0("  -> you did not (properly) specify any layer, so I create a RasterStack\n"))
    layer <- seq_along(files)
  }

  tsds <- subsets_name[layer]
  files <- files[layer]

  if(length(tsds)>1){
    out <- stack(tsds)
  } else{
    out <- raster(tsds)
  }
  names(out) <- files

  return(out)
}

#' Load \code{svg} files
#'
#' svg is the short form of \emph{Scalable Vector Graphics}. These files are
#' basically text files interpreted and consequently visualised by any program
#' that is suitable to do so (such as your web-browser or
#' \href{www.inkscape.org}{inkscape}). This means that everything that is
#' visible in the image, is somehow coded into the text. Consequently, in case
#' the file is systematic and contains information of interest, these can be
#' turned into computer readable data.
#'
#' This is a helper to \code{\link{loadData}} and is not intended to be used on
#' its own.
#' @template path
#' @param layer [\code{character(1)}]\cr the origin of the dataset that is
#'   captured by the file. Recently this is only \code{"emma"}.
#' @return \code{data.frame} of the content of interest depending on
#'   \code{layer}.
#' @family loaders
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_split str_replace
#' @export

load_svg <- function(path, layer){

  assertFile(path, access = "r", extension = "svg")
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE)

  txt <- suppressWarnings(readLines(path))

  # throw out uninteresting lines
  if(layer == "emma"){

    path <- str_split(string = path, pattern = "/")[[1]]
    species <- path[length(path)]
    species <- str_split(string = species, pattern = "[.]")[[1]][1]
    species <- str_replace(string = species, pattern = "_", replacement = " ")

    txt <- txt[grep("use id[[:space:]]?=", txt)]
    txt <- gsub("'", "", txt)
    txt <- gsub('\"', "", txt)
    txt <- sub('<use id[[:space:]]?=[[:space:]]?', "", txt)
    txt <- sub('xlink:href[[:space:]]?=[[:space:]]?#', "", txt)
    txt <- sub('[[:space:]]?/>', "", txt)
    txt <- gsub(' = ', "=", txt)

    if(length(txt) != 0){
      # make a proper data.frame out of the mess.
      allOcc <- str_split(string = txt, pattern = " ")
      allOcc <- as.data.frame(do.call(rbind, allOcc))[-c(2:3)] # these values which look like coordinates are merely values needed to render the svg file.
      allOcc <- cbind(species, allOcc)
      allOcc <- as_tibble(allOcc)
      colnames(allOcc) <- c("species", "square", "year")
    } else{
      allOcc <- tibble(species = species, square = 0, year = 0)
    }
    return(allOcc)
    
  } else{
    stop("loading svg files other than from the EMMA dataset has not been programmed yet...")
  }
}

load_tif <- function(path){

  raster::raster(path)

}

