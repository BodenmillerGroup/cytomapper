#' @rdname loadImages
#' @title
#'
#' @description Function to read in single- or multi-channel images
#'
#' @param x The function takes a variety of possible character inputs
#' \describe{
#' \item{A single file}}{}.
#' \item{A path}}{}.
#' \item{A list of files}}{}.
#' }
#' @param pattern character vector of the following form
#' \describe{
#' \item{A single character specifying a pattern to search for in the specified path
#' (regular expressions are supported)}.
#' \item{A character vector in which unique entries are matched against
#' file names in the specified path}.
#' }
#'  @param ... arguments passed to the \code{\link{readImage}} function
#'
#' @return An \linkS4class{Image} or \linkS4class{ImageList} object
#'
#' @examples
#' # TODO
#'
#' @seealso
#' \code{\link{readImage}}, for reading in individual images.
#'
#' @author Nils Eling \email{nils.eling@@dqbm.uzh.ch},
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
#'
#' @importFrom EBImage Image
#' @importFrom tools file_ext
#' @export


### Example of inputs
## loadImages("ImagePath", "ImageName", "ImageId", '/home/nicolasd/Git/SingleCellMapper/inst/extdata/image_list.csv')
##
## sce <- readRDS('/home/nicolasd/Git/SingleCellMapper/inst/extdata/sce.rds')
## loadImages(colData(sce)$ImagePath, colData(sce)$ImageName, colData(sce)$ImageNb)
## loadImages("ImagePath", "ImageName", "ImageNb", sce)


loadImages <- function(x, pattern = NULL, ...) {

  # Validity checks
  x <- .valid.loadImage.input(x, pattern)

  # If an external csv file is provided, import it
  if(!is.null(image.list) & is.character(image.list)){
    image.list <- importCSV(image.list)
  }

  # Create a SimpleList containing image paths, names and ids
  image.list <- createImageList(path=image.path, name=image.name, id=image.id, image.list=image.list)

  # Read the images with EBImage
  images <- readImages(image.list)

  return(images)
}

#' readImages
#'
#' read the images and store them in an ImageList (currently, a SimpleList, should be adapted)
#'
#' @param image.list
#' @keywords image read
#' @return images
#' @import
#' @importFrom EBImage readImage
#' @export

readImages <- function (image.list) {
  fn <- file.path(image.list$imagePath, image.list$imageName)

  SimpleList(images)
  images <- lapply(fn, EBImage::readImage)
}



#' createImageList
#'
#' Store the image paths and names in a DataFrame
#'
#' @param path
#' @param name
#' @param id
#' @param image.list
#' @keywords image list path
#' @return image.list
#' @import
#' @importFrom S4Vectors DataFrame
#' @export

createImageList <- function (path, name, id, image.list=NULL) {

  if(is(image.list, "SingleCellExperiment")){
    image.list <- colData(sce)
  }

  if(is.null(image.list)){
    image.list <- DataFrame(imagePath = path,
                            imageName = name,
                            imageID = id)
  } else if (!is.null(image.list)){
    image.list <- DataFrame(imagePath = image.list[, (path)],
                            imageName = image.list[, (name)],
                            imageID = image.list[, (id)])
  }
  return(unique(image.list))
}


#' importCSV
#'
#' If an external CSV file is provided, import it
#'
#' @param image.list
#' @keywords image list csv import
#' @return image.list
#' @import
#' @importFrom tools file_ext
#' @export

#importCSV <- function (image.list) {

#  if (file.exists(image.list) & tools::file_ext(image.list) == "csv")
#    image.list <- read.csv(image.list, header = T, stringsAsFactors = F)
#
#  return(image.list)
#}

.valid.loadImage.input <- function(x, pattern){

  # Check if input is character
  if(!is.character(x)){
    stop("Please provide a string input indicating a single file/n",
    ", a path or a vector of files.")
  }

  # Further checks depending on length of object
  if(length(x) == 1){
    if(!file.exists(x)){

      stop("The provided file or path does not exist.\n",
           "Make sure the file or path is accessible from the current location.")

    }

    if(dir.exists(x) & is.null(pattern)){

        # Check if path only contains images
        exten <- sapply(list.files(x), tools::file_ext)

        if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff"))) > 0){
          stop("The provided path contains file-types other than 'jpeg', 'tiff' or 'png'.\n",
               "Please provide a correct regular expression in the 'pattern' argument to select correct images.")
        }

        message("All files in the provided location will be read in.")
        out <- list.files(x, full.names = TRUE)

    } else if(dir.exists(x) & !is.null(pattern)){

      # Check pattern
      if(!is.character(pattern) & !is.factor(pattern)){
        stop("Please provide a single character, character vector or factor as pattern input .\n")
      }

      out <- list.files(x, full.names = TRUE)

      # Since more than regular expressions can be given to pattern,
      # we need to perform selection manually
      if(length(pattern) == 1){
        out <- out[,grepl(pattern, out)]

      } else {
        # Build pattern for grep function
        pattern <- unique(pattern)

        out <- out[,grepl(paste(pattern, collapse = "|"), out)]
      }

      # Check if any of the files contain the pattern
      if(length(out) == 0){
        stop("The pattern does not match any of the files in the provided directory.")
      }

      # Check if all of the files are of the supported format
      exten <- sapply(list.files(x), tools::file_ext)

      if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff"))) > 0){
        stop("The provided path contains file-types other than 'jpeg', 'tiff' or 'png'.\n",
             "Please provide a correct regular expression in the 'pattern' argument to select correct images.")
      }

    } else {
        if(!tools::file_ext(x) %in% c("jpeg", "png", "tiff")){
          stop("The provided file is not of type 'jpeg', 'tiff' or 'png'.\n",
               "Other image types are not supported.")
        }
      out <- x
    }
  } else {
    # Check if files exists
    cur_check <- sapply(x, file.exists)
    if(sum(!cur_check) > 0){
      stop("One or multiple files do not exist.\n",
           "Please correct the input.")
    }

    # Check if files are os supported format
    exten <- sapply(x, tools::file_ext)
    if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff"))) > 0){
      stop("The files are of type other than 'jpeg', 'tiff' or 'png'.\n",
           "Please only provide files of the supported file-type..")
    }

    out <- x

  }
  return(out)
}




