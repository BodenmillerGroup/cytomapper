#' @title Read images into CytoImageList object
#' @name loadImages
#'
#' @description
#' Function to read in single- or multi-channel images from a specified path or file.
#' The function returns an \linkS4class{CytoImageList} object containing one image per slot.
#' Supported file extensions are: '.tiff', '.tif', '.png', '.jpeg', '.jpg'.
#'
#' @param x The function takes a variety of possible character inputs:
#' \describe{
#' \item{A single file}{Full path and file name of an individual image file.}
#' \item{A path}{A path to where image files are located.}
#' \item{A list of files}{A character vector where each entry represents an individual file.}
#' }
#' @param pattern Character inputs of the following form:
#' \describe{
#' \item{A single character}{A pattern to search for in the specified path
#' (regular expressions are supported).}
#' \item{A character vector}{Unique entries are matched against
#' file names in the specified path.}
#' }
#' @param ... arguments passed to the \code{\link{readImage}} function.
#'
#' @return An \linkS4class{Image} or \linkS4class{CytoImageList} object
#'
#' @section Loading specific images:
#' This function loads images via the \code{\link{readImage}} function and
#' stores them in an CytoImageList object. In the simplest case, \code{x} is an
#' image file name. If \code{x} is a path, the \code{pattern} argument can be
#' used to select image names with certain patterns. For convenience, pattern
#' also takes a vector of characters (e.g. a colData entry in a
#' \linkS4class{SingleCellExperiment} object) to select by unique image names.
#' Furthermore, a vecotr of image names can be provided to read in multiple
#' images.
#'
#' @examples
#' # Providing a single file
#' single.image <- system.file("extdata/A02_mask.tiff", package = "cytomapper")
#' single.image <- loadImages(single.image)
#'
#' # Providing a path and pattern
#' path.to.images <- system.file("extdata", package = "cytomapper")
#' image.list <- loadImages(path.to.images, pattern = "mask.tiff")
#'
#' # Providing multiple patterns
#' data(pancreasSCE)
#' path.to.images <- system.file("extdata", package = "cytomapper")
#' image.list <- loadImages(path.to.images, pattern = pancreasSCE$MaskName)
#'
#' # Providing multiple files
#' list.images <- list.files(system.file("extdata", package = "cytomapper"),
#'                           pattern = "_mask.tiff", full.names = TRUE)
#' image.list <- loadImages(list.images)
#'
#' @seealso
#' \code{\link{readImage}}, for reading in individual images.
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch}),
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
#'
#' @export
loadImages <- function(x, pattern = NULL, ...) {

  # Validity checks
  x <- .valid.loadImage.input(x, pattern)

  # Read in images
  if(length(x) == 1){
    out <- EBImage::readImage(x, ...)

  } else {
    cur_list <- lapply(x, function(y){EBImage::readImage(y, ...,
                                      names = NULL)})
    out <- CytoImageList(cur_list)
    names(out) <- sub("\\.[^.]*$", "", basename(x))
  }

  return(out)
}
