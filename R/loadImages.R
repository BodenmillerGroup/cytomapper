#' @title Read images into CytoImageList object
#' @name loadImages
#'
#' @description
#' Function to read in single- or multi-channel images from a specified path or
#' file. The function returns a \linkS4class{CytoImageList} object containing
#' one image per slot. Supported file extensions are: '.tiff', '.tif', '.png',
#' '.jpeg', '.jpg'.
#'
#' @param x The function takes a variety of possible character inputs:
#' \describe{
#' \item{A single file}{Full path and file name of an individual image file.}
#' \item{A path}{A path to where image files are located.}
#' \item{A vector of files}{A character vector where each entry represents an
#' individual file.}
#' }
#' @param pattern Character inputs of the following form:
#' \describe{
#' \item{A single character}{A pattern to search for in the specified path
#' (regular expressions are supported).}
#' \item{A character vector}{Unique entries are matched against
#' file names in the specified path.}
#' }
#' @param on_disk Logical indicating if images in form of
#' \linkS4class{HDF5Array} objects should be stored on disk rather than in
#' memory.
#' @param ... arguments passed to the \code{\link{readImage}} function.
#'
#' @return A \linkS4class{CytoImageList} object
#'
#' @section Loading specific images:
#' This function loads images via the \code{\link{readImage}} function and
#' stores them in a CytoImageList object. In the simplest case, \code{x} is an
#' image file name. If \code{x} is a path, the \code{pattern} argument can be
#' used to select image names with certain patterns. For convenience, pattern
#' also takes a vector of characters (e.g. a colData entry in a
#' \linkS4class{SingleCellExperiment} object) to select by unique image names.
#' Furthermore, a vector of image names can be provided to read in multiple
#' images.
#'
#' @examples
#' # Providing a single file
#' single.image <- system.file("extdata/E34_mask.tiff", package = "cytomapper")
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
#'                             pattern = "_mask.tiff", full.names = TRUE)
#' image.list <- loadImages(list.images)
#'
#' @seealso
#' \code{\link{readImage}}, for reading in individual images.
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch}),
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
#'
#' @export
#' @importFrom BiocParallel bplapply SerialParam
loadImages <- function(x, pattern = NULL, on_disk = FALSE, h5FilesPath = getHDF5DumpDir(), 
                        BPPARAM = SerialParam(), ...) {

    # Validity checks
    x <- .valid.loadImage.input(x, pattern)

    # Read in images
    cur_list <- bplapply(x, function(y){
            cur_img <- EBImage::readImage(y, ..., names = NULL)
        
            if (on_disk) {
                writeHDF5Array(imageData(cur_img), 
                               filepath = h5FilesPath)
            } else {
                cur_img
            }
        }, BPPARAM = BPPARAM)
    
    out <- CytoImageList(cur_list)
    names(out) <- sub("\\.[^.]*$", "", basename(x))

    return(out)
}
