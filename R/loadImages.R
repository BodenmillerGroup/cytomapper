#' @rdname loadImages
#' @title Function to read in images
#'
#' @description Function to read in single- or multi-channel images
#'
#' @param x The function takes a variety of possible character inputs
#' \describe{
#' \item{A single file}{}.
#' \item{A path}{}.
#' \item{A list of files}{}.
#' }
#' @param pattern character vector of the following form
#' \describe{
#' \item{A single character specifying a pattern to search for in the specified path
#' (regular expressions are supported)}{}.
#' \item{A character vector in which unique entries are matched against
#' file names in the specified path}{}.
#' }
#' @param ... arguments passed to the \code{\link{readImage}} function
#'
#' @return An \linkS4class{Image} or \linkS4class{ImageList} object
#'
#' @examples
#' # Providing a single file
#' single.image <- system.file("extdata/A02_mask.tiff", package = "SingleCellMapper")
#' single.image <- loadImages(single.image)
#'
#' # Providing a path and pattern
#' path.to.images <- system.file("extdata", package = "SingleCellMapper")
#' image.list <- loadImages(path.to.images, pattern = "mask.tiff")
#'
#' # Providing multiple patterns
#' sce <- readRDS(system.file("extdata/sce.rds", package = "SingleCellMapper"))
#' path.to.images <- system.file("extdata", package = "SingleCellMapper")
#' image.list <- loadImages(path.to.images, pattern = sce$MaskName)
#'
#' @seealso
#' \code{\link{readImage}}, for reading in individual images.
#'
#' @author Nils Eling \email{nils.eling@@dqbm.uzh.ch},
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
#'
#' @importFrom EBImage Image readImage
#' @export
loadImages <- function(x, pattern = NULL, ...) {

  # Validity checks
  x <- .valid.loadImage.input(x, pattern)

  # Read in images
  if(length(x) == 1){
    out <- EBImage::readImage(x, ...)

  } else {
    cur_list <- lapply(x, function(y){EBImage::readImage(y, ...)})
    out <- ImageList(cur_list)
    names(out) <- sub("\\.[^.]*$", "", basename(x))
  }

  return(out)
}
