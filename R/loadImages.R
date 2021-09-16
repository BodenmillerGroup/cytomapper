#' @title Read images into CytoImageList object
#' @name loadImages
#'
#' @description
#' Function to read in single- or multi-channel images from a specified path or
#' file. The function returns a \linkS4class{CytoImageList} object containing
#' one image per slot. Supported file extensions are: '.tiff', '.tif', '.png',
#' '.jpeg', '.jpg', ".h5".
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
#' \linkS4class{HDF5Array} objects (as .h5 files) should be stored on disk
#' rather than in memory.
#' @param h5FilesPath path to where the .h5 files for on disk representation
#' are stored. This path needs to be defined when \code{on_disk = TRUE}.
#' When files should only temporarily be stored on disk, please set
#' \code{h5FilesPath = getHDF5DumpDir()}
#' @param name (if reading in .h5 files) a single character, a character vector
#' of length equal to the length of x or NULL. See details for how to set 
#' \code{name}.
#' @param BPPARAM parameters for parallelised reading in of images. 
#' This is only recommended for very large images. 
#' See \code{\linkS4class{MulticoreParam}} for information on how to use multiple
#' cores for parallelised processing.
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
#' @section Reading in .h5 files:
#' When reading in .h5 files by default the \code{loadImages} function will
#' try to read in the dataset with the same name as the .h5 file from within
#' the file. If datasets are stored with different names, the \code{name}
#' argument must be specified. This can either be a single character if datasets
#' across all files are named the same or a character vector of the same length
#' as \code{x} indicating the dataset name within each .h5 file. By default,
#' the images/datasets are not read into memory when stored in .h5 files.
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
#' # On disk representation
#' path.to.images <- system.file("extdata", package = "cytomapper")
#' image.list <- loadImages(path.to.images, pattern = "mask.tiff",
#'                             on_disk = TRUE, 
#'                             h5FilesPath = HDF5Array::getHDF5DumpDir())
#'                             
#' # Parallel processing
#' path.to.images <- system.file("extdata", package = "cytomapper")
#' image.list <- loadImages(path.to.images, pattern = "mask.tiff",
#'                             BPPARAM = BiocParallel::MulticoreParam())
#'
#' @seealso
#' \code{\link{readImage}}, for reading in individual images.
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch}),
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
#'
#' @export
#' @importFrom BiocParallel bplapply SerialParam
#' @importFrom HDF5Array writeHDF5Array HDF5Array
#' @importFrom DelayedArray DelayedArray
#' @importFrom EBImage imageData
loadImages <- function(x, pattern = NULL, on_disk = FALSE, h5FilesPath = NULL, 
                       name = NULL, BPPARAM = SerialParam(), ...) {

    # Validity checks
    x <- .valid.loadImage.input(x, pattern, name)

    if (length(name) > 1) {
        names(name) <- x
    }
    
    if (all(file_ext(x) == "h5")) {
        on_disk <- TRUE
    }
    
    # Read in images
    cur_list <- bplapply(x, function(y){
            
            if (file_ext(y) == "h5") {
                if (is.null(name)) {
                    cur_name <- sub("\\.[^.]*$", "", basename(y))
                } else if (length(name) > 1) {
                    cur_name <- name[y]
                } else {
                    cur_name <- name
                }
            
                DelayedArray(HDF5Array(y, name = cur_name))
                
            } else {
                cur_img <- EBImage::readImage(y, ..., names = NULL)
        
                if (on_disk) {
                
                    if (is.null(h5FilesPath)) {
                        stop("When storing the images on disk, please specify a 'h5FilesPath'. \n",
                            "You can use 'h5FilesPath = getHDF5DumpDir()' to temporarily store the images.\n",
                            "If doing so, .h5 files will be deleted once the R session ends.")
                    }
                
                    # Build filename
                    cur_name <- sub("\\.[^.]*$", "", basename(y))
                    cur_file <- file.path(h5FilesPath, paste0(cur_name, ".h5"))
                
                    # Check if file already exists
                    # If so, delete them
                    if (file.exists(cur_file)) {
                        file.remove(cur_file)
                    }
                
                    writeHDF5Array(DelayedArray(imageData(cur_img)), 
                                filepath = cur_file,
                                name = cur_name,
                                with.dimnames = TRUE)
                } else {
                    cur_img
                }
            }
        }, BPPARAM = BPPARAM)
    
    names(cur_list) <- sub("\\.[^.]*$", "", basename(x))
    out <- CytoImageList(cur_list, on_disk = on_disk, h5FilesPath = h5FilesPath)

    return(out)
}
