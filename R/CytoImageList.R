#' S4 class for list of images
#'
#' This class facilitates the handling of multiple one- or multi-channel images.
#' It inherits from \code{\linkS4class{SimpleList}} setting
#' \code{elementType="Image"}. Therefore, each slot contains an either one- or
#' multi-dimensional array in form of an \code{\linkS4class{Image}} object.
#'
#' @param ... A list of images (or coercible to a list) or individual images
#' @param on_disk Logical indicating if images in form of
#' \linkS4class{HDF5Array} objects (as .h5 files) should be stored on disk
#' rather than in memory.
#' @param h5FilesPath path to where the .h5 files for on disk representation
#' are stored. This path needs to be defined when \code{on_disk = TRUE}.
#' When files should only temporarily be stored on disk, please set
#' \code{h5FilesPath = getHDF5DumpDir()}
#' @param BPPARAM parameters for parallelised processing. 
#' This is only recommended for very large images. 
#' See \code{\linkS4class{MulticoreParam}} for information on how to use
#' multiple cores for parallelised processing.
#'
#' @details Similar to the \code{\linkS4class{Image}} class, the first two
#' dimensions of each entry indicate the spatial dimension of the image. These
#' can be different for each entry. The third dimension indicates the number
#' of channels per Image. Each entry in the CytoImageList class object must
#' contain the same number of channels. Here, each channel represents pixel
#' values indicating measurement intensities or in case of segmentation masks
#' the cells' ID. The CytoImageList class therefore only supports a Grayscale
#' colormode (see \code{\link{colormode}}) representation of each
#' individual image.
#'
#' The class further contains an \code{elementMetadata} slot that
#' stores image-level meta information. This slot should be accessed using the
#' \code{\link{mcols}} accessor function.
#'
#' @section Restrictions on entry names:
#' The CytoImageList class only supports unique entry names to avoid duplicated
#' images. Names of a CytoImageList object can be get and set via
#' \code{names(x)}, where \code{x} is a CytoImageList object. Furthermore, only
#' named or unnamed CytoImageList objects are allowed. Partially named objects
#' causing empty or NA names return an error.
#'
#' @section Coercion:
#' Coercion to and from list, \code{\linkS4class{SimpleList}} and
#' \code{\linkS4class{List}}:
#' \describe{
#' \item{as.list(x), as(x, "SimpleList"),
#' as(x, "SimpleList"):}{Coercion from a CytoImageList object \code{x}}
#' \item{as(x, "CytoImageList"):}{Coercion from a list, SimpleList or List
#' object \code{x} to anCytoImageList object}}
#'
#' @section Looping:
#' While \code{\link[base]{lapply}} and \code{\link[base]{mapply}} return
#' regular list objects, \code{\link{endoapply}} and
#' \code{\link{mendoapply}} return CytoImageList objects.
#' 
#' @section On disk representation:
#' When setting \code{on_disk = TRUE} and specifying the \code{h5FilesPath}, 
#' images are stored on disk. To convert back to an in-memory 
#' \code{CytoImageList} object, one can call 
#' \code{CytoImageList(on_disk_IL, on_disk = FLASE)}.
#'
#' @seealso
#' \code{\linkS4class{Image}}, for further image analysis tools.
#' 
#' \code{\linkS4class{SimpleList}}, for basics functions to handle SimpleList
#' objects
#' 
#' \code{?\link{loadImages}}, for reading images into a CytoImageList
#' object
#' 
#' \code{?"\link{CytoImageList-naming}"}, for setting and getting image and
#' channel names
#' 
#' \code{?"\link{CytoImageList-subsetting}"}, for subsetting and
#' accessor functions
#'
#' @return A CytoImageList object
#'
#' @examples
#' # Creation of CytoImageList
#' u <- matrix(rbinom(100, 10, 0.5), ncol=10, nrow=10)
#' v <- matrix(rbinom(100, 10, 0.5), ncol=10, nrow=10)
#' IL1 <- CytoImageList(image1 = Image(u), image2 = Image(v))
#'
#' # Coercion
#' as.list(IL1)
#' as(IL1, "SimpleList")
#' as(list(image1 = Image(u), image2 = Image(v)), "CytoImageList")
#' 
#' # On disk representation
#' IL1 <- CytoImageList(image1 = Image(u), image2 = Image(v),
#'                      on_disk = TRUE, 
#'                      h5FilesPath = HDF5Array::getHDF5DumpDir())
#'
#' @aliases
#' coerce,ANY,CytoImageList-method
#' coerce,list,CytoImageList-method
#' show,CytoImageList-method
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#'
#' @docType class
#'
#' @importFrom S4Vectors new2 mcols<-
#'
#' @export
#' @importFrom BiocParallel bplapply SerialParam MulticoreParam
#' @importFrom EBImage Image imageData
#' @importFrom DelayedArray DelayedArray
CytoImageList <- function(..., on_disk = FALSE, h5FilesPath = NULL,
                            BPPARAM = SerialParam()){
    
    args <- list(...)
    
    if (length(args) == 1L &&
        methods::extends(class(args[[1L]]), "list")){
        args <- args[[1]]
    }
    
    if (length(args) == 1L &&
        methods::extends(class(args[[1L]]), "SimpleList")){
        
        # Make sure mcols are transfered
        cur_meta <- mcols(args[[1]])
        
        args <- as.list(args[[1]])
    }
    
    if (on_disk) {
        
        cur_class <- lapply(args, class)
        
        if (all(cur_class == "Image")) {
            
            if (is.null(names(args))){
                stop("Please specify the names of the images.")
            }
            
            if (is.null(h5FilesPath)) {
                stop("When storing the images on disk, please specify a 'h5FilesPath'. \n",
                     "You can use 'h5FilesPath = getHDF5DumpDir()' to temporarily store the images.\n",
                     "If doing so, .h5 files will be deleted once the R session ends.")
            }
            
            cur_names <- names(args)
            args <- bplapply(names(args), function(y){
                cur_name <- y
                cur_file <- file.path(h5FilesPath, paste0(y, ".h5"))
                
                # Check if file already exists
                # If so, delete them
                if (file.exists(cur_file)) {
                    file.remove(cur_file)
                }
                
                writeHDF5Array(DelayedArray(imageData(args[[y]])), 
                               filepath = cur_file,
                               name = cur_name,
                               with.dimnames = TRUE)
            }, BPPARAM = BPPARAM)
            names(args) <- cur_names
        }
    } else {
        cur_class <- lapply(args, class)
        
        if (all(cur_class == "HDF5Array" | cur_class == "HDF5Matrix" |
                cur_class == "DelayedArray" | cur_class == "DelayedMatrix")) {
            
            if (is.null(names(args))){
                stop("Please specify the names of the images.")
            }
            cur_names <- names(args)
            args <- bplapply(names(args), function(y){
                Image(as.array(args[[y]]))
            }, BPPARAM = BPPARAM)
            names(args) <- cur_names
        }
        
    }
    
    x <- S4Vectors::new2("CytoImageList", listData = args)
    
    # Store metadata again
    if (exists("cur_meta") && !is.null(cur_meta)) {
        mcols(x) <- cur_meta
    }
    
    return(x)
}

# Coercion from list
#' @exportMethod coerce
#' @importFrom DelayedArray seed path
setAs("list", "CytoImageList", function(from) {
    
    if (class(from[[1]]) %in% c("HDF5Array", "DelayedArray",
                                "HDF5Matrix", "DelayedMatrix")){
        on_disk <- TRUE
        h5FilesPath <- dirname(path(seed(from[[1]])))
    } else {
        on_disk <- FALSE
        h5FilesPath <- NULL
    }
    
    # Use constructor function
    CytoImageList(from, on_disk = on_disk, h5FilesPath = h5FilesPath)
})

# Coercion from ANY
#' @exportMethod coerce
#' @importFrom DelayedArray seed path
setAs("ANY", "CytoImageList", function(from) {
    
    if (class(from[[1]]) %in% c("HDF5Array", "DelayedArray",
                                "HDF5Matrix", "DelayedMatrix")){
        on_disk <- TRUE
        h5FilesPath <- dirname(path(seed(from[[1]])))
    } else {
        on_disk <- FALSE
        h5FilesPath <- NULL
    }
    
    # Use constructor function
    CytoImageList(from, on_disk = on_disk, h5FilesPath = h5FilesPath)
})

# Expanded show method
#' @exportMethod show
setMethod("show", signature = signature(object="CytoImageList"),
    definition = function(object){
        lo <- length(object)
        cat(class(object)[1], " containing ", lo,
            " image(s)\n", sep = "")
        if (!is.null(names(object))) {
                cat(paste0("names(", lo, "):"), names(object), "\n", sep = " ")
                if (length(dim(object[[1]])) > 2) {
                cat("Each image contains ", dim(object[[1]])[3],
                    " channel(s)\n", sep = "")
                } else {
                cat("Each image contains 1 channel\n", sep = "")
                }
        }
        if (!is.null(channelNames(object))) {
            cat(paste0("channelNames(", length(channelNames(object)),
                    "):"), channelNames(object), "\n", sep = " ")
        }
    }
)

