# Utility functions for CytoImageList and Image class objects -----------------

#' @title Getting and setting the channel and image names
#' @name CytoImageList-naming
#'
#' @description
#' Methods to get and set the names of individual channels or the names of
#' individual images.
#'
#' @section Setting and getting the channel names:
#' In the following code, \code{x} is a \linkS4class{CytoImageList}
#' object containing one or multiple channels. The channel
#' names can be replaced by \code{value}, which contains a character vector of
#' the same length as the number of channels in the images.
#' \describe{
#' \item{\code{channelNames(x)}}{Returns the names of all channels stored in
#' \code{x}}
#' \item{\code{channelNames(x) <- value}}{Replaces the channel names of
#' \code{x} with \code{values}. For this, \code{value} needs to have the same
#' length as the number of channels in \code{x}}
#' }
#'
#' @section Setting and getting the image names:
#' Here, \code{x} is a \linkS4class{CytoImageList} object. The element
#' names can be replaced by \code{value}, which contains a character vector of
#' the same length as the number of images. In case of the CytoImageList object,
#' elements are always images.
#' \describe{
#' \item{\code{names(x)}}{Returns the names of all images stored in \code{x}}
#' \item{\code{names(x) <- value}}{Replaces the image names of
#' \code{x} with \code{value}. For this, \code{value} needs to have the same
#' length as \code{x}}
#' }
#'
#' @examples
#' data("pancreasImages")
#'
#' # Get channel and image names
#' channelNames(pancreasImages)
#' names(pancreasImages)
#'
#' # Set channel and image names
#' channelNames(pancreasImages) <- paste0("marker", 1:5)
#' names(pancreasImages) <- paste0("image", 1:3)
#'
#' @aliases
#' channelNames channelNames<-
#' channelNames,CytoImageList-method
#' channelNames<-,CytoImageList-method
#' names,CytoImageList-method
#' names<-,CytoImageList-method
#'
#' @docType methods
#'
#' @author
#' Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
NULL

#' @export
setMethod("channelNames",
    signature = signature(x="CytoImageList"),
    definition =  function(x){
    if(length(dim(x[[1]])) == 2L){
        return(NULL)
    } else {
        return(dimnames(x[[1]])[[3]])
    }
})

#' @export
#' @importFrom S4Vectors endoapply
#' @importFrom EBImage Image
#' @importFrom methods validObject
setReplaceMethod("channelNames",
    signature = signature(x="CytoImageList"),
    definition = function(x, value){
    # Image needs to be expanded to store channel names
    if(length(dim(x[[1]])) == 2L){
        x <- S4Vectors::endoapply(x, function(y){
        cur_Image <- Image(y, dim = c(dim(y)[1], dim(y)[2], 1))
        dimnames(cur_Image) <- c(dimnames(y), NULL)
            return(cur_Image)
        })
    }

    x <- S4Vectors::endoapply(x, function(y){
        dimnames(y)[[3]] <- as.character(value)
            return(y)
        })

    validObject(x)

    return(x)
    })

#' @export
#' @importFrom methods callNextMethod
setMethod("names",
    signature = signature(x="CytoImageList"),
    definition = function(x){
        callNextMethod()
    })

#' @export
#' @importFrom methods callNextMethod as validObject
setReplaceMethod("names",
    signature = signature(x="CytoImageList"),
    definition = function(x, value){
        .Object <- callNextMethod()
        .Object <- as(.Object, "CytoImageList")
        validObject(.Object)
        return(.Object)
    })

#' @title Manipulating CytoImageList objects
#' @name CytoImageList-manipulation
#'
#' @description Methods to change pixel values in CytoImageList objects. In the
#' following sections, \code{object} is a \linkS4class{CytoImageList} object
#' containing one or multiple channels.
#'
#' @section Image scaling:
#' In some cases, images need to be scaled by a constant
#' (e.g. 2^16-1 = 65535) \code{value} to revert them back to the original
#' pixel values after reading them in.
#' \describe{
#' \item{\code{scaleImages(object, value)}}{Scales all images in the
#' \linkS4class{CytoImageList} \code{object} by \code{value}.}
#' }
#'
#' @section Image normalization:
#' Linear scaling of the intensity values of each \linkS4class{Image} contained
#' in a \linkS4class{CytoImageList} \code{object} to a specific range. Images
#' can either be scaled to the minimum/maximum value per channel or across all
#' channels (default \code{separateChannels = TRUE}). Also, images can be scaled
#' to the minimum/maximum value per image or across all images (default
#' \code{separateImages = FALSE}). The latter allows the visual comparison of
#' intensity values across images.
#'
#' To clip the images before normalization, the \code{inputRange} can be set.
#' The \code{inputRange} either takes NULL (default), a vector of length 2
#' specifying the clipping range for all channels or a list where each
#' named entry contains a channel-specific clipping range.
#'
#' \code{normalize(object, separateChannels = TRUE, separateImages = FALSE,
#' ft = c(0, 1), inputRange = NULL)}:
#'
#' \describe{
#' \item{\code{object}:}{A CytoImageList object}
#' \item{\code{separateChannels}:}{Logical if pixel values should be normalized
#' per channel (default) or across all channels.}
#' \item{\code{separateImages}:}{Logical if pixel values should be normalized
#' per image or across all images (default).}
#' \item{\code{ft}:}{Numeric vector of 2 values, target minimum and maximum
#' intensity values after normalization (see \code{\link[EBImage]{normalize}}).}
#' \item{\code{inputRange}:}{Numeric vector of 2 values, sets the absolute
#' clipping range of the input intensity values (see
#' \code{\link[EBImage]{normalize}}). Alternatively a names list where each
#' entry corresponds to a channel-specific clipping range.}
#' }
#'
#' @return A CytoImageList object containing the manipulated Images.
#'
#' @examples
#' data(pancreasImages)
#'
#' # Scale images to create segmentation masks
#' cur_files <- list.files(system.file("extdata", package = "cytomapper"),
#'                         pattern = "mask.tiff", full.names = TRUE)
#' x <- loadImages(cur_files)
#' # Error when running plotCells(x)
#' # Therefore scale to account for 16 bit encoding
#' x <- scaleImages(x, 2^16 - 1)
#' plotCells(x)
#'
#' # Default normalization
#' x <- normalize(pancreasImages)
#' plotPixels(x, colour_by = c("H3", "CD99"))
#'
#' # Setting the clipping range
#' x <- normalize(x, inputRange = c(0, 0.9))
#' plotPixels(x, colour_by = c("H3", "CD99"))
#' 
#' # Setting the clipping range per channel
#' x <- normalize(pancreasImages, 
#'                inputRange = list(H3 = c(0, 70), CD99 = c(0, 100)))
#' plotPixels(x, colour_by = c("H3", "CD99"))
#'
#' # Normalizing per image
#' x <- normalize(pancreasImages, separateImages = TRUE)
#' plotPixels(x, colour_by = c("H3", "CD99"))
#'
#' @seealso \code{\link[EBImage]{normalize}} for details on Image normalization
#'
#' @aliases
#' scaleImages scaleImages,CytoImageList-method normalize
#' normalize,CytoImageList-method
#'
#' @docType methods
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
NULL

#' @export
setMethod("scaleImages",
    signature = signature(object="CytoImageList"),
    definition = function(object, value){
        if (length(value) != 1L || !is.numeric(value)) {
            stop("'value' must be a single numeric.")
        }
        cur_out <- endoapply(object, function(y){y * value})
        return(cur_out)
    })

#' @importFrom stats quantile
#' @importFrom EBImage combine abind
normImages <- function(object, separateChannels = TRUE, separateImages = FALSE,
                ft = c(0, 1), inputRange = NULL){

    if (!is.logical(separateChannels) ||
        length(separateChannels) > 1L ||
        is.na(separateChannels)) {
        stop("'separateChannels' only takes TRUE or FALSE.")
    }
    if (!is.logical(separateImages) ||
        length(separateImages) > 1L ||
        is.na(separateImages)) {
        stop("'separateImages' only takes TRUE or FALSE.")
    }
    if (!is.list(inputRange) &&
        !is.null(inputRange) &&
        length(inputRange) != 2) {
        stop("'inputRange' takes a vector of length 2, a list or NULL.")
    }
    
    # Number of frames
    nf <- numberOfFrames(object[[1]])

    # Number of images
    ni <- length(object)
    
    if (is.list(inputRange)) {
        
        if (is.null(channelNames(object))) {
            stop("Please set the 'channelNames' of the CytoImageList object.")
        }
        
        if (!all(names(inputRange) %in% channelNames(object))) {
            stop("The names of 'inputRange' should correspond to the",
            "'channelNames' of the CytoImageList object.")
        }
        
        cur_inputRanges <- vector(mode = "list", 
                                length = length(channelNames(object)))
        names(cur_inputRanges) <- channelNames(object)
        cur_inputRanges[names(inputRange)] <- inputRange
        
        if (separateImages) {
            
            cur_out <- endoapply(object, function(y){
                
                cur_names <- dimnames(y)
                
                y <- lapply(names(cur_inputRanges), function(i){
                    EBImage::normalize(y[,,i], separate = TRUE,
                                       ft = ft, inputRange = cur_inputRanges[[i]])
                })
                y <- combine(y)
                
                dimnames(y) <- cur_names

                return(y)
            })
            
        } else {
            
            cur_range <- cur_inputRanges[unlist(lapply(cur_inputRanges, 
                                                       is.null))]
            
            cur_r <- lapply(names(cur_range), function(x){
                quantile(unlist(lapply(getChannels(object, x), quantile, 
                                       probs = c(0, 1))), c(0,1))
            })
            names(cur_r) <- names(cur_range)
            
            cur_inputRanges[names(cur_r)] <- cur_r
                
            cur_out <- endoapply(object, function(y){
                    
                cur_names <- dimnames(y)
                    
                if (nf == 1) {
                        
                    y <- EBImage::normalize(y,
                                            separate = TRUE, ft=ft,
                                            inputRange = cur_inputRanges[[1]])
                        
                } else {
                        
                    y <- lapply(names(cur_inputRanges), function(i){
                        EBImage::normalize(y[,,i],
                                        separate = TRUE, ft=ft,
                                        inputRange = cur_inputRanges[[i]])
                        })
                    y <- combine(y)
            
                }
                        
                dimnames(y) <- cur_names
                    
                return(y)
            })
        }
        
    } else {
        
        if (separateImages) {
    
            cur_out <- endoapply(object, function(y){
                y <- EBImage::normalize(y, separate = separateChannels,
                                        ft = ft, inputRange = inputRange)
                return(y)
            })
    
        } else {
    
            # Finding the maximum and minimum values
            if (separateChannels && is.null(inputRange)) {
                if (nf == 1) {
                    cur_range <- vapply(object, function(i){
                        quantile(i, probs = c(0, 1))
                    }, FUN.VALUE = numeric(2))
                    cur_range <- quantile(cur_range, c(0, 1))
                } else {
                    cur_range <- vapply(seq_len(nf), function(i){
                        cur_r <- vapply(getChannels(object, i),
                                        function(x){
                                            quantile(x, c(0,1))
                                        }, FUN.VALUE = numeric(2))
                        quantile(cur_r, c(0, 1))
                    }, FUN.VALUE = numeric(2))
                }
            } else if(!separateChannels && is.null(inputRange)) {
                cur_range <- vapply(object, function(i){
                    quantile(i, probs = c(0, 1))
                }, FUN.VALUE = numeric(2))
                cur_range <- quantile(cur_range, c(0, 1))
            }
    
            if (separateChannels) {
    
                cur_out <- endoapply(object, function(y){
    
                    cur_names <- dimnames(y)
    
                    if (nf == 1) {
    
                        if (!is.null(inputRange)) {
                            cur_range <- inputRange
                        } else {
                            cur_range <- as.numeric(cur_range)
                        }
    
                        y <- EBImage::normalize(y,
                                                separate = TRUE, ft=ft,
                                                inputRange = cur_range)
    
                    } else {
    
                        if (!is.null(inputRange)) {
                            y <- EBImage::normalize(y, separate = TRUE, ft=ft,
                                            inputRange = inputRange)
                        } else {
                            y <- lapply(seq_len(nf), function(i){
                                EBImage::normalize(y[,,i],
                                            separate = TRUE, ft=ft,
                                            inputRange = as.numeric(cur_range[,i]))
                            })
                            y <- combine(y)
                        }
    
                    }
    
                    dimnames(y) <- cur_names
    
                    return(y)
                })
    
            } else {
                cur_out <- endoapply(object, function(y){
                    if(is.null(inputRange)){
                        inputRange <- as.numeric(cur_range)
                    }
    
    
                    y <- EBImage::normalize(y, separate = FALSE,
                                            ft=ft, inputRange = inputRange)
    
                    return(y)
                })
    
            }
        }
        
    }
    return(cur_out)
}


#' @export
setMethod("normalize",
    signature = signature(object = "CytoImageList"),
    definition = normImages)
