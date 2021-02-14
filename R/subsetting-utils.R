# -------------------------------------------------------
# Utility functions for CytoImageList subsetting
# -------------------------------------------------------

#' @title General subsetting methods for CytoImageList objects
#' @name CytoImageList-subsetting
#'
#' @description
#' These getter and setter functions are used to extract, replace and merge
#' entries in a \code{\linkS4class{CytoImageList}} object.
#'
#' @param x,y \code{CytoImageList} objects
#' @param i integer, logical, character or vector of such indicating which
#' element(s) to replace or extract
#' @param value a \code{CytoImageList} or \code{\linkS4class{Image}}
#' object
#'
#' @section Setting and getting images:
#' Functions to extract and replace elements (= images) of a
#' \linkS4class{CytoImageList} object. In the following code, \code{x} is a
#' CytoImageList object. The parameter \code{i} indicates the element(s) of
#' \code{x} that should be returned or replaced. Replacement is done by
#' \code{value}, which takes a CytoImageList or Image object. If \code{length(i)
#' > 0}, \code{value} has to be a CytoImageList object of \code{length(i)},
#' otherwise \code{value} allows a CytoImageList object of length 1 or an Image
#' object. If an Image object is provided, only the image entry in the
#' CytoImageList object is replaced, not the corresponding elementMetadata
#' entry.
#'
#' \describe{
#' \item{\code{getImages(x, i)}}{Returns image(s) indicated by \code{i} of
#' the CytoImageList object \code{x}}
#' \item{\code{setImages(x, i) <- value}}{Replaces the image(s) indicated by
#' \code{i} of the CytoImageList object \code{x} with \code{value}. For this,
#' \code{value} needs to have the same length as \code{i}}
#' }
#'
#' These setter and getter functions are the recommended way of extracting and
#' replacing images in a CytoImageList object. Alternatively, the standard
#' operations via \code{`[`}, \code{`[[`}, \code{`[<-`} and \code{`[[<-`} can be
#' performed (see \code{?\link{List}} for S4Vectors subsetting functionality).
#' However, these operations do not change element names during replacment
#' calls. The \code{setImages()} function makes sure that element names are
#' replaced if \code{value} is named or if \code{i} is a character or vector of
#' characters.
#'
#' @section Getting and setting channels:
#' Functions to extract and replace channels of a \linkS4class{CytoImageList}
#' object. Here, \code{x} is a \linkS4class{CytoImageList} object. The parameter
#' \code{i} indicates the channels of \code{x} that should be returned or
#' replaced. Replacement is done by \code{value}, which takes a CytoImageList
#' object. The CytoImageList object \code{value} needs to have the same length
#' as \code{x}. Furthermore, the number of channels in \code{value} should be
#' identical to \code{length(i)}.
#'
#' \describe{
#' \item{\code{getChannels(x, i)}}{Returns channel(s) indicated by \code{i} of
#' the CytoImageList object \code{x}}
#' \item{\code{setChannels(x, i) <- value}}{Replaces the channel(s) indicated
#' by \code{i} of the CytoImageList object \code{x} with \code{value}. For this,
#' \code{value} needs to have the same length as \code{i} and the same
#' number of channels as \code{length(i)}.}
#' }
#'
#' The \code{setChannels()} setter function does not allow adding new channels
#' to the CytoImageList object. For this operation, the \code{mergeChannels}
#' function was implemented (see below).
#'
#' @section Merging images:
#' Merging images is possible by merging two or more CytoImageList objects via:
#'
#' \describe{
#' \item{\code{c(x, y)}}{Returns an composite CytoImageList object with
#' elements of both CytoImageList objects \code{x} and \code{y}.
#' More than two CytoImageList objects can be merged in that way.}
#' }
#'
#' @section Merging channels:
#' Merging channels is possible via:
#'
#' \describe{
#' \item{\code{mergeChannels(x, y, h5FilesPath = NULL)}:}{
#' Returns a CytoImageList in which the channels of the CytoImageList object
#' \code{y} have been appended to the channels of the CytoImageList object
#' \code{x}. Only channels of two CytoImageList objects can be merged in that
#' way. The \code{h5FilesPath} argument can be ignored unless images are stored 
#' on disk. To avoid overriding the .h5 files, one needs to specify a new
#' location where the merged images are stored on disk. 
#' }
#' }
#'
#' @return A CytoImageList object
#'
#' @examples
#' data("pancreasImages")
#'
#' # Get images
#' getImages(pancreasImages, 1)
#' getImages(pancreasImages, "E34_imc")
#' getImages(pancreasImages, 1:2)
#' getImages(pancreasImages, c("E34_imc", "G01_imc"))
#' getImages(pancreasImages, grepl("E34_imc", names(pancreasImages)))
#'
#' # Set images
#' setImages(pancreasImages, 1) <- pancreasImages[1]
#' setImages(pancreasImages, "J02_imc") <- pancreasImages[1]
#' setImages(pancreasImages, "J02_imc") <- NULL
#'
#' # Get channels
#' getChannels(pancreasImages, 1)
#' getChannels(pancreasImages, "CD99")
#' getChannels(pancreasImages, c("CD99", "PIN"))
#'
#' # Set channels
#' channel1 <- getChannels(pancreasImages, 1)
#' setChannels(pancreasImages, 1) <- channel1
#' channelPIN <- getChannels(pancreasImages, "PIN")
#' setChannels(pancreasImages, "CD8a") <- channelPIN
#' setChannels(pancreasImages, "CD8a") <- NULL
#'
#' # Merge images
#' data("pancreasImages")
#' c(pancreasImages[c(1,3)], pancreasImages[2])
#'
#' # Merge channels
#' channel12 <- getChannels(pancreasImages, c(1,2))
#' channel34 <- getChannels(pancreasImages, c(3,4))
#' mergeChannels(channel12, channel34)
#' 
#' # Merge channels on disk
#' cur_images <- CytoImageList(pancreasImages,
#'                      on_disk = TRUE, 
#'                      h5FilesPath = HDF5Array::getHDF5DumpDir())
#' channel12 <- getChannels(cur_images, c(1,2))
#' channel34 <- getChannels(cur_images, c(3,4))
#' 
#' # This will overwrite the initial .h5 files
#' mergeChannels(channel12, channel34,
#'                 h5FilesPath = HDF5Array::getHDF5DumpDir())
#'
#' @aliases
#' getImages getChannels mergeChannels
#' setImages<- setChannels<-
#' getImages,CytoImageList-method
#' setImages<-,CytoImageList-method
#' getChannels,CytoImageList-method
#' setChannels<-,CytoImageList-method
#' [<-,CytoImageList,ANY,ANY,CytoImageList-method
#' [[<-,CytoImageList,ANY,ANY-method
#'
#' @docType methods
#'
#' @author
#' Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
NULL

#' @export
setMethod("getImages",
    signature = signature(x="CytoImageList"),
    definition = function(x, i){

        if(missing(i) || is.null(x)){
            return(x)
        }

        # Initial checks
        if(is.null(i) || (!is.numeric(i)  &&
            !is.character(i) &&
            !is.logical(i))){
            stop("Invalid subsetting. \n",
                "Only logicals, characters and integers are supported")
        }

        return(x[i])
    })

#' @export
setReplaceMethod("setImages",
    signature = signature(x="CytoImageList"),
    definition = function(x, i, value){

        if(missing(i) || is.null(x)){
            return(x)
        }

        if(is.null(i) || (!is.numeric(i)  &&
            !is.character(i) &&
            !is.logical(i))){
            stop("Invalid subsetting. \n",
                "Only logicals, characters and integers are supported")
        }

        # Further checks
        .valid.Image.setting(x, i, value)

        # Set correct names
        cor_names <- NULL
        if(!is.character(i) && !is.null(value)){
            cor_names <- names(x)
            names(cor_names) <- cor_names
            cor_names[i] <- names(value)
        }

        # If value is Image_OR_DelayedArray, only the image will be replaced
        if(is(value, "Image_OR_DelayedArray")){
            x[[i]] <- value
        } else {
            x[i] <- value
        }

        if(!is.null(cor_names)){
            names(x) <- as.character(cor_names)
        }

        return(x)
    })

#' @export
#' @importFrom S4Vectors endoapply
#' @importFrom methods validObject
setMethod("getChannels",
    signature = signature(x="CytoImageList"),
    definition = function(x, i){
        # Initial checks
        if(is.null(i) || (!is.numeric(i)  &&
            !is.character(i) &&
            !is.logical(i))){
            stop("Invalid subsetting. \n",
                "Only logicals, characters and integers are supported")
        }

        if(is.character(i) &&
            sum(!(i %in% channelNames(x))) > 0){
            stop("'i' not in channelNames(x)")
        }

        if(length(dim(x[[1]])) >= 3){
            
            if (is(x[[1]], "Image")) {
                x <- S4Vectors::endoapply(x, function(y){
                    y[,,i,drop=FALSE]
                })
            } else {
                x@listData <- lapply(x, function(y){
                    y[,,i,drop=FALSE]
                })
            }

        } else {
            if(i != 1L){
                stop(paste("For single-channel images,",
                        "channels must be named or 'i' needs to be 1."))
            }
        }

        validObject(x)

        return(x)
    })

#' @export
#' @importFrom S4Vectors mendoapply
#' @importFrom methods is validObject
setReplaceMethod("setChannels",
    signature = signature(x="CytoImageList"),
    definition = function(x, i, value){

        if(missing(i) || is.null(x)){
            return(x)
        }

        if(is.null(i) || (!is.numeric(i)  &&
            !is.character(i) &&
            !is.logical(i))){
            stop("Invalid subsetting. \n",
                "Only logicals, characters and integers are supported")
            }

        # Further checks
        .valid.Channel.setting(x, i, value)

        # Use getChannels function if value is NULL
        if(is.null(value) && is.numeric(i)){
            cur_ind <- seq_len(length.out = dim(x[[1]])[3])
            cur_ind <- cur_ind[-i]
            x <- getChannels(x, cur_ind)
        } else if(is.null(value) && is.character(i)){
            x <- getChannels(x, !(channelNames(x) %in% i))
        } else {
            # Set correct names
            cor_names <- NULL
            if(!is.character(i)){
                cor_names <- channelNames(x)
                cor_names[i] <- channelNames(value)
            }

            x <- S4Vectors::mendoapply(function(k, u){
                k[,,i] <- u
                return(k)
            }, x, value)

            if(!is.null(cor_names)){
                channelNames(x) <- as.character(cor_names)
            }
        }

        validObject(x)

        return(x)
    })

# Expand bracket functions to check if valid object is returned
#' @export
#' @importFrom methods callNextMethod as validObject
setReplaceMethod("[",
    signature = c("CytoImageList", "ANY", "ANY", "CytoImageList"),
    definition = function(x, i, j, ..., value){
        .Object <- callNextMethod()
        .Object <- as(.Object, "CytoImageList")
        validObject(.Object)
        return(.Object)
    })

#' @export
#' @importFrom methods callNextMethod as validObject
setReplaceMethod("[[",
    signature = c("CytoImageList", "ANY", "ANY"),
    definition = function(x, i, j, ..., value){
        .Object <- callNextMethod()
        .Object <- as(.Object, "CytoImageList")
        validObject(.Object)
        return(.Object)
    })

#' @export
#' @importFrom S4Vectors mendoapply
#' @importFrom methods is validObject
#' @importFrom EBImage abind
mergeChannels <- function(x, y, h5FilesPath = NULL){
    if(!is(x, "CytoImageList") || !is(y, "CytoImageList")){
        stop("'x' and 'y' must be CytoImageList objects")
    }

    # Further checks
    if(length(x) != length(y)){
        stop("Invalid merge operation: \n",
            "'y' needs to have same length as 'x'")
    }
    
    if(!all(as.character(lapply(x, class)) == 
            as.character(lapply(y, class)))){
        stop("Invalid merge operation: \n",
             "'y' needs to contain the same class objects as 'x'")
    }

    
    if (is(x[[1]], "Image")) {
        x <- S4Vectors::mendoapply(function(k, u){
            k <- abind(k,u)
            return(k)
        }, x, y)
    } else {
        
        if (is.null(h5FilesPath)){
            stop("Please specify the filepath \n", 
                 "where the merged images should be stored.")
        }
        
        x@listData <- mapply(function(cur_name, k, u){
            cur_array <- abind(as.array(k), as.array(u))
            
            cur_file <- file.path(h5FilesPath, paste0(cur_name, ".h5"))
            
            # Check if file already exists
            # If so, delete them
            if (file.exists(cur_file)) {
                file.remove(cur_file)
            }
            
            writeHDF5Array(DelayedArray(cur_array), 
                           filepath = cur_file,
                           name = cur_name,
                           with.dimnames = TRUE)
    
        }, names(x), x, y, SIMPLIFY = FALSE)
    }
        
    validObject(x)

    return(x)
}
