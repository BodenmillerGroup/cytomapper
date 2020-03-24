# Utility functions for ImageList and Image class objects

#' @title Getting and setting the channel and image names
#' @name ImageList-naming
#'
#' @description
#' Methods to get and set the names of individual channels or the names of
#' individual images.
#'
#' @section Setting and getting the channel names:
#' In the following code, \code{x} is either an \linkS4class{ImageList} or
#' \linkS4class{Image} object containing one or multiple channels.
#' The channel names can be replaced by \code{value}, which contains a character
#' vector of the same length as the number of channels in the image(s).
#' \describe{
#' \item{\code{channelNames(x)}:}{Returns the names of all channels stored in
#' \code{x}}
#' \item{\code{channelNames(x) <- value}:}{Replaces the channel names of
#' \code{x} with \code{values}. For this, \code{value} needs to have the same
#' length as the number of channels in \code{x}}
#' }
#'
#' @section Setting and getting the image names:
#' Here, \code{x} is either a \linkS4class{ImageList} object. The element names
#' can be replaced by \code{value}, which contains a character vector of the
#' same length as the number of images. In case of the ImageList object,
#' elements are always images.
#' \describe{
#' \item{\code{names(x)}:}{Returns the names of all images stored in \code{x}}
#' \item{\code{names(x) <- value}:}{Replaces the image names of
#' \code{x} with \code{values}. For this, \code{value} needs to have the same
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
#' channelNames,ImageList-method
#' channelNames<-,ImageList-method
#' channelNames,Image-method
#' channelNames<-,Image-method
#' names,ImageList-method
#' names<-,ImageList-method
#'
#' @docType methods
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch}
NULL

#' @export
setMethod("channelNames",
          signature = signature(x="Image"),
          definition = function(x){
            if(length(dim(x)) == 2L){
              return(NULL)
            } else {
              return(dimnames(x)[[3]])
            }
            })

#' @export
#' @importFrom EBImage Image
setReplaceMethod("channelNames",
          signature = signature(x="Image"),
          definition = function(x, value){
            # Image needs to be expanded to store channel names
            if(length(dim(x)) == 2L){
              cur_Image <- Image(x, dim = c(dim(x)[1], dim(x)[2], 1))
              dimnames(cur_Image) <- c(dimnames(x), NULL)
              x <- cur_Image
            }

            dimnames(x)[[3]] <- as.character(value)
            return(x)
          })

#' @export
setMethod("channelNames",
          signature = signature(x="ImageList"),
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
                 signature = signature(x="ImageList"),
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
          signature = signature(x="ImageList"),
          definition = function(x){
            callNextMethod()
          })

#' @export
#' @importFrom methods callNextMethod as validObject
setReplaceMethod("names",
                 signature = signature(x="ImageList"),
                 definition = function(x, value){
                   .Object <- callNextMethod()
                   .Object <- as(.Object, "ImageList")
                   validObject(.Object)
                   return(.Object)
                 })

#' @title Manipulating ImageList objects
#' @name ImageList-manipulation
#'
#' @description Methods to change pixel values in ImageList objects. In the
#'   following sections, \code{object} is an \linkS4class{ImageList} object
#'   containing one or multiple channels.
#'
#' @section Image scaling:
#' In some cases, images need to be scaled by a constant
#' (e.g. 2^16-1 = 65535) \code{value} to revert them back to the original
#' pixel values after reading them in.
#' \describe{
#'   \item{\code{scaleImages(object, value)}:}{Scales all images in the
#'   \linkS4class{ImageList} object \code{object} by \code{value}.}
#' }
#'
#' @section Image normalization:
#' Linear scaling of the intensity values of each \linkS4class{Image} contained
#' in an \linkS4class{ImageList} \code{object} to a specific range. Images can
#' either be scaled to the minimum/maximum value per channel or across all
#' channels (default \code{separateChannels = TRUE}). Also, Images can be scaled
#' to the minimum/maximum value per image or across all images (default
#' \code{separateImages = FALSE}). The later allows the visual comparison of
#' intensity values across images.
#'
#' To clip the images before normalization, the \code{inputRange} or
#' \code{percentileRange} paramters can be set. The main difference between
#' these two options is a hard (inputRange) or percentile (percentileRange)
#' clipping range. \code{percentileRange} allows setting the minimum and maximum
#' range of values in terms of percentiles (e.g. \code{c(0, 0.99)} for the 0 and
#' 99th percentile clipping).
#'
#' \code{normalize(object, separateChannels = TRUE, separateImages = FALSE,
#'   ft = c(0, 1), percentileRange = c(0, 1), inputRange = NULL)}:
#'
#' \describe{
#' \item{\code{object}:}{An ImageList object}
#' \item{\code{separateChannels}:}{Logical if pixel values should be normalized
#' per channel (default) or across all channles.}
#' \item{\code{separateImages}:}{Logical if pixel values should be normalized
#' per image or across all images (default).}
#' \item{\code{ft}:}{Numeric vector of 2 values, target minimum and maximum
#' intensity values after normalization (see \code{\link[EBImage]{normalize}}).}
#' \item{\code{percentileRange}:}{Numeric vector of 2 values between 0 and 1,
#' sets the precentile clipping range of the input intensity values.}
#' \item{\code{inputRange}:}{Numeric vector of 2 values, sets the absolute
#' clipping range of the input intensity values (see
#' \code{\link[EBImage]{normalize}}).}
#' }
#'
#' @return An ImageList object containing the manipulated Images
#'
#' @examples
#' data(pancreasImages)
#'
#' # Scale images to create segmentation masks
#' cur_files <- list.files(system.file("extdata", package = "SingleCellMapper"),
#'                         pattern = "mask.tiff", full.names = TRUE)
#' x <- loadImages(cur_files)
#' # Error when running plotCells(x)
#' # Therefore scale to account for 16 bit encoding
#' x <- scaleImages(x, 2^16 - 1)
#' plotCells(x)
#'
#' # Default normalization
#' x <- normalize(pancreasImages)
#' plotPixels(x, colour_by = c("H3", "SMA"))
#'
#' # Setting the clipping range
#' x <- normalize(pancreasImages, percentileRange = c(0, 0.99))
#' plotPixels(x, colour_by = c("H3", "SMA"))
#'
#' # Normalizing per image
#' x <- normalize(pancreasImages, separateImages = TRUE,
#'                percentileRange = c(0, 0.99))
#' plotPixels(x, colour_by = c("H3", "SMA"))
#'
#' @seealso \code{\link[EBImage]{normalize}} for details on Image normalization
#'
#' @aliases scaleImages scaleImages,ImageList-method normalize
#'   normalize,ImageList-method
#'
#' @docType methods
#'
#' @author Nils Eling \email{nils.eling@@dqbm.uzh.ch}
NULL

#' @export
setMethod("scaleImages",
          signature = signature(object="ImageList"),
          definition = function(object, value){
            if(length(value) != 1L || !is.numeric(value)){
              stop("'value' must be a single numeric.")
            }
            cur_out <- endoapply(object, function(y){y * value})
            return(cur_out)
          })

#' @importFrom stats quantile
normImages <- function(object, separateChannels = TRUE, separateImages = FALSE,
                       ft = c(0, 1), percentileRange = c(0, 1), inputRange = NULL){

  if(!is.logical(separateChannels)){
    stop("'separateChannels' only takes TRUE or FALSE.")
  }
  if(!is.logical(separateImages)){
    stop("'separateImages' only takes TRUE or FALSE.")
  }

  if(!is.null(percentileRange)){
    if(!is.numeric(percentileRange) ||
       length(percentileRange) != 2L ||
       min(percentileRange) < 0 ||
       max(percentileRange) > 1){
      stop("'percentileRange' takes two numeric values indicating \n",
           "the lower and upper percentile for clipping")
    }
    if(diff(percentileRange) <= 0){
      stop("Invalid input for 'percentileRange'")
    }
  }

  if((!is.null(percentileRange) && !is.null(inputRange)) ||
     (is.null(percentileRange) && is.null(inputRange))){
    stop("Please specify either 'percentileRange' or 'inputRange'")
  }

  if(separateImages){
    if(separateChannels){

      cur_out <- endoapply(object, function(y){
        if(!is.null(inputRange)){
          y <- EBImage::normalize(y, separate = TRUE, ft=ft, inputRange)
        } else {
          for(i in seq_len(dim(y)[3])){
            cur_min <- quantile(y[,,i], probs = percentileRange[1])
            cur_max <- quantile(y[,,i], probs = percentileRange[2])
            if(cur_min == cur_max){
              stop("Minimum and maximum value for the indicated percentiles are identical.")
            }
            y[,,i] <- EBImage::normalize(y[,,i], separate = TRUE, ft=ft,
                                inputRange = c(cur_min, cur_max))
          }
        }
        return(y)
      })

    } else {

      cur_out <- endoapply(object, function(y){
        if(!is.null(inputRange)){
          y <- normalize(y, separate = FALSE, ft=ft, inputRange)
        } else {
          cur_min <- quantile(y, probs = percentileRange[1])
          cur_max <- quantile(y, probs = percentileRange[2])
          if(cur_min == cur_max){
            stop("Minimum and maximum value for the indicated percentiles are identical.")
          }
          y <- EBImage::normalize(y, separate = FALSE, ft=ft,
                         inputRange = c(cur_min, cur_max))
        }

        return(y)

      })
    }
  } else {
    if(separateChannels){
      if(!is.null(percentileRange)){
        min_vector <- NULL
        max_vector <- NULL
        for(i in seq_len(numberOfFrames(object[[1]]))){
          cur_dist <- unlist(lapply(getChannels(object, i), as.numeric))
          cur_min <- quantile(cur_dist, percentileRange[1])
          cur_max <- quantile(cur_dist, percentileRange[2])
          min_vector <- c(min_vector, cur_min)
          max_vector <- c(max_vector, cur_max)
          if(cur_min == cur_max){
            stop("Minimum and maximum value for the indicated percentiles are identical.")
          }
        }
      }

      cur_out <- endoapply(object, function(y){
        if(!is.null(inputRange)){
          y <- normalize(y, separate = TRUE, ft=ft, inputRange)
        } else {
          for(i in seq_len(dim(y)[3])){
            y[,,i] <- EBImage::normalize(y[,,i], separate = TRUE, ft=ft,
                                inputRange = c(min_vector[i], max_vector[i]))
          }
        }
        return(y)
      })
    } else {
      if(!is.null(percentileRange)){
        cur_dist <- unlist(lapply(object, as.numeric))
        cur_min <- quantile(cur_dist, probs = percentileRange[1])
        cur_max <- quantile(cur_dist, probs = percentileRange[2])
        if(cur_min == cur_max){
          stop("Minimum and maximum value for the indicated percentiles are identical.")
        }
      }

      cur_out <- endoapply(object, function(y){
        if(!is.null(inputRange)){
          y <- EBImage::normalize(y, separate = FALSE, ft=ft, inputRange)
        } else {
          y <- EBImage::normalize(y, separate = FALSE, ft=ft,
                         inputRange = c(cur_min, cur_max))
        }
        return(y)
      })

    }
  }
  return(cur_out)
}


#' @export
setMethod("normalize",
          signature = signature(object = "ImageList"),
          definition = normImages)
