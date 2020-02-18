# -------------------------------------------------------
# Utility functions for ImageList subsetting
# -------------------------------------------------------

#' @title General subsetting methods for ImageList objects
#' @name ImageList-subsetting
#'
#' @description
#' These getter and setter functions are used to extract, replace and merge
#' entries in an \code{\linkS4class{ImageList}} object.
#'
#' @param x,y \code{ImageList} objects
#' @param i integer, logical, character or vector of such indicating which
#'   element(s) to replace or extract
#' @param value an \code{ImageList} or \code{\linkS4class{Image}}
#' object
#'
#' @section Setting and getting images:
#' Functions to extract and replace elements (= images) of an
#' \linkS4class{ImageList} object. In the following code, \code{x} is an
#' ImageList object. The parameter \code{i} indicates the
#' element(s) of \code{x} that should be returned or replaced. Replacement is
#' done by \code{value}, which takes an ImageList or Image object. If
#' \code{length(i) > 0}, \code{value} has to be an ImageList object of
#' \code{length(i)}, otherwise \code{value} allows an ImageList object of length
#' 1 or an Image object.
#'
#' \describe{
#' \item{\code{getImages(x, i)}:}{Returns image(s) indicated by \code{i} of
#' the ImageList object \code{x}}
#' \item{\code{setImages(x, i) <- value}:}{Replaces the image(s) indicated by
#' \code{i} of the ImageList object \code{x} with \code{value}. For this,
#' \code{value} needs to have the same length as \code{i}}
#' }
#'
#' These setter and getter functions are the recommended way of extracting and replacing
#' images in an ImageList object. Alternatively, the standard operations via \code{`[`}, \code{`[[`},
#' \code{`[<-`} and \code{`[[<-`} can be performed (see \code{?\link{List}} for S4Vectors subsetting
#' functionality). However, these operations do not change element names during replacment
#' calls. The \code{setImages()} function makes sure that element names are replaced if
#' \code{value} is named or of \code{i} is a character or vector of characters.
#'
#' @section Getting and setting channels:
#' Functions to extract and replace channels of an \linkS4class{ImageList}
#' object. Here, \code{x} is an \linkS4class{ImageList} object. The parameter
#' \code{i} indicates the channels of \code{x} that should be returned or
#' replaced. Replacement is done by \code{value}, which takes an ImageList object.
#' The ImageList object \code{value} needs to have the same length as \code{x}.
#' Furthermore, the number of channels in \code{value} should be identical to
#' \code{length(i)}.
#'
#' \describe{
#' \item{\code{getChannels(x, i)}:}{Returns channel(s) indicated by \code{i} of
#' the ImageList object \code{x}}
#' \item{\code{setChannels(x, i) <- value}:}{Replaces the channel(s) indicated by
#' \code{i} of the ImageList object \code{x} with \code{value}. For this,
#' \code{value} needs to have the same length as \code{i} and the same
#' number of channels as \code{length(i)}.}
#' }
#'
#' The \code{setChannels()} setter function does not allow adding new channels to
#' the ImageList object. For this operation, the \code{mergeChannels} function
#' was implemented (see below).
#'
#' @section Merging images:
#' Merging images is possible by merging two or more ImageList objects via:
#'
#' \describe{
#' \item{\code{c(x,y)}:}{Returns an composite ImageList object with
#' elements of both ImageList objects \code{x} and \code{y}.
#' More than two ImageList objects can be merged in that way.}
#' }
#'
#' @section Merging channels:
#' Merging channels is possible via:
#'
#' \describe{
#' \item{\code{mergeChannels(x,y)}:}{Returns an ImageList in which
#' the channels of the ImageList object \code{y} have been appended
#' to the channels of the ImageList object \code{x}.
#' Only channels of two ImageList objects can be merged in that way.}
#' }
#'
#' @return An ImageList object
#'
#' @examples
#' data("pancreasImages")
#'
#' # Get images
#' getImages(pancreasImages, 1)
#' getImages(pancreasImages, "A02_imc")
#' getImages(pancreasImages, 1:2)
#' getImages(pancreasImages, c("A02_imc", "D01_imc"))
#' getImages(pancreasImages, grepl("A02_imc", names(pancreasImages)))
#'
#' # Set images
#' setImages(pancreasImages, 1) <- pancreasImages[1]
#' setImages(pancreasImages, "F02_imc") <- pancreasImages[1]
#' setImages(pancreasImages, "F02_imc") <- NULL
#'
#' # Get channels
#' getChannels(pancreasImages, 1)
#' getChannels(pancreasImages, "SMA")
#' getChannels(pancreasImages, c("SMA", "INS"))
#'
#' # Set channels
#' channel1 <- getChannels(pancreasImages, 1)
#' setChannels(pancreasImages, 1) <- channel1
#' channelINS <- getChannels(pancreasImages, "INS")
#' setChannels(pancreasImages, "CD38") <- channelINS
#' setChannels(pancreasImages, "CD38") <- NULL
#'
#' # Merge images
#' c(pancreasImages[c(1,3)], pancreasImages[2])
#'
#' # Merge channels
#' channel12 <- getChannels(pancreasImages, c(1,2))
#' channel34 <- getChannels(pancreasImages, c(3,4))
#' mergeChannels(channel12, channel34)
#'
#' @aliases
#' getImages getChannels mergeChannels
#' setImages<- setChannels<-
#' getImages,ImageList-method
#' setImages<-,ImageList-method
#' getChannels,ImageList-method
#' setChannels<-,ImageList-method
#' [<-,ImageList,ANY,ANY,ImageList-method
#' [[<-,ImageList,ANY,ANY-method
#'
#' @docType methods
#'
#' @author
#' Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
NULL

#' @export
setMethod("getImages",
          signature = signature(x="ImageList"),
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
                 signature = signature(x="ImageList"),
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

                   # Coerce Image to ImageList
                   if(is(value, "Image")){
                     value <- ImageList(value)
                   }

                   # Set ImageList
                   x[i] <- value
                   if(!is.null(cor_names)){
                     names(x) <- as.character(cor_names)
                   }

                   return(x)
                 })

#' @export
#' @importFrom S4Vectors endoapply
#' @importFrom methods validObject
setMethod("getChannels",
          signature = signature(x="ImageList"),
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

            x <- S4Vectors::endoapply(x, function(y){
              y[,,i,drop=FALSE]
            })

            validObject(x)

            return(x)
          })

#' @export
#' @importFrom S4Vectors mendoapply
#' @importFrom methods is validObject
setReplaceMethod("setChannels",
                 signature = signature(x="ImageList"),
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
                 signature = c("ImageList", "ANY", "ANY", "ImageList"),
                 definition = function(x, i, j, ..., value){
                   .Object <- callNextMethod()
                   .Object <- as(.Object, "ImageList")
                   validObject(.Object)
                   return(.Object)
                 })
#' @export
#' @importFrom methods callNextMethod as validObject
setReplaceMethod("[[",
                 signature = c("ImageList", "ANY", "ANY"),
                 definition = function(x, i, j, ..., value){
                   .Object <- callNextMethod()
                   .Object <- as(.Object, "ImageList")
                   validObject(.Object)
                   return(.Object)
                 })

#' @export
#' @importFrom S4Vectors mendoapply
#' @importFrom methods is validObject
#' @importFrom EBImage abind
mergeChannels <- function(x, y){
  if(!is(x, "ImageList") || !is(y, "ImageList")){
    stop("'x' and 'y' must be ImageList objects")
  }

  # Further checks
  if(length(x) != length(y)){
    stop("Invalid merge operation: \n",
         "'y' needs to have same length as 'x'")
  }

  x <- S4Vectors::mendoapply(function(k, u){
    k <- abind(k,u)
    return(k)
  }, x, y)

  validObject(x)

  return(x)
}
