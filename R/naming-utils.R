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
#' channelNames names
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

