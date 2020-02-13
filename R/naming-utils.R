# Utility functions for ImageList and Image class objects

#' @title Getting and setting the channel and image names
#' @name ImageList-naming
#'
#' @description TODO # Mention Image and ImageList
#'
#' @section Getters:
#' \describe{
#' \item{\code{channelNames(x)}:}{Returns the names of all channels stored in
#' the Image or ImageList object \code{x}}
#' \item{\code{channelNames(x) <- value}:}{Replaces the channel names of the Image or ImageList object
#' \code{x} with \code{values}. For this, \code{value} needs to have the same
#' length as the number of channels in \code{x}}
#' \item{\code{names(x)}:}{Returns the names of all images stored in
#' the ImageList object \code{x}}
#' \item{\code{names(x) <- value}:}{Replaces the image names of
#' \code{x} with \code{values}. For this, \code{value} needs to have the same
#' length as the ImageList object \code{x}}
#' }
#'
#' @aliases
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
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
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
              x <- Image(x, dim = c(dim(x)[1], dim(x)[2], 1))
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
                       y <- Image(y, dim = c(dim(y)[1], dim(y)[2], 1))
                     })
                   }

                   x <- S4Vectors::endoapply(x, function(y){
                     dimnames(y)[[3]] <- as.character(value)
                     y
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

