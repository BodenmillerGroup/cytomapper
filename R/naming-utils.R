# Utility functions for ImageList and Image class objects

#' @title Getting and setting the channel and image names
#' @name ImageList-naming
#'
#' @description TODO
#'
#' @details
#' # TODO
#'
#' @param x TODO
#' @param value TODO
#'
#' @aliases channelNames names
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch}
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
NULL

#' @export
#' @rdname ImageList-naming
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
#' @rdname ImageList-naming
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
#' @rdname ImageList-naming
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
#' @rdname ImageList-naming
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

#' @rdname ImageList-naming
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

