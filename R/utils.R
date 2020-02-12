# Utility functions for ImageList and Image class objects

#' @title Getting and setting the channel names
#' @name channelNames
#' @description TODO
#'
#' @details
#' # TODO
#'
#' @param x TODO
#' @param value TODO
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch}
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
NULL

#' @export
#' @rdname channelNames
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
#' @rdname channelNames
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
#' @rdname channelNames
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
#' @rdname channelNames
#' @importFrom EBImage Image
#' @importFrom S4Vectors endoapply
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

#' @title Merging channels of two ImageList objects
#' @name mergeChannels
#' @description TODO
#'
#' @details
#' # TODO
#'
#' @param x TODO
#' @param y TODO
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch}
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
#' @export
#'
#' @importFrom S4Vectors mendoapply
mergeChannels <- function(x, y){
                   if(missing(i) || is.null(x)){
                     return(x)
                   }

                   if(!is(x, "ImageList") || !is(x, "ImageList")){
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

