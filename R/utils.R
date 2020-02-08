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
            if(length(dim(x)) == 2){
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
            if(length(dim(x)) == 2){
              x <- Image(x, dim = c(dim(x)[1], dim(x)[2], 1))
            }

            dimnames(x)[[3]] <- value
            return(x)
          })

#' @export
#' @rdname channelNames
setMethod("channelNames",
          signature = signature(x="ImageList"),
          definition =  function(x){
            if(length(dim(x[[1]])) == 2){
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
                   if(length(dim(x[[1]])) == 2){
                     x <- S4Vectors::endoapply(x, function(y){
                       y <- Image(y, dim = c(dim(y)[1], dim(y)[2], 1))
                     })
                   }

                   x <- S4Vectors::endoapply(x, function(y){
                     dimnames(y)[[3]] <- value
                     y
                   })
                   return(x)
                 })

#' @title Getting and setting individual channels and images
#' @name ImageList-subsetting
#'
#' @description
#' These getter and setter functions are used to extracting and replacing
#' entries in an \code{\linkS4class{ImageList}} object.
#'
#'
#' @details
#' These methods are preferred over subsetting via `[<-` or `[[<-`, which by default
#' does not replace the entry names.
#'
#' The `[<-` or `[[<-` functions for the \code{\linkS4class{ImageList}} object
#' have been adjusted to call getImages and setImages.
#'
#' @param x TODO
#' @param value TODO
#'
#' @return An ImageList object
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch}
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
NULL

#' @export
#' @rdname ImageList-subsetting
setMethod("getImages",
          signature = signature(x="ImageList"),
          definition = function(x, value){

            ans.list <- as.list(x)
            ans.mcols <- mcols(x)

            # Initial checks
            if(is.null(value) || (!is.numeric(value)  &&
                                  !is.character(value) )){
              stop("Invalid argument for 'value'")
            }

            cur_list <- ans.list[value]
            if(is.null(ans.mcols)){
              cur_mcols <- NULL
            } else {
              cur_mcols <- ans.mcols[value,]
            }

            cur_ImageList <- ImageList(cur_list,
                                       elementMetadata = cur_mcols,
                                       channelNames = channelNames(x))
            return(cur_ImageList)
          })

#' @export
#' @rdname ImageList-subsetting
setReplaceMethod("setImages",
          signature = signature(x="ImageList"),
          definition = function(x, value){

          })

#' @export
#' @rdname ImageList-subsetting
setMethod("getChannels",
          signature = signature(x="ImageList"),
          definition = function(x, value){
            # Initial checks
            if(is.null(value) || (!is.numeric(value) &&
                                  !is.character(value) )){
              stop("Invalid argument for 'value'")
            }
            if(is.character(value) &&
               sum(!(value %in% channelNames(x))) > 0){
              stop("'value' not in channelNames(x)")
            }

            ans <- S4Vectors::endoapply(x, function(y){
              y[,,value]
            })

            return(ans)
          })

#' @export
#' @rdname ImageList-subsetting
setReplaceMethod("setChannels",
          signature = signature(x="ImageList"),
          definition = function(x, value){

          })



