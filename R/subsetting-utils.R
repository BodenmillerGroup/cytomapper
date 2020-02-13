# -------------------------------------------------------
# Utility functions for ImageList subsetting
# -------------------------------------------------------

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
#' It is only allowed to replace explicitily named entried in named ImageList
#'
#' @section Getting and setting images:
#' TODO
#'
#' @section Getting and setting channels:
#' TODO
#' Discuss adding channels to list
#'
#' @param x TODO
#' @param i,j TODO
#' @param .. TODO
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
#' @rdname ImageList-subsetting
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
                   if(!is.character(i)){
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
#' @rdname ImageList-subsetting
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
#' @rdname ImageList-subsetting
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
                     x <- getChannels(x, !(i %in% channelNames(x)))
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
#' @rdname ImageList-subsetting
#' @export
#' @importFrom methods callNextMethod as validObject
setReplaceMethod("[",
                 signature = signature(x="ImageList"),
                 definition = function(x, i, j, ..., value){
                   .Object <- callNextMethod()
                   .Object <- as(.Object, "ImageList")
                   validObject(.Object)
                   return(.Object)
                 })

#' @rdname ImageList-subsetting
#' @export
#' @importFrom methods callNextMethod as validObject
setReplaceMethod("[[",
                 signature = signature(x="ImageList"),
                 definition = function(x, i, j, ..., value){
                   .Object <- callNextMethod()
                   .Object <- as(.Object, "ImageList")
                   validObject(.Object)
                   return(.Object)
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
#' @importFrom methods is validObject
#' @importFrom EBImage abind
mergeChannels <- function(x, y){
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
