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
#' @param x TODO
#' @param i TODO
#' @param value TODO
#' @param drop TODO
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
            if(is.null(i) || (!is.integer(i)  &&
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

                   if(is.null(i) || (!is.integer(i)  &&
                                     !is.character(i) &&
                                     !is.logical(i))){
                     stop("Invalid subsetting. \n",
                          "Only logicals, characters and integers are supported")
                   }

                   # Further checks
                   .valid.Image.setting(x, i, value)

                   # Set correct names
                   if(!is.character(i)){
                     cor_names <- names(x)
                     names(cor_names) <- cor_names
                     cor_names[names(value)] <- names(value)
                   }

                   # Subset ImageList
                   x[i] <- value
                   if(!missing(cor_names)){
                     names(x) <- as.character(cor_names)
                   }

                   return(x)
                 })

#' @export
#' @rdname ImageList-subsetting
#' @importFrom S4Vectors endoapply
setMethod("getChannels",
          signature = signature(x="ImageList"),
          definition = function(x, i, drop=FALSE){
            # Initial checks
            if(is.null(i) || (!is.integer(i)  &&
                              !is.character(i) &&
                              !is.logical(i))){
              stop("Invalid subsetting. \n",
                   "Only logicals, characters and integers are supported")
            }

            if(is.character(i) &&
               sum(!(i %in% channelNames(x))) > 0){
              stop("'i' not in channelNames(x)")
            }

            ans <- S4Vectors::endoapply(x, function(y){
              y[,,i,drop=drop]
            })

            return(ans)
          })

#' @export
#' @rdname ImageList-subsetting
#' @importFrom S4Vectors mendoapply
setReplaceMethod("setChannels",
                 signature = signature(x="ImageList"),
                 definition = function(x, i, value){

                   if(missing(i) || is.null(x)){
                     return(x)
                   }

                   if(is.null(i) || (!is.integer(i)  &&
                                     !is.character(i) &&
                                     !is.logical(i))){
                     stop("Invalid subsetting. \n",
                          "Only logicals, characters and integers are supported")
                   }

                   # Further checks
                   .valid.Channel.setting(x, i, value)

                   x <- S4Vectors::mendoapply(function(k, u){
                     k[,,i] <- u
                   }, x, value)

                   return(x)
                 })
