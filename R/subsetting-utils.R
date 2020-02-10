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
#' The `[<-` or `[[<-` functions for the \code{\linkS4class{ImageList}} object
#' have been adjusted to call getImages and setImages.
#'
#' @param x TODO
#' @param value TODO
#' @param i
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

            ans.list <- as.list(x)
            ans.mcols <- mcols(x)

            # Initial checks
            if(is.null(i) || (!is.numeric(i)  &&
                              !is.character(i) )){
              stop("Invalid argument for 'i'")
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
                 definition = function(x, i){

                   ans.list <- as.list(x)
                   ans.mcols <- mcols(x)

                   if(is.null(i) || (!is.numeric(i)  &&
                                     !is.character(i) )){
                     stop("Invalid argument for 'value'")
                   }

                   # If value is numeric, make sure that names are correctly replaced
                   if(is.numeric(i) && is.null(names(x)) && is.null()){

                   }


                 })

#' @export
#' @rdname ImageList-subsetting
setMethod("getChannels",
          signature = signature(x="ImageList"),
          definition = function(x, i, drop=FALSE){
            # Initial checks
            if(is.null(i) || (!is.numeric(i) &&
                              !is.character(i) )){
              stop("Invalid argument for 'value'")
            }
            if(is.character(i) &&
               sum(!(i %in% channelNames(x))) > 0){
              stop("'value' not in channelNames(x)")
            }

            ans <- S4Vectors::endoapply(x, function(y){
              y[,,i]
            })

            return(ans)
          })

#' @export
#' @rdname ImageList-subsetting
setReplaceMethod("setChannels",
                 signature = signature(x="ImageList"),
                 definition = function(x, i){

                 })



