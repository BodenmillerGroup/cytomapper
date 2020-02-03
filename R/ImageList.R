#' @title S4 class for list of images
#'
#' @description This class allows the handling of multiple one- or multi-channel images.
#' The class inherits from the \linkS4class{SimpleList} object class storing \linkS4class{Image} objects in each slot.
#' Furthermore, the class contains an \code{elementMetadata} slot that stores image-level meta information.
#'
#' @details
#' In this class ...
#'
#' @param ... TODO
#' @param elementMetadata TODO
#' @param elementMetadata TODO
#'
#' @docType class
#'
#' @importFrom methods new
#'
#' @author
#' Nils Eling \email{nils.eling@dqbm.uzh.ch}
#' Nicolas Damond \email{nicolas.damond@dqbm.uzh.ch}
#' @export
ImageList <- function(..., elementMetadata=NULL, channelNames=NULL){
  x <- methods::new("ImageList", ...,
               elementMetadata=elementMetadata)
  channelNames(x) <- channelNames
  return(x)
}

