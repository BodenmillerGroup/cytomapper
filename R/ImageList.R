#' S4 class for list of images
#'
#' This class allows the handling of multiple one- or multi-channel images. The
#' class inherits from \linkS4class{SimpleList} setting
#' \code{elementType="Image"}. Each slot contains an either one- or
#' multi-dimensional array in form of an \linkS4class{Image} object.
#'
#' Similar to the \linkS4class{Image} class, the first two dimensions of each
#' entry indicate the spatial dimension of the image. These can be different for
#' each entry. However, the third dimension indicates the number of channels per
#' Image. The ImageList class only supports a Grayscale colormode (see
#' \link{colormode}) representation of each individual image.
#'
#' Furthermore, each entry in the ImageList class object must contain the same
#' number of channels. Here, each channel represents pixel values indicating
#' measurement intensities or in case of segementation masks the cells' ID.
#'
#' The class further contains an \link{elementMetadata} slot that stores
#' image-level meta information.
#'
#' @param ... Named Image objects or a list of Image objects.
#' @param elementMetadata A data frame where each row indicates an image and
#'   each column stores image-specific meta-information.
#' @param channelNames A vector od string specifying the names of individual
#'   channels.
#'
#' @details In this class ...
#'
#' @section Accessors:
#' channelNames
#' mcols
#'
#' @section Coercion:
#'
#' @section Subsetting:
#' getFrames
#'
#' @section Looping:
#' endoapply
#'
#' @return An ImageList object
#'
#' @examples Example
#'
#' @seealso \code{\linkS4class{Image}}, for ..
#' @seealso \code{\linkS4class{SimpleList}}, for ..
#'
#' @aliases ImageList-class
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch}
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}.
#'
#' @docType class
#'
#' @importFrom methods extends
#' @importFrom S4Vectors new2
#'
#' @export
ImageList <- function(..., elementMetadata=NULL, channelNames=NULL){
  args <- list(...)
  if (length(args) == 1L && methods::extends(class(args[[1L]]), "list"))
    args <- args[[1L]]
  x <- S4Vectors::new2("ImageList", listData=args,
               elementMetadata=elementMetadata)
  channelNames(x) <- channelNames
  return(x)
}

