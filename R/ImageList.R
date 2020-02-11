#' S4 class for list of images
#'
#' This class facilitates the handling of multiple one- or multi-channel images. It
#'inherits from \code{\linkS4class{SimpleList}} setting
#' \code{elementType="Image"}. Therefore, each slot contains an either one- or
#' multi-dimensional array in form of an \code{\linkS4class{Image}} object.
#'
#'
#' @param ... Named Image objects or a list of Image objects.
#' @param elementMetadata A data frame in which each row indicates an image and
#'   each column stores image-specific meta-information.
#' @param channelNames A character vector or string specifying the names of individual
#'   channels.
#'
#' @details Similar to the \code{\linkS4class{Image}} class, the first two dimensions of each
#' entry indicate the spatial dimension of the image. These can be different for
#' each entry. The third dimension indicates the number of channels per
#' Image. Each entry in the ImageList class object must contain the same
#' number of channels. Here, each channel represents pixel values indicating
#' measurement intensities or in case of segmentation masks the cells' ID.
#' The ImageList class therefore only supports a Grayscale colormode (see
#' \code{\link[EBImage]{colormode}}) representation of each individual image.
#'
#' The class further contains an \code{\link{elementMetadata}} slot that stores
#' image-level meta information. This slot should be accessed using the
#' \code{\link[S4Vectors]{mcols}} accessor function.
#'
#' @section Accessors:
#' channelNames
#' mcols
#'
#' @section Coercion:
#' An \code{ImageList} object can be coerced from \code{list()},
#' \code{SimpleList} and \code{List} objects. Also \code{list},
#' \code{SimpleList} and \code{List} objects can be coerced to \code{ImageList}.
#'
#' @section Subsetting:
#'
#' getFrames
#'
#' @section Looping:
#' endoapply
#'
#' @return An ImageList object
#'
#' @examples
#' # Creation of ImageList
#'
#' # Accessors
#'
#' # Coercion
#' # as("ImageList", list)
#'
#' # Subsetting
#'
#' # Looping
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
  if (length(args) == 1L && methods::extends(class(args[[1L]]), "SimpleList"))
    args <- as.list(args[[1L]])
  x <- S4Vectors::new2("ImageList", listData=args,
               elementMetadata=elementMetadata)
  channelNames(x) <- channelNames
  return(x)
}

