#' S4 class for list of images
#'
#' This class facilitates the handling of multiple one- or multi-channel images. It
#'inherits from \code{\linkS4class{SimpleList}} setting
#' \code{elementType="Image"}. Therefore, each slot contains an either one- or
#' multi-dimensional array in form of an \code{\linkS4class{Image}} object.
#'
#'
#' @param ... Named Image objects or a list of Image objects.
#' @param x TODO
#' @param value TODO
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
#' @section Restrictions on entries:
#' Discuss named list - not empty names, no NA, not duplicated names
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
#' getImages
#' setImages
#' mergeChannels
#'
#' @section Looping:
#' endoapply, mendoapply
#'
#' @seealso
#' \code{\linkS4class{Image}}, for ..
#' \code{\linkS4class{SimpleList}}, for ..
#' \code{?"\link{ImageList-naming}"}, for ...
#' \code{?"\link{ImageList-subsetting}"}, for ...
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
#'
#' @aliases
#' ImageList-class
#' coerce,ANY,ImageList-method
#' coerce,list,ImageList-method
#' show,ImageList-method
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch}
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}.
#'
#' @docType class
#'
#' @importFrom S4Vectors new2
#'
#' @export
ImageList <- function(...){
  args <- list(...)
  if (length(args) == 1L && methods::extends(class(args[[1L]]), "list"))
    args <- args[[1L]]
  if (length(args) == 1L && methods::extends(class(args[[1L]]), "SimpleList"))
    args <- as.list(args[[1L]])
  x <- S4Vectors::new2("ImageList", listData=args)
  return(x)
}

# Coercion from list
#' @exportMethod coerce
setAs("list", "ImageList", function(from) {
  # Use constructor function
  ImageList(from)
})

# Coercion from ANY
#' @exportMethod coerce
setAs("ANY", "ImageList", function(from) {
  # Use constructor function
  ImageList(from)
})

# Expanded show method
#' @exportMethod show
setMethod("show", signature = signature(object="ImageList"),
          definition = function(object){
            lo <- length(object)
            cat(class(object)[1], " containing ", lo,
                " images\n", sep = "")
            if (!is.null(names(object)))
              cat(paste0("names(", lo, "):"), names(object), "\n", sep = " ")
            if(length(dim(object[[1]])) > 2){
              cat("Each image contains ", dim(object[[1]])[3],
                  " channel(s)\n", sep = "")
            } else {
              cat("Each image contains 1 channel\n", sep = "")
            }
            if(!is.null(channelNames(object))){
              cat(paste0("channelNames(", length(channelNames(object)),
                  "):"), channelNames(object), "\n", sep = " ")
            }
          })

