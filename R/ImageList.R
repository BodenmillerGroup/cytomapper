#' S4 class for list of images
#'
#' This class facilitates the handling of multiple one- or multi-channel images.
#' It inherits from \code{\linkS4class{SimpleList}} setting
#' \code{elementType="Image"}. Therefore, each slot contains an either one- or
#' multi-dimensional array in form of an \code{\linkS4class{Image}} object.
#'
#' @details Similar to the \code{\linkS4class{Image}} class, the first two
#'   dimensions of each entry indicate the spatial dimension of the image. These
#'   can be different for each entry. The third dimension indicates the number
#'   of channels per Image. Each entry in the ImageList class object must
#'   contain the same number of channels. Here, each channel represents pixel
#'   values indicating measurement intensities or in case of segmentation masks
#'   the cells' ID. The ImageList class therefore only supports a Grayscale
#'   colormode (see \code{\link[EBImage]{colormode}}) representation of each
#'   individual image.
#'
#'   The class further contains an \code{\link{elementMetadata}} slot that
#'   stores image-level meta information. This slot should be accessed using the
#'   \code{\link[S4Vectors]{mcols}} accessor function.
#'
#' @section Restrictions on entry names:
#' The ImageList class only supports unique entry names to avoid duplicated
#' images. Names of an ImageList object can be get and set via \code{names(x)},
#' where \code{x} is an ImageList object. Furthermore, only named or unnamed
#' ImageList objects are allowed. Partially named objects causing empty or NA
#' names return an error.
#'
#' @section Constructor:
#' An ImageList object can be created via:
#' \describe{
#' \item{\code{ImageList(...)}:}{Here, \code{...} is a list (or coercible to a
#' list) or individual images}
#' }
#'
#' @section Coercion:
#' Coercion to and from list, \code{\linkS4class{SimpleList}} and
#' \code{\linkS4class{List}}: \describe{ \item{as.list(x), as(x, "SimpleList"),
#' as(x, "SimpleList"):}{Coercion from an ImageList object \code{x}} \item{as(x,
#' "ImageList"):}{Coercion from a list, SimpleList or List object \code{x} to an
#' ImageList object} }
#'
#' @section Looping:
#' While \code{\link[base]{lapply}} and \code{\link[base]{mapply}} return
#' regular list objects, \code{\link[S4Vectors]{endoapply}} and
#' \code{\link[S4Vectors]{mendoapply}} return ImageList objects.
#'
#' @seealso
#' \code{\linkS4class{Image}}, for further image analysis tools.
#' \code{\linkS4class{SimpleList}}, for basics functions to handle SimpleList
#' objects
#' \code{?\link{loadImages}}, for reading images into an ImageList
#' object
#' \code{?"\link{ImageList-naming}"}, for setting and getting image and
#' channel names
#' \code{?"\link{ImageList-subsetting}"}, for subsetting and
#' accessor functions
#'
#' @return An ImageList object
#'
#' @examples
#' # Creation of ImageList
#' u <- matrix(rbinom(100, 10, 0.5), ncol=10, nrow=10)
#' v <- matrix(rbinom(100, 10, 0.5), ncol=10, nrow=10)
#' IL1 <- ImageList(image1 = Image(u), image2 = Image(v))
#'
#' # Coercion
#' as.list(IL1)
#' as(IL1, "SimpleList")
#' as(list(image1 = Image(u), image2 = Image(v)), "ImageList")
#'
#' @aliases
#' ImageList-class
#' coerce,ANY,ImageList-method
#' coerce,list,ImageList-method
#' show,ImageList-method
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
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
                " image(s)\n", sep = "")
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

