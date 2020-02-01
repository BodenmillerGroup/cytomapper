#' @name ImageList
#' @aliases ImageList-class
#'
#' @title S4 class for list of images
#' @description This class allows the handling of multiple one- or multi-channel images.
#' The class builds on the \linkS4class{SimpleList} object class storing \linkS4class{Image} objects in each slot.
#' Furthermore, the class contains a metadata slot that stores image-level meta information.
#'
#' @details
#' Objects of class \code{dbFrame} hold all data required for debarcoding:
#'
#' @slot images
#'
#' @author Nils Eling \email{nils.eling@dqbm.uzh.ch}
#' @importFrom methods new
#' @export
setClass(
  Class="ImageList",
  package="SingleCellMapper",
  representation(int_metadata="list"),
  contains = "SimpleList",
  prototype = prototype(
    int_metadata=list(
      version=utils::packageVersion("SingleCellMapper"))
    )
  )

# Validity checks
setValidity(Class="ImageList",
            method=function(object) {

              # Check if all entries are Image class objects

              # Check if all images have the same number of channels

              # Check

              return(TRUE)
            }
)

# Constructor function
ImageList <- function(..., elementMetadata=DataFrame()){
    new("ImageList", ..., elementMetadata)
}



