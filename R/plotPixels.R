#' Function to visualize pixel-level information of multi-channel images
#' @name plotPixels
#'
#' This function takes a \code{\linkS4class{ImageList}} object to colour pixels
#' by marker expression. Additionally, a \code{\linkS4class{SingleCellExperiment}}
#' object and \code{\linkS4class{ImageList}} object containing segmentation masks
#' can be provided to outline cells based on metadata.
#'
#' @param image an \code{\linkS4class{ImageList}} object containing single or
#'   multi-channel \code{\linkS4class{Image}} objects (see Details)
#' @param object (optional) an \code{\linkS4class{SingleCellExperiment}} object.
#' @param mask (optional) an \code{\linkS4class{ImageList}} object containing
#'   single-channel \code{\linkS4class{Image}} objects
#' @param cell_ID (optional) character specifying the \code{colData(object)}, in which the
#'   integer cell IDs are stored
#' @param image_ID (optional)
#' @param colour_by TODO
#' @param outline_by (optional)
#' @param subset_images TODO
#' @param save_images TODO
#' @param return_images TODO
#' @param colour TODO
#' @param scale_bar TODO
#' @param ... TODO
#'
#' @return TODO
#'
#' @examples
#' # TODO
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch},
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
#'
#' @export
plotPixels <- function(image,
                       object = NULL,
                       mask = NULL,
                       cell_ID = NULL,
                       image_ID = NULL,
                       colour_by = NULL,
                       outline_by = NULL,
                       subset_images = NULL,
                       colour = NULL,
                       scale_bar = list(length = 20,
                                        label = NULL,
                                        lwd = 2,
                                        col = "white",
                                        margin = 10),
                       ...) {

}
