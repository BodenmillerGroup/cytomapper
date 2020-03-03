#' Function to visualize pixel-level information of multi-channel images
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
#' @param cell_id (optional) character specifying the \code{colData(object)}, in which the
#'   integer cell IDs are stored
#' @param img_id (optional)
#' @param colour_by TODO
#' @param outline_by (optional)
#' @param subset_images TODO
#' @param colour TODO
#' @param missing_colour TODO
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
                       cell_id = NULL,
                       img_id = NULL,
                       colour_by = NULL,
                       outline_by = NULL,
                       subset_images = NULL,
                       colour = NULL,
                       missing_colour = "gray",
                       scale_bar = list(length = 20,
                                        label = NULL,
                                        lwd = 2,
                                        colour = "white",
                                        margin = 10),
                       ...) {
  # Object checks
  if(!is.null(object)){
    .valid.sce(object, img_id, cell_id, exprs_values = NULL)
  }
  if(!is.null(mask)){
    .valid.mask(mask, img_id)
  }
  .valid.image(image, img_id)
  .valid.matchObjects.plotPixels(object, mask, image, img_id)

  # Argument checks
  .valid.plotPixels.input(image, object, mask, img_id, colour_by, outline_by,
                         subset_images,
                         colour, missing_colour,
                         scale_bar)

  # Select images for plotting
  image <- .select_images(object, image, img_id, subset_images)
  if(!is.null(mask)){
    mask <- .select_images(object, mask, img_id, subset_images)
  }
  cur_col <- list()

  # Colour the images
  # Here, a SimpleList is returned that allows storing colour Images
  if(!is.null(colour_by)){

    # Select the colours
    cur_col$colour_by <- .selectColours(object, colour_by, colour)

    # Colouring by features
    out_img <- .colourImageByFeature(image,
                                 colour_by,
                                 cur_col$colour_by)
  } else {
    if(is.null(channelNames(image))){
      colour_by <- 1
      cur_col$colour_by <- .selectColours(object, colour_by, colour)
      out_img <- .colourImageByFeature(image,
                                     colour_by,
                                     cur_col$colour_by)
    } else{
      colour_by <- channelNames(image)[1]
      cur_col$colour_by <- .selectColours(object, colour_by, colour)
      out_img <- .colourImageByFeature(image,
                                     colour_by,
                                     cur_col$colour_by)
    }
  }

  # Add outline
  if(!is.null(outline_by)){
    cur_col$outline_by <- .selectColours(object, outline_by, colour)
    out_img <- .outlineImageByMeta(object, mask, out_img, cell_id, img_id,
                                outline_by, cur_col$outline_by)
  } else if(!is.null(mask)){
    out_img <- mendoapply(function(cur_mask, cur_image){
      cur_img <- paintObjects(cur_mask, Image(cur_image), col = missing_colour)
      return(cur_img)
    }, mask, out_img)
    out_img <- as(out_img, "SimpleList")
  }

  # Plot images
  .displayImages(object, image, exprs_values = NULL,
                 outline_by, colour_by, mask, out_img, img_id,
                 scale_bar, cur_col)

}
