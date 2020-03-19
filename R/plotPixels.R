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
#' @param bcg brightness/contrast/gamma list("H3" = c(0,1,1))
#' @param outline_by (optional)
#' @param subset_images TODO Note that Masks and Images need to have same names or same IDs
#' @param colour TODO
#' @param ... Further arguments passed to  \code{?"\link{plotting-param}"}
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
                       bcg = NULL,
                       outline_by = NULL,
                       subset_images = NULL,
                       colour = NULL,
                       ...) {
  # Object checks
  .valid.image(image, img_id)
  if(!is.null(object)){
    .valid.sce(object, img_id, cell_id, exprs_values = NULL)
  }
  if(!is.null(mask)){
    .valid.mask(mask, img_id)
  }
  .valid.matchObjects.plotPixels(object, mask, image, img_id)

  # Argument checks
  # Check colour_by argument
  if (!is.null(colour_by)){
    .valid.colour_by(colour_by, object, image,
                     call.arg = "plotPixels")
  }
  # Check outline_by argument
  if(!is.null(outline_by)){
    .valid.outline_by(outline_by, object, mask, image)
  }
  # Check subset_images argument
  if(!is.null(subset_images)){
    .valid.subset_images(subset_images, image = image, img_id)
  }
  # Check colour argument
  if(!is.null(colour)){
    .valid.colour(colour, colour_by, outline_by, object, image = image)
  }

  # Check bcg argument
  if(!is.null(bcg)){
    .valid.bcg(bcg, colour_by)
  }

  # Set further arguments
  dotArgs <- list(...)
  plottingParam <- .plottingParam(dotArgs, image = image)

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
                                 bcg,
                                 cur_col$colour_by)
  } else {
    if(is.null(channelNames(image))){
      colour_by <- 1
      cur_col$colour_by <- .selectColours(object, colour_by, colour)
      out_img <- .colourImageByFeature(image,
                                     colour_by,
                                     bcg,
                                     cur_col$colour_by)
    } else{
      colour_by <- channelNames(image)[1]
      cur_col$colour_by <- .selectColours(object, colour_by, colour)
      out_img <- .colourImageByFeature(image,
                                     colour_by,
                                     bcg,
                                     cur_col$colour_by)
    }
  }

  # Add outline
  if(!is.null(outline_by)){
    cur_col$outline_by <- .selectColours(object, outline_by, colour)
    out_img <- .outlineImageByMeta(object, mask, out_img, cell_id, img_id,
                                outline_by, cur_col$outline_by[[1]])
  } else if(!is.null(mask)){
    out_img <- mendoapply(function(cur_mask, cur_image){
      cur_img <- paintObjects(cur_mask, Image(cur_image),
                              col = plottingParam$missing_colour)
      return(cur_img)
    }, mask, out_img)
    out_img <- as(out_img, "SimpleList")
  }

  # Plot images
  .displayImages(object, image, exprs_values = NULL,
                   outline_by, colour_by, mask, out_img, img_id,
                   cur_col, plottingParam)

  return_objects <- NULL

  if(plottingParam$return_plot){
    return_objects <- as.list(return_objects)
    cur_plot <- recordPlot()
    return_objects$plot <- cur_plot
  }

  return(return_objects)
}
