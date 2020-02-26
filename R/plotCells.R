#' @title Plotting function to visualize cell-level information
#' @name plotCells
#'
#' @description TODO
#'
#' @param data a \code{\link[SingleCellExperiment]{SingleCellExperiment}}.
#' @param mask TODO
#' @param image_ID entry in object and mask
#' @param colour_by Either rownames (markers) or colnames of the colData(data) data.frame
#' @param outline_by TODO
#' @param save_image TODO
#' @param return_image TODO
#' @param colour TODO
#'
#' @section Linking image IDs and cell IDs:
#' TODO
#'
#' @return TODO
#'
#' @examples
#' # TODO
#' # col = list(cell_type = c("CD4" = "red", "CD8" = "blue"),
#` # tumour_stroma = c("tumour" = "white", "stroma" = "black"))
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
#'
#' @importFrom viridis viridis
#' @importFrom RColourBrewer brewer.pal
#' @export
plotCells <- function(object,
                      mask,
                      cell_ID,
                      image_ID,
                      colour_by = NULL,
                      outline_by = NULL,
                      exprs_values = "counts",
                      subset_images = NULL,
                      save_images = NULL,
                      return_images = FALSE,
                      colour = NULL,
                      missing_colour = "gray",
                      scale_bar = list(length = 20,
                                       label = NULL,
                                       lwd = 2,
                                       col = "white",
                                       margin = 10),
                      ...) {

  # Object checks
  .valid.sce(object, image_ID, cell_ID, exprs_values)
  .valid.mask(mask, image_ID)
  .valid.matchObjects(object, mask, image_ID, cell_ID)

  # Argument checks
  .valid.plotCells.input(object, mask, image_ID, colour_by, outline_by,
                         subset_images, save_images,
                         return_images, colour, missing_colour,
                         scale_bar)

  # Select images for plotting
  mask <- .select_images(object, mask, image_ID, subset_images)

  # Colour the masks
  # Here, a SimpleList is returned that allows storing colour Images
  if(!is.null(colour_by)){

    # Select the colours
    cur_col <- list()
    cur_col$colour_by <- .selectColours(object, colour_by, colour, missing_colour)

    if(all(colour_by %in% colnames(colData(object)))){
      # Colouring by metadata
      img <- .colourMaskByMeta(object, mask, cell_ID, image_ID,
                                colour_by, cur_col$colour_by)
    } else {
      # Colouring by features
      img <- .colourMaskByFeature(object, mask, cell_ID, image_ID,
                                   colour_by, exprs_values, cur_col$colour_by)
    }
  } else {
    img <- as(mask, "SimpleList")
  }

  # Add outline
  if(!is.null(outline_by)){
    cur_col$outline_by <- .selectColours(object, outline_by, colour, missing_colour)
    img <- .outlineMaskByMeta(object, mask, img, cell_ID, image_ID,
                               outline_by, cur_col$outline_by)
  }

  # Plot images
  .displayImages(object, outline_by, colour_by, img,
                 scale_bar, cur_col)


  # Add legend

}
