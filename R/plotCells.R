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
#' @param col TODO
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
#' @importFrom raster scalebar
#' @export
plotCells <- function(object,
                      mask,
                      cell_ID,
                      image_ID,
                      colour_by = NULL,
                      outline_by = NULL,
                      subset_images = NULL,
                      save_images = NULL,
                      return_images = FALSE,
                      col = NULL,
                      missing_col = "gray",
                      scale_bar = list(length = 100,
                                       label = NULL,
                                       position = NULL,
                                       lwd = 2)) {

  # Object checks
  .valid.sce(object, image_ID, cell_ID)
  .valid.mask(mask, image_ID)
  .valid.matchObjects(object, mask, image_ID, cell_ID)

  # Argument checks
  .valid.plotCells.input(object, mask, image_ID, colour_by, outline_by,
                         subset_images, save_images,
                         return_images, col, missing_col,
                         scale_bar)

  # Select images for plotting
  mask <- .select_images(object, mask, subset_images)

  # Build default colour scale

  # Add scale bar

  # Add legend

}
