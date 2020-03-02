#' Function to visualize cell-level information on segmentation masks
#' @name plotCells
#'
#' This function takes a \code{\linkS4class{SingleCellExperiment}} and
#' \code{\linkS4class{ImageList}} object to colour cells by marker expression or
#' metadata.
#'
#' @param object a \code{\linkS4class{SingleCellExperiment}} object.
#' @param mask an \code{\linkS4class{ImageList}} object single-channel
#'   \code{\linkS4class{Image}} objects (see Details)
#' @param cell_ID character specifying the \code{colData(object)}, in which the
#'   integer cell IDs are stored
#' @param image_ID TODO
#' @param colour_by TODO
#' @param outline_by TODO
#' @param exprs_values TODO
#' @param subset_images TODO
#' @param save_images TODO
#' @param return_images TODO
#' @param colour TODO
#' @param missing_colour TODO
#' @param scale_bar TODO
#' @param ... TODO
#'
#'@section Segmentaion mask object:
#' TODO
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
#' @export
plotCells <- function(object,
                      mask,
                      cell_ID,
                      image_ID,
                      colour_by = NULL,
                      outline_by = NULL,
                      exprs_values = "counts",
                      subset_images = NULL,
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
  .valid.matchObjects(object, mask, image = NULL, image_ID, cell_ID)

  # Argument checks
  .valid.plotCells.input(object, mask, image_ID, colour_by, outline_by,
                         subset_images,
                         colour, missing_colour,
                         scale_bar)

  # Select images for plotting
  mask <- .select_images(object, mask, image_ID, subset_images)
  cur_col <- list()

  # Colour the masks
  # Here, a SimpleList is returned that allows storing colour Images
  if(!is.null(colour_by)){

    # Select the colours
    cur_col$colour_by <- .selectColours(object, colour_by, colour)

    if(all(colour_by %in% colnames(colData(object)))){
      # Colouring by metadata
      img <- .colourMaskByMeta(object, mask, cell_ID, image_ID,
                                colour_by, cur_col$colour_by, missing_colour)
    } else {
      # Colouring by features
      img <- .colourMaskByFeature(object, mask, cell_ID, image_ID,
                                   colour_by, exprs_values,
                                  cur_col$colour_by, missing_colour)
    }
  } else {
    img <- endoapply(mask, function(x){
      x[x == 0L] <- "#000000"
      x <- replace(x, which(x != "#000000"), missing_colour)
      x
    })
    img <- as(img, "SimpleList")
  }

  # Add outline
  if(!is.null(outline_by)){
    cur_col$outline_by <- .selectColours(object, outline_by, colour)
    img <- .outlineMaskByMeta(object, mask, img, cell_ID, image_ID,
                               outline_by, cur_col$outline_by)
  }

  # Plot images
  .displayImages(object, exprs_values, outline_by, colour_by, mask, img, image_ID,
                 scale_bar, cur_col)
}
