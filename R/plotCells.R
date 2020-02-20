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
                      colour = NULL,
                      missing_colour = "gray",
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
                         return_images, colour, missing_colour,
                         scale_bar)

  # Select images for plotting
  mask <- .select_images(object, mask, image_ID, subset_images)

  # Colour the masks
  if(!is.null(colour_by)){

    # Colouring the metadata
    if(all(colour_by %in% colnames(colData(object)))){

      # If colour is not specified, we select a number of default colours
      cur_entries <- unique(colData(object)[,colour_by])
      if(is.null(colour)){
        if(length(cur_entries) > 23){
          cur_col <- viridis(length(cur_entries))
          names(cur_col) <- cur_entries
          cur_col <- c(cur_col, missing_col = missing_col)
        } else {
          cur_col <- c(brewer.pal(12, "Paired"),
                       brewer.pal(8, "Pastel2")[-c(3,5,8)],
                       brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
          cur_col <- cur_col[1:length(cur_entries)]
          names(cur_col) <- cur_entries
          cur_col <- c(cur_col, missing_col = missing_col)
        }
      } else {
        cur_col <- colour[colour_by]
        cur_col <- c(cur_col, missing_col = missing_col)
      }

      mask <- .colourMaskByMeta(object, mask, cell_ID, image_ID,
                                colour_by, cur_col)
    } else {
      mask <- .colourMaskByFeature(object, mask, cell_ID, image_ID,
                                   colour_by, cur_col)
    }
  }

  # Add outline
  if(!is.null(outline_by)){
    mask <- .outlineMaskByMeta()
  }

  # Add scale bar

  # Add legend

}
