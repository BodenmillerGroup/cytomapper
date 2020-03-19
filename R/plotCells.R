#' Function to visualize cell-level information on segmentation masks
#'
#' This function takes a \code{\linkS4class{SingleCellExperiment}} and
#' \code{\linkS4class{ImageList}} object to colour cells by marker expression or
#' metadata.
#'
#' @param object a \code{\linkS4class{SingleCellExperiment}} object.
#' @param mask an \code{\linkS4class{ImageList}} object single-channel
#'   \code{\linkS4class{Image}} objects (see Details)
#' @param cell_id character specifying the \code{colData(object)}, in which the
#'   integer cell IDs are stored
#' @param img_id TODO
#' @param colour_by TODO
#' @param outline_by TODO
#' @param exprs_values TODO
#' @param subset_images TODO
#' @param colour TODO
#' @param ... Further arguments passed to  \code{?"\link{plotting-param}"}
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
#' # colour = list(cell_type = c("CD4" = "red", "CD8" = "blue"),
#` # tumour_stroma = c("tumour" = "white", "stroma" = "black"))
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
#'
#' @export
plotCells <- function(mask,
                      object = NULL,
                      cell_id = NULL,
                      img_id = NULL,
                      colour_by = NULL,
                      outline_by = NULL,
                      exprs_values = "counts",
                      subset_images = NULL,
                      colour = NULL,
                      ...) {

  # Object checks
  .valid.mask(mask, img_id)
  if(!is.null(object)){
    .valid.sce(object, img_id, cell_id, exprs_values)
    .valid.matchObjects.plotCells(object, mask, img_id)

    if(is.null(img_id) || is.null(cell_id)){
      stop("Please provide an 'img_id' and 'cell_id' entry.")
    }
  }

  # Argument checks
  # Check colour_by argument
  if (!is.null(colour_by)){
    .valid.colour_by(colour_by, object, image = NULL,
                     call.arg = "plotCells")
  }
  # Check outline_by argument
  if(!is.null(outline_by)){
    .valid.outline_by(outline_by, object, mask, image = NULL)
  }
  # Check subset_images argument
  if(!is.null(subset_images)){
    .valid.subset_images(subset_images, image = mask, img_id)
  }
  # Check colour argument
  if(!is.null(colour)){
    .valid.colour(colour, colour_by, outline_by, object, image = NULL)
  }

  # Set further arguments
  dotArgs <- list(...)
  plottingParam <- .plottingParam(dotArgs, image = mask)

  # Select images for plotting
  mask <- .select_images(object, mask, img_id, subset_images)
  cur_col <- list()

  # Colour the masks
  # Here, a SimpleList is returned that allows storing colour Images
  if(!is.null(colour_by)){

    # Select the colours
    cur_col$colour_by <- .selectColours(object, colour_by, colour,
                                        call.arg = "colour_by")

    if(all(colour_by %in% colnames(colData(object)))){
      # Colouring by metadata
      out_img <- .colourMaskByMeta(object, mask, cell_id, img_id,
                                   colour_by, cur_col$colour_by[[1]],
                                   plottingParam$missing_colour,
                                   plottingParam$background_colour)
    } else {
      # Colouring by features
      out_img <- .colourMaskByFeature(object, mask, cell_id, img_id,
                                      colour_by, exprs_values,
                                      cur_col$colour_by,
                                      plottingParam$missing_colour,
                                      plottingParam$background_colour)
    }
  } else {
    out_img <- endoapply(mask, function(x){
      x[x == 0L] <- plottingParam$background_colour
      x <- replace(x, which(x != plottingParam$background_colour),
                   plottingParam$missing_colour)
      x
    })
    out_img <- as(out_img, "SimpleList")
  }

  # Add outline
  if(!is.null(outline_by)){
    cur_col$outline_by <- .selectColours(object, outline_by, colour,
                                         call.arg = "outline_by")
    out_img <- .outlineImageByMeta(object, mask, out_img, cell_id, img_id,
                                   outline_by, cur_col$outline_by[[1]])
  }

  # Plot images
  .displayImages(object, image = NULL, exprs_values, outline_by, colour_by,
                 mask, out_img, img_id,
                 cur_col, plottingParam)

  return_objects <- NULL

  if(plottingParam$return_plot){
    return_objects <- as.list(return_objects)
    cur_plot <- recordPlot()
    return_objects$plot <- cur_plot
  }

  return(return_objects)
}
