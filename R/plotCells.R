#' @rdname plotCells
#' @title Plotting function to visualize cell-level information
#'
#' @description TODO
#'
#' @param data a \code{\link[SingleCellExperiment]{SingleCellExperiment}}.
#' @param mask TODO
#' @param colour_by Either rownames (markers) or colnames of the colData(data) data.frame
#' @param outline_by TODO
#' @param save_image TODO
#' @param return_image TODO
#' @param col TODO
#'
#' @return TODO
#'
#' @examples
#' # TODO
#' # col = list(cell_type = c("CD4" = "red", "CD8" = "blue"),
#` # tumour_stroma = c("tumour" = "white", "stroma" = "black"))
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch},
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
#'
#' @export
plotCells <- function(data,
                       mask,
                       colour_by = NULL,
                       outline_by = NULL,
                       save_image = FALSE,
                       return_image = FALSE,
                       col = NULL) {

}
