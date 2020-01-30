#' @rdname plotCells
#' @title
#'
#' @description
#'
#' @param data a \code{\link[SingleCellExperiment]{SingleCellExperiment}}.
#' @param mask
#' @param colour_by Either rownames (markers) or colnames of the colData(data) data.frame
#' @param outline_by
#' @param save_image
#' @param return_image
#' @param col
#'
#' @return
#'
#' @examples
#' # TODO
#'
#' @author Nils Eling \email{nils.eling@@dqbm.uzh.ch},
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom SingleCellExperiment colData
#' @importFrom reshape2 melt
#' @export

plotCells <- function(data,
                       mask,
                       colour_by = cell_type,
                       outline_by = tumour_stroma,
                       save_image = FALSE,
                       return_image = FALSE,
                       col = list(cell_type = c("CD4" = "red", "CD8" = "blue"),
                                  tumour_stroma = c("tumour" = "white", "stroma" = "black"))) {
  print("Hello, world!")
}
