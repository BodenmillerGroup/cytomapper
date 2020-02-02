#' @rdname plotCellCounts
#' @title
#'
#' @description
#'
#' @param data a \code{\link[SingleCellExperiment]{SingleCellExperiment}}.
#' @param image TODO
#' @param mask TODO
#' @param colour_by TODO
#' @param outline_by TODO
#' @param save_image TODO
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

plotPixels <- function(data,
                       image,
                       mask = NULL,
                       colour_by,
                       outline_by = NULL,
                       save_image = FALSE,
                       return_image = FALSE,
                       col = NULL) {
  print("Hello, world!")
}
