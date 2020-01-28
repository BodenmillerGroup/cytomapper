#' @rdname plotCellCounts
#' @title
#'
#' @description
#'
#' @param data a \code{\link[SingleCellExperiment]{SingleCellExperiment}}.
#' @param image
#' @param mask
#' @param colour_by
#' @param outline_by
#' @param save_image
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
                       save_image = FALSE) {
  print("Hello, world!")
}
