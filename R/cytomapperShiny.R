#' Shiny app to gate cells on images
#'
#' TODO
#'
#' @param image (optional) an \code{\linkS4class{ImageList}} object containing single or
#'   multi-channel \code{\linkS4class{Image}} objects (see Details)
#' @param object an \code{\linkS4class{SingleCellExperiment}} object.
#' @param mask an \code{\linkS4class{ImageList}} object containing
#'   single-channel \code{\linkS4class{Image}} objects
#' @param cell_id character specifying the \code{colData(object)}, in which the
#'   integer cell IDs are stored
#' @param img_id character specifying the \code{colData(object)}, in which the
#'   integer image IDs are stored
#' @param ... Parameters passed onto \code{\link{plotCells}} or \code{\link{plotPixels}}
#'
#' @examples
#' # TODO
#'
#' @author
#' Nils Eling \email{nils.eling@@dqbm.uzh.ch},
#'
#' @export
#' @import ggplot2
#' @import ggbeeswarm
#' @import svgPanZoom
#' @import svglite
#' @import shiny
#' @import shinydashboard
#' @importFrom ggridges geom_density_ridges2
cytomapperShiny <- function(object,
                        mask,
                        image = NULL,
                        cell_id = NULL,
                        img_id = NULL,
                        ...) {
    # Object checks
    .valid.sce(object, img_id, cell_id, exprs_values = NULL)
    
    if (!is.null(mask)) {
        .valid.mask(mask, img_id)
    }
    
    if (!is.null(image)) {
        .valid.image(image, img_id)
    }

    shiny_ui <- dashboardPage(
        header = .cytomapper_header(),
        sidebar = .cytomapper_sidebar(),
        body = .cytomapper_body(),
    )

    shiny_server <- function(input, output, session, ...) {
        .cytomapper_server(object, mask, image, cell_id, img_id, 
                           input, output, session, ...)
    }

    shinyApp(ui = shiny_ui, server = shiny_server)
}


