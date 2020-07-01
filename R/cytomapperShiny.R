#' Shiny application to visualise gated cells on images
#'
#' This shiny application allows users to gate cells based on their raw or
#' transformed expression values and visualises gated cells on their
#' corresponding images.
#'
#' @param object a \code{\linkS4class{SingleCellExperiment}} object.
#' @param mask (optional) a \code{\linkS4class{CytoImageList}} containing
#'   single-channel \code{\linkS4class{Image}} objects
#' @param image (optional) a \code{\linkS4class{CytoImageList}} object
#'   containing single or multi-channel \code{\linkS4class{Image}} objects.
#' @param cell_id character specifying the \code{colData(object)} entry, in
#'   which the integer cell IDs are stored. These IDs should match the integer
#'   pixel values in the segmentation mask object.
#' @param img_id character specifying the \code{colData(object)} and
#'   \code{mcols(mask)} and/or \code{mcols(image)} entry, in which the image IDs
#'   are stored.
#' @param ... parameters passed to the \code{\link{plotCells}} or
#'   \code{\link{plotPixels}} function.
#'
#' @section User inputs: 
#' This function requires at least a \code{\linkS4class{SingleCellExperiment}}
#' input object. Gating is performed on cell-specific marker counts stored in
#' the \code{assay} slots of the object. These can either be raw counts (usually
#' stored in the \code{counts} slot) or transformed/scaled counts stored in
#' other assay slots. Gating can only be performed sample-wise; therefore, even 
#' if \code{mask} or \code{image} are not specified, \code{img_id} needs to point to
#' the \code{colData} entry storing unique sample IDs.
#' 
#' If mask is specified
#'
#' @section The user interface
#'
#' @section Download of gated cells
#'
#' @section Getting further help
#'
#' @seealso \code{\link{plotCells}} and \code{\link{plotPixels}} for the main
#'   plotting function
#'
#' @examples
#' # TODO
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' @author Tobias Hoch (\email{tobias.hoch@@dqbm.uzh.ch})
#'
#' @export
#'
#' @import shiny
#' @import shinydashboard
cytomapperShiny <- function(object,
                        mask = NULL,
                        image = NULL,
                        cell_id = NULL,
                        img_id = NULL,
                        ...) {
    
    # Further checks for shiny:
    # img_id needs to be specified even if image/mask is not specified
    # Check if channelNames and rownames are correctly set.
    # If image is specified, mask must also be specified
    
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


