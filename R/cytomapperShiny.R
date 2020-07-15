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
#' The UI is based on two tabs ("Scatter Plots" and "Images") and a "General Control" panel. Using the control panel, the user
#' can set the number of scatter plots to define a cell population, on which sample the gating should be applied, and which \code{Assay}
#' should be displayed. Depending on the number of plots selected, the user can define the marker that are used for 
#' each plot. At least one marker has to be defined (displayed as jittered points), two markers are shown as scatter plot. 
#'
#' @section Download of gated cells:
#' After a set of cells are selected the user can downloaded the cell data in 
#' the form of a \code{\linkS4class{SingleCellExperiment}} object stored in a RDS file. The object is a subset of the input 
#' \code{\linkS4class{SingleCellExperiment}} object. The object contains a new column named 
#' \code{CellLabel} stored in the \code{colData} of the \code{\linkS4class{SingleCellExperiment}} object. The new column
#' contains a character label defined by the user.
#'
#' @section Getting further help:
#' Find more information by clicking on the question mark in the upper right corner. 
#'
#' @seealso \code{\link{plotCells}} and \code{\link{plotPixels}} for the main plotting function
#'
#' @examples \code{
#' # Only run this example in interactive R sessions
#' if (interactive()) {
#'    options(device.ask.default = FALSE)
#' # Load example data sets
#' data("pancreasSCE")
#' data("pancreasImages")
#' data("pancreasMasks")
#' 
#' # Use shiny with SCE object, images and masks
#' cytomapperShiny(pancreasSCE, pancreasMasks, pancreasImages, 
#' cell_id = "CellNb", img_id = "ImageNb")
#' 
#' # Use shiny with SCE object and masks
#' cytomapperShiny(pancreasSCE, pancreasMasks, 
#' cell_id = "CellNb", img_id = "ImageNb")
#' 
#' # Use shiny with SCE object only
#' cytomapperShiny(pancreasSCE, 
#' cell_id = "CellNb", img_id = "ImageNb")
#' }
#'}
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


