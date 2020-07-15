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
#' If image is specified - also specify mask
#'
#' @section The user interface
#'
#' @section Download of gated cells:
#' After cells were gated based on their expression values, the user can downloaded the selected cells in 
#' the form of a \code{\linkS4class{SingleCellExperiment}} object saved in a RDS file. The object is a subset of the input 
#' \code{\linkS4class{SingleCellExperiment}} object. The object contains a new column called 
#' \code{CellLabel} stored in the \code{colData} of the \code{\linkS4class{SingleCellExperiment}} object. The new column
#' contains a character defined by the user.
#'
#' @section Getting further help:
#' 
#'
#' @seealso \code{\link{plotCells}} and \code{\link{plotPixels}} for the main plotting function
#'
#' @examples \code{
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
    # Object checks
    .valid.sce.shiny(object, img_id, image)
    
    if (!is.null(mask)) {
        .valid.sce(object, img_id, cell_id, exprs_values = NULL)
        .valid.mask(mask, img_id)
        .valid.matchObjects.plotCells(object, mask, img_id)
    }
    
    if (!is.null(image)) {
        if (is.null(mask)) {
            stop("Please provide a mask object.")
        }
        .valid.image(image, img_id)
        .valid.matchObjects.plotPixels(object, mask, image, img_id)
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


