#' Function to visualize pixel-level information of multi-channel images
#'
#' This function takes a \code{\linkS4class{CytoImageList}} object to colour
#' pixels by marker expression. Additionally, a
#' \code{\linkS4class{SingleCellExperiment}} object and \code{CytoImageList}
#' object containing segmentation masks can be provided to outline cells based
#' on metadata.
#'
#' @param image a \code{\linkS4class{CytoImageList}} object containing single or
#' multi-channel \code{\linkS4class{Image}} objects (see details below).
#' @param object an optional \code{\linkS4class{SingleCellExperiment}} object.
#' @param mask an optional \code{\linkS4class{CytoImageList}} object containing
#' segmentaion masks in form of single-channel \code{\linkS4class{Image}}
#' objects (see details below)
#' @param cell_id character specifying the \code{colData(object)} entry, in
#' which the integer cell IDs are stored. These IDs should match the integer
#' pixel values in the segmentation mask object.
#' @param img_id character specifying the \code{colData(object)},
#' \code{mcols(image)} and \code{mcols(mask)} entry, in which the image IDs
#' are stored (see section 'Linking the \code{SingleCellExperiment} and
#' \code{CytoImageList} objects' below)
#' @param colour_by character or character vector specifying the features
#' (contained in \code{channelNames(image)}) used to colour individual cells.
#' Pixels can be coloured by up to six features.
#' @param outline_by single character indicating the \code{colData(object)}
#' entry by which to outline individual cells
#' @param bcg a list with names matching the entries to \code{colour_by}. Each
#' entry contains a numeric vector of three entries:
#' \enumerate{
#'     \item brightness value added to the specified channel
#'     \item contrast value multiplied with the specified channel
#'     \item gamma value (channel is exponentiated by this value)
#' }
#' Default is c(0,1,1).
#' @param colour a list with names matching the entries to \code{colour_by}
#' and/or \code{outline_by}. When setting the colour for continous features,
#' at least two colours need to be provided indicating the colours for minimum
#' and maximum values. When outlining by discrete values, a colour for each
#' unique entry needs to be provided (see section 'Setting the colours' and
#' examples)
#' @param ... Further arguments passed to  \code{?"\link{plotting-param}"}
#'
#' @section Multi-channel image and segmentation mask objects:
#' In the \code{plotPixels} function, \code{image} refers to a
#' \code{\linkS4class{CytoImageList}} object that contains a single or multiple
#' single- or multi-channel \code{\linkS4class{Image}} objects. Up to six
#' channels can be overlayed to generate a composite image. When outlining
#' cells, a \code{\linkS4class{SingleCellExperiment}} object and
#' \code{\linkS4class{CytoImageList}} object containing segmentation masks must
#' be provided. The function assumes that each object in the segmentation mask
#' is a cell. The key features of such segmentation masks include:
#' \itemize{
#'     \item each Image object contains only one channel
#'     \item pixel values are integers indicating the cells' IDs
#' }
#'
#' @section Linking  SingleCellExperiment and CytoImageList objects:
#' To outline individual cells contained in the segmentation masks based on
#' metadata stored in the SingleCellExperiment object, an \code{img_id} and
#' \code{cell_id} entry needs to be provided. Image IDs are matched between the
#' \code{SingleCellExperiment} and \code{CytoImageList} objects via entries to
#' the \code{colData(object)[,img_id]}, \code{mcols(image)[,img_id]} and the
#' \code{mcols(image)[,img_id]} slots. Cell IDs are matched between the
#' \code{SingleCellExperiment} and \code{CytoImageList} object via entries to
#' \code{colData(object)[,cell_id]} and the integer values of the segmentation
#' masks.
#'
#' @section Setting the colours:
#' By default, features and metadata are coloured based on internally-set
#' colours. To set new colours, a \code{list} object must be provided. The names
#' of the object must correspond to the entries to \code{colour_by} and/or
#' \code{outline_by}. When setting the colours for continous expression values
#' or continous metadata entries, a vector of at least two colours need to be
#' specified. These colours will be passed onto \code{\link{colorRampPalette}}
#' for interpolation. Cells can be outlined by discrete metadata entries when
#' specifying a named vector in which each entry corresponds to a unique entry
#' to the metadata vector.
#'
#' @section Subsetting the \code{CytoImageList} objects:
#' The \code{CytoImageList} object(s) can be subsetted before calling the
#' \code{plotPixels} function. In that case, only the selected images are
#' displayed.
#'
#' @section Subsetting the \code{SingleCellExperiment} object:
#' The \code{SingleCellExperiment} object can be subsetted before calling the
#' \code{plotPixels} function. In that case, only cells contained in the
#' \code{SingleCellExperiment} object are outlined.
#'
#' @section Colour scaling:
#' When plotting pixel intensities, colours are scaled to the minimum and
#' maximum per channel across all images that are being displayed. Therefore,
#' when subsetting images, displayed intensities might change. However, the
#' colour legend will display the correct numeric minimum and maximum pixel
#' intensity across all displayed images per channel.
#'
#' @seealso
#' For further plotting parameters see \code{?"\link{plotting-param}"}.
#' For instructions on how to normalize images see
#' \code{\link{normalize}}.
#'
#' @return a list if \code{return_images} and/or \code{return_plot}
#' is TRUE (see \code{?"\link{plotting-param}"}).
#' \itemize{
#' \item \code{plot}: a single plot object (\code{display = "all"}) or a list
#' of plot objects (\code{display = "single"})
#' \item \code{images}: a \code{\linkS4class{SimpleList}} object containing
#' three-colour \code{\linkS4class{Image}} objects.
#' }
#'
#' @examples
#' data(pancreasMasks)
#' data(pancreasImages)
#' data(pancreasSCE)
#'
#' # Visualize the images - by default the first channel
#' plotPixels(pancreasImages)
#'
#' # Colour the channels
#' plotPixels(pancreasImages, colour_by = c("CD99", "CDH"))
#'
#' # Outline the cells based on metadata
#' plotPixels(pancreasImages, object = pancreasSCE, mask = pancreasMasks,
#'             img_id = "ImageNb", cell_id = "CellNb",
#'             colour_by = c("CD99", "CDH"), outline_by = "CellType")
#'
#' # Enhance individual channels
#' plotPixels(pancreasImages, colour_by = c("CD99", "CDH"),
#'             bcg = list(CD99 = c(0, 2, 1)))
#'
#' # Subset the images
#' cur_images <- getImages(pancreasImages, 1:2)
#' plotPixels(cur_images, colour_by = c("CD99", "CDH"))
#'
#' # Set colour
#' plotPixels(pancreasImages, colour_by = c("CD99", "CDH"),
#'             colour = list(CD99 = c("black", "green"),
#'                             CDH = c("black", "blue")))
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
#'
#' @export
plotPixels <- function(
    image,
    object = NULL,
    mask = NULL,
    cell_id = NULL,
    img_id = NULL,
    colour_by = NULL,
    outline_by = NULL,
    bcg = NULL,
    colour = NULL,
    ...) {
    
    if (!is.null(mask) && attr(class(mask), "package") == "cytomapper") {
        warning("Please update the CytoImageList object by calling 'updateObject(mask)'")
        mask <- CytoImageList::updateObject(mask)
    }
    
    if (attr(class(image), "package") == "cytomapper") {
        warning("Please update the CytoImageList object by calling 'updateObject(image)'")
        image <- CytoImageList::updateObject(image)
    }
    
    # Object checks
    .valid.image(image, img_id)
    if (!is.null(object)) {
        .valid.sce(object, img_id, cell_id, exprs_values = NULL)
    }
    if (!is.null(mask)) {
        .valid.mask(mask, img_id)
    }
    .valid.matchObjects.plotPixels(object, mask, image, img_id)

    # Argument checks
    # Check colour_by argument
    if (!is.null(colour_by)) {
        .valid.colour_by(colour_by, object, image,
                    call.arg = "plotPixels")
    }
    # Check outline_by argument
    if (!is.null(outline_by)) {
        .valid.outline_by(outline_by, object, mask, image)
    }
    # Check colour argument
    if (!is.null(colour)) {
        .valid.colour(colour, colour_by, outline_by, object, image = image)
    }

    # Check bcg argument
    if (!is.null(bcg)) {
        .valid.bcg(bcg, colour_by)
    }

    # Set further arguments
    dotArgs <- list(...)
    plottingParam <- .plottingParam(dotArgs, image = image)

    cur_col <- list()
    cur_limits <- list()
    
    is_Image_mask <- is(mask[[1]], "Image")

    # Colour the images
    # Here, a SimpleList is returned that allows storing colour Images
    if (!is.null(colour_by)) {

        # Select the colours
        cur_col$colour_by <- .selectColours(object, colour_by, colour)

        # Colouring by features
        out_img <- .colourImageByFeature(image,
                colour_by, bcg, cur_col$colour_by,
                plottingParam)
        cur_limits$colour_by <- out_img$cur_limit
        out_img <- out_img$imgs
        
    } else {
        if (is.null(channelNames(image))) {
            colour_by <- 1
            cur_col$colour_by <- .selectColours(object, colour_by, colour)
            out_img <- .colourImageByFeature(image, colour_by,
                                    bcg, cur_col$colour_by,
                                    plottingParam)
            cur_limits$colour_by <- out_img$cur_limit
            out_img <- out_img$imgs
        } else {
            colour_by <- channelNames(image)[1]
            cur_col$colour_by <- .selectColours(object, colour_by, colour)
            out_img <- .colourImageByFeature(image, colour_by,
                                    bcg, cur_col$colour_by,
                                    plottingParam)
            cur_limits$colour_by <- out_img$cur_limit
            out_img <- out_img$imgs
        }
    }

    # Add outline
    if (!is.null(outline_by)) {
        cur_col$outline_by <- .selectColours(object, outline_by, colour)
        out_img <- .outlineImageByMeta(object, mask, out_img, cell_id, img_id,
                                    outline_by, cur_col$outline_by[[1]], 
                                    plottingParam$thick)
        cur_limits$outline_by <- out_img$cur_limit
        out_img <- out_img$imgs
    } else if (!is.null(mask)) {
        out_img <- mendoapply(function(cur_image, cur_mask){
            
            if (is_Image_mask) {
                cur_img <- paintObjects(cur_mask, Image(cur_image),
                                        col = plottingParam$missing_colour,
                                        thick = plottingParam$thick)
            } else {
                cur_img <- paintObjects(as.array(cur_mask), Image(cur_image),
                                        col = plottingParam$missing_colour,
                                        thick = plottingParam$thick)
            }
            return(cur_img)
        }, out_img, mask)
        out_img <- as(out_img, "SimpleList")
        cur_limits$outline_by <- NULL
    }

    # Plot images
    cur_plot <- .displayImages(object = object, image = image, 
                    exprs_values = NULL, outline_by = outline_by, 
                    colour_by = colour_by, mask = mask, out_img = out_img, 
                    img_id = img_id, cur_col = cur_col, 
                    plottingParam = plottingParam, cur_limits = cur_limits)

    return_objects <- NULL

    if (!is.null(cur_plot)) {
        return_objects <- as.list(return_objects)
        return_objects$plot <- cur_plot
    }

    if (plottingParam$return_images) {
        return_objects <- as.list(return_objects)
        out_img <- endoapply(out_img, Image)
        return_objects$images <- out_img
    }

    if (!is.null(return_objects)) {
        return(return_objects)
    }
}
