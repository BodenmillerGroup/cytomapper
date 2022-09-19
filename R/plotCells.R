#' Function to visualize cell-level information on segmentation masks
#'
#' This function takes a \code{\linkS4class{SingleCellExperiment}} and
#' \code{\linkS4class{CytoImageList}} object containing segmentation masks to
#' colour cells by marker expression or metadata.
#'
#' @param object a \code{\linkS4class{SingleCellExperiment}} object.
#' @param mask a \code{\linkS4class{CytoImageList}} containing single-channel
#' \code{\linkS4class{Image}} objects (see section 'Segmentation mask object'
#' below).
#' @param cell_id character specifying the \code{colData(object)} entry, in
#' which the integer cell IDs are stored. These IDs should match the integer
#' pixel values in the segmentation mask object.
#' @param img_id character specifying the \code{colData(object)} and
#' \code{mcols(mask)} entry, in which the image IDs are stored (see section
#' 'Linking the \code{SingleCellExperiment} and \code{CytoImageList} object'
#' below)
#' @param colour_by character or character vector specifying the features
#' (\code{rownames(object)}) or metadata (\code{colData(object)} entry) used
#' to colour individual cells. Cells can be coloured by single
#' \code{colData(object)} entries or by up to six features.
#' @param outline_by single character indicating the \code{colData(object)}
#'   entry by which to outline individual cells.
#' @param exprs_values single character indicating which \code{assay(object)}
#' entry to use when visualizing feature counts.
#' @param colour a list with names matching the entries to \code{colour_by}
#' and/or \code{outline_by}. When setting the colour for continous features,
#' at least two colours need to be provided indicating the colours for minimum
#' and maximum values. When colouring discrete vectors, a colour for each
#' unique entry needs to be provided (see section 'Setting the colours' and
#' examples)
#' @param ... Further arguments passed to \code{?"\link{plotting-param}"}
#'
#' @section Segmentation mask object:
#' In the \code{plotCells} function, \code{mask} refers to a
#' \code{\linkS4class{CytoImageList}} object that contains a single or multiple
#' segmentation masks in form of individual \code{\linkS4class{Image}} objects.
#' The function assumes that each object in the segmentation mask is a cell.
#' The key features of such masks include:
#' \itemize{
#'     \item each Image object contains only one channel
#'     \item pixel values are integers indicating the cells' IDs or 
#'     0 (background)
#' }
#'
#' @section Linking SingleCellExperiment and CytoImageList objects:
#' To colour individual cells contained in the segmentation masks based on
#' features and metadata stored in the SingleCellExperiment object, an
#' \code{img_id} and \code{cell_id} entry needs to be provided. Image IDs are
#' matched between the \code{SingleCellExperiment} and \code{CytoImageList}
#' object via entries to the \code{colData(object)[,img_id]} and the
#' \code{mcols(mask)[,img_id]} slots. Cell IDs are matched between the
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
#' for interpolation. Discrete metadata entries can be coloured by specifying a
#' named vector in which each entry corresponds to a unique entry to the
#' metadata vector.
#'
#' @section Subsetting the \code{CytoImageList} object:
#' The \code{CytoImageList} object can be subsetted before calling the
#' \code{plotCells} function. In that case, only the selected images are
#' displayed.
#'
#' @section Subsetting the \code{SingleCellExperiment} object:
#' The \code{SingleCellExperiment} object can be subsetted before calling the
#' \code{plotCells} function. In that case, only cells contained in the
#' \code{SingleCellExperiment} object are coloured/outlined.
#'
#' @section Colour scaling:
#' When colouring features using the plotCells function, colours are scaled
#' between the minimum and maximum per feature across the full assay contained
#' in the SingleCellExperiment object. When subsetting images, cell-level
#' expression is not scaled across the subsetted images but the whole
#' SingleCellExperiment object. To avoid this, the SingleCellExperiment object
#' can be subsetted to only contain the cells that should be displayed before
#' plotting.
#'
#' @seealso For further plotting parameters see \code{?"\link{plotting-param}"}
#'
#' @return a list if \code{return_images} and/or \code{return_plot} is TRUE
#' (see \code{?"\link{plotting-param}"}).
#' \itemize{
#' \item \code{plot}: a single plot object (\code{display = "all"}) or a list
#' of plot objects (\code{display = "single"})
#' \item \code{images}: a \code{\linkS4class{SimpleList}} object containing
#' three-colour \code{\linkS4class{Image}} objects.
#' }
#'
#' @examples
#' data(pancreasMasks)
#' data(pancreasSCE)
#'
#' # Visualize the masks
#' plotCells(pancreasMasks)
#'
#' # Colour the masks based on averaged expression
#' plotCells(pancreasMasks, object = pancreasSCE, img_id = "ImageNb",
#'             cell_id = "CellNb", colour_by = c("CD99", "CDH"))
#'
#' # Colour the masks based on metadata
#' plotCells(pancreasMasks, object = pancreasSCE, img_id = "ImageNb",
#'             cell_id = "CellNb", colour_by = "CellType")
#'
#' # Outline the masks based on metadata
#' plotCells(pancreasMasks, object = pancreasSCE, img_id = "ImageNb",
#'             cell_id = "CellNb", colour_by = "CD99",
#'             outline_by = "CellType")
#'
#' # Colour the masks based on arcsinh-transformed expression
#' plotCells(pancreasMasks, object = pancreasSCE, img_id = "ImageNb",
#'             cell_id = "CellNb", colour_by = "CD99",
#'             exprs_values = "exprs")
#'
#' # Subset the images
#' cur_images <- getImages(pancreasMasks, 1:2)
#' plotCells(cur_images, object = pancreasSCE, img_id = "ImageNb",
#'             cell_id = "CellNb", colour_by = "CD99")
#'
#' # Set colour
#' plotCells(pancreasMasks, object = pancreasSCE, img_id = "ImageNb",
#'             cell_id = "CellNb", colour_by = "CD99", outline_by = "CellType",
#'             colour = list(CD99 = c("black", "red"),
#'                             CellType = c(celltype_A = "blue",
#'                                         celltype_B = "green",
#'                                         celltype_C = "red")))
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
#'
#' @export
plotCells <- function(
    mask,
    object = NULL,
    cell_id = NULL,
    img_id = NULL,
    colour_by = NULL,
    outline_by = NULL,
    exprs_values = "counts",
    colour = NULL,
    ...) {
    
    if (attr(class(mask), "package") == "cytomapper") {
        warning("Please update the CytoImageList object by calling 'updateObject(mask)'")
    }

    # Object checks
    .valid.mask(mask, img_id)
    if (!is.null(object)) {
        .valid.sce(object, img_id, cell_id, exprs_values)
        .valid.matchObjects.plotCells(object, mask, img_id)
    }

    # Argument checks
    # Check colour_by argument
    if (!is.null(colour_by)) {
        .valid.colour_by(colour_by, object, image = NULL,
                call.arg = "plotCells")
    }
    # Check outline_by argument
    if (!is.null(outline_by)) {
        .valid.outline_by(outline_by, object, mask, image = NULL)
    }
    # Check colour argument
    if (!is.null(colour)) {
        .valid.colour(colour, colour_by, outline_by, object, image = NULL)
    }

    # Set further arguments
    dotArgs <- list(...)
    plottingParam <- .plottingParam(dotArgs, image = mask)

    cur_col <- list()
    cur_limits <- list()
    
    # Check image type
    is_Image <- is(mask[[1]], "Image")

    # Colour the masks
    # Here, a SimpleList is returned that allows storing colour Images
    if (!is.null(colour_by)) {
        # Select the colours
        cur_col$colour_by <- .selectColours(object, colour_by, colour,
                                    call.arg = "colour_by")

        if (all(colour_by %in% colnames(colData(object)))) {
            # Colouring by metadata
            out_img <- .colourMaskByMeta(object, mask, cell_id, img_id,
                                    colour_by, cur_col$colour_by[[1]],
                                    plottingParam$missing_colour,
                                    plottingParam$background_colour)
            cur_limits$colour_by <- out_img$cur_limit
            out_img <- out_img$imgs
        } else {
            # Colouring by features
            out_img <- .colourMaskByFeature(object, mask, cell_id, img_id,
                                            colour_by, exprs_values,
                                            cur_col$colour_by,
                                            plottingParam$missing_colour,
                                            plottingParam$background_colour,
                                            plottingParam)
            cur_limits$colour_by <- out_img$cur_limit
            out_img <- out_img$imgs
        }
    } else {
        out_img <- endoapply(mask, function(x){
            
            if (!is_Image) {
                x <- as.array(x)
            }
            
            x[x == 0L] <- plottingParam$background_colour
            x <- replace(x, which(x != plottingParam$background_colour),
                    plottingParam$missing_colour)
            Image(x)
        })
        out_img <- as(out_img, "SimpleList")
        cur_limits$colour_by <- NULL
    }

    # Add outline
    if (!is.null(outline_by)) {
        cur_col$outline_by <- .selectColours(object, outline_by, colour,
                                        call.arg = "outline_by")
        out_img <- .outlineImageByMeta(object, mask, out_img, cell_id, img_id,
                                        outline_by, cur_col$outline_by[[1]], 
                                        plottingParam$thick)
        cur_limits$outline_by <- out_img$cur_limit
        out_img <- out_img$imgs
    }

    # Plot images
    cur_plot <- .displayImages(object = object, image = NULL, 
                    exprs_values = exprs_values, outline_by = outline_by,
                    colour_by = colour_by,  mask = mask, out_img = out_img, 
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
