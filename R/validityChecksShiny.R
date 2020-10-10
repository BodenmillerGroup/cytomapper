# Check sce validity for shiny
.valid.sce.shiny <- function(object, img_id, cell_id, image, mask){
    if (!is(object, "SingleCellExperiment")) {
        stop("'object' is not of type 'SingleCellExperiment'.")
    }
    
    if (!is(metadata(object), "list")) {
        warning("metadata('object') is not of type 'list'.\n",
        "metadata('object') will be stored as 'list' \n", 
        "in the metadata slot of the output object.")
    }

    if (is.null(img_id)) {
        stop("Please provide an 'img_id' argument.\n",
             "Gating is only supported on individual samples.")
    }

    if (is.null(cell_id)) {
        stop("Please provide a 'cell_id' argument.")
    }
    
    if (!is.character(cell_id) || length(cell_id) > 1) {
        stop("Invalid argument for 'cell_id'.")
    }

    if (!is.character(img_id) || length(img_id) > 1) {
        stop("Invalid argument for 'img_id'.")
    }

    if (is.null(colData(object))) {
        stop("Please store the image- and metadata\n",
                "in the 'colData' slot of 'object'.")
    }

    if (!(img_id %in% colnames(colData(object)))) {
        stop("'img_id' not in 'colData(object)'.")
    }
    
    if (!(cell_id %in% colnames(colData(object)))) {
        stop("'cell_id' not in 'colData(object)'.")
    }

    if (is.null(rownames(object))) {
        stop("Please specify the rownames of the 'object'.")
    }

    if (!is.null(image)) {
        if (is.null(channelNames(image))) {
            stop("Please specify the 'channelNames' of the 'image' object.")
        }
        if (!identical(channelNames(image), rownames(object))) {
            stop("The 'channelNames' of the images\n",  
                 "need to match the rownames of the object.")
        }
        if (length(image) != length(unique(colData(object)[[img_id]]))) {
            stop("Please provide a unique image/mask\n",
                 "for every sample stored in 'object'.")
        }
    }
    
    if (!is.null(mask)) {
        if (length(mask) != length(unique(colData(object)[[img_id]]))) {
            stop("Please provide a unique image/mask\n", 
                 "for every sample stored in 'object'.")
        }
    }
}
