# Check sce validity for shiny
.valid.sce.shiny <- function(object, img_id){
    if (!is(object, "SingleCellExperiment")) {
        stop("'object' is not of type 'SingleCellExperiment'.")
    }

    if (is.null(img_id)) {
        stop("Please provide an 'img_id' argument.\n",
             "Gating is only supported on individual samples.")
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
    
    if (is.null(rownames(object))) {
        stop("Please specify the rownames of the 'object'.")
    }
    
    if (!is.null(image)) {
        if (is.null(channelNames(image))) {
            stop("Please specify the 'channelNames' of the 'image' object.")
        }
        if (!identical(channelNames(image), rownames(object))) {
            stop("The 'channelNames' of the images need to match the rownames of the object.")
        }
    }
}
