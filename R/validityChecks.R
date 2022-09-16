#' @importFrom tools file_ext
.valid.loadImage.input <- function(x, pattern, name){

    # Check if input is character
    if(!is.character(x)){
        stop("Please provide a string input indicating a single file\n",
            ", a path or a vector of files.")
    }

    # Further checks depending on length of object
    if(length(x) == 1){
        if(!file.exists(x)){

            stop("The provided file or path does not exist.\n",
            "Make sure the file or path is accessible\n",
            "from the current location.")

        }

        if(dir.exists(x) && is.null(pattern)){

            # Check if path only contains images
            exten <- file_ext(list.files(x))

            if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff",
                                            "tif", "jpg", "h5"))) > 0){
                stop("The provided path contains file-types other than\n",
                    "'jpeg', 'tiff', 'png' or 'h5'.\n",
                    "Please provide a correct regular expression\n",
                    "in the 'pattern' argument to select correct images.")
            }

            message("All files in the provided location will be read in.")
            out <- list.files(x, full.names = TRUE)

        } else if (dir.exists(x) && !is.null(pattern)) {
            # Check pattern
            if (!is.character(pattern) && !is.factor(pattern)) {
                stop("Please provide a single character,\n",
                        "character vector or factor as pattern input.")
            }

            out <- list.files(x, full.names = TRUE)

            # Since more than regular expressions can be given to pattern,
            # we need to perform selection manually
            if(length(pattern) == 1){
                out <- out[grepl(pattern, out)]
            } else {
                # Build pattern for grep function
                pattern <- unique(pattern)

                out <- out[grepl(paste(pattern, collapse = "|"), out)]
            }

            # Check if any of the files contain the pattern
            if(length(out) == 0){
                stop("The pattern does not match any\n",
                    "of the files in the provided directory.")
            }

            # Check if all of the files are of the supported format
            exten <- file_ext(out)

            if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff",
                                        "tif", "jpg", "h5"))) > 0){
                stop("The provided path contains file-types other than\n",
                    "'jpeg', 'tiff', 'png' or 'h5'.\n",
                    "Please provide a correct regular expression\n",
                    "in the 'pattern' argument to select correct images.")
            }

        } else {
            cur_ext <- file_ext(x)
            if(!(cur_ext %in% c("jpeg", "png", "tiff", "tif", "jpg", "h5"))){
                stop("The provided file is not of type ",
                    "'jpeg', 'tiff', 'png' or 'h5'.\n",
                    "Other image types are not supported.")
            }
            out <- x
        }
    } else {
        # Check if files exists
        cur_check <- file.exists(x)
        if(sum(!cur_check) > 0){
            stop("One or multiple files do not exist.\n",
                "Please correct the input.")
        }

        # Check if files are os supported format
        exten <- file_ext(x)
        cur_test <- unique(exten) %in% c("jpeg", "png", "tiff", 
                                         "tif", "jpg", "h5")
        if(sum(!cur_test) > 0){
            stop("The files are of type other than ",
                 "'jpeg', 'tiff', 'png' or 'h5'.\n",
                "Please only provide files of the supported file-type.")
        }

        out <- x

    }
    
    # Check name argument
    if (!is.null(name)) {
        if (!all(is.character(name))) {
            stop("Argument 'name' must be of type character.")
        }
        
        if (length(name) != 1 && length(name) != length(out)) {
            stop("Length of 'name' must either be 1 or the same length as ",
                 "the number of files read in.")
        }
    }
    
    return(out)
}

# Check sce validity
#' @importFrom SummarizedExperiment assayNames
.valid.sce <- function(object, img_id, cell_id, exprs_values){
    if(!is(object, "SingleCellExperiment")){
        stop("'object' is not of type 'SingleCellExperiment'.")
    }

    if(is.null(img_id) || is.null(cell_id)){
        stop("Please provide an 'img_id' and 'cell_id' argument")
    }

    if(!is.character(img_id) || length(img_id) > 1 ||
        !is.character(cell_id) || length(cell_id) > 1){
        stop("Invalid argument for 'img_id' and/or 'cell_id'.")
    }

    if(is.null(colData(object))){
        stop("Please store the image- and cell-level metadata\n",
                "in the 'colData' slot of 'object'.")
    }

    if(!(img_id %in% colnames(colData(object))) ||
        !(cell_id %in% colnames(colData(object)))){
        stop("'img_id' and/or 'cell_id' not in 'colData(object)'.")
    }
    
    if (!all(is.numeric(colData(object)[,cell_id]))) {
        stop("Cell ids should only contain numeric integer values.")
    }

    if(!all(colData(object)[,cell_id] == floor(colData(object)[,cell_id]))){
        stop("Cell ids should only contain numeric integer values.")
    }

    if(!is.null(exprs_values) && !(exprs_values %in% assayNames(object))){
        stop("'exprs_values' not an assay entry in 'object'.")
    }
}

# Check mask valididty
#' @importFrom EBImage numberOfFrames
#' @importFrom S4Vectors mcols
.valid.mask <- function(mask, img_id){
    if(!is(mask, "CytoImageList")){
        stop("Please provide the segmentation mask(s)\n",
                "in form of a 'CytoImageList' object")
    }

    # Check number of channels in mask
    if(!all(unlist(lapply(mask, numberOfFrames)) == 1L)){
        stop("Segmentation masks must only contain one channel.")
    }

    # Check if masks only contain integers
    cur_out <- lapply(mask, function(x){all(x == floor(x))})
    if(!all(unlist(cur_out))){
        stop("Segmentation masks must only contain integer values.")
    }

    # Check if img_id exists in elementMetadata
    if(!is.null(img_id) && !(img_id %in% colnames(mcols(mask)))){
        stop("'img_id' not in 'mcols(mask)'.")
    }

    # Check if img_id contain unique entries
    l_unique <- length(unique(mcols(mask)[,img_id]))
    l_all <- length(mcols(mask)[,img_id])
    if(l_unique < l_all){
        stop("Entries to in the 'mcols(mask)[,img_id]' slot are not unique.")
    }
}

# Check image valididty
#' @importFrom S4Vectors mcols
.valid.image <- function(image, img_id){
    if(!is(image, "CytoImageList")){
        stop("Please provide the image(s) in form of a 'CytoImageList' object")
    }

    # Check if Image_id exists in elementMetadata
    if(!is.null(img_id) && !(img_id %in% colnames(mcols(image)))){
        stop("'img_id' not in 'mcols(image)'.")
    }

    # Check if img_id contain unique entries
    l_unique <- length(unique(mcols(image)[,img_id]))
    l_all <- length(mcols(image)[,img_id])
    if(l_unique < l_all){
        stop("Entries to in the 'mcols(image)[,img_id]' slot are not unique.")
    }
}

# Check if entries in objects are matching
#' @importFrom S4Vectors mcols
.valid.matchObjects.plotCells <- function(object, mask, img_id){
    # Check if image ids match
    sce_images <- unique(colData(object)[,img_id])
    mask_images <- mcols(mask)[,img_id]
    if(all(!(mask_images %in% sce_images))){
        stop("None of the images appear in 'object'.\n",
            "Please make sure to set the image ids correctly.")
    }
}

#' @importFrom S4Vectors mcols
.valid.matchObjects.plotPixels <- function(object, mask, image, img_id){
    if(!is.null(mask)){
        if(is.null(img_id)){
            stop("'img_id' is missing.")
        }
        image_images <- mcols(image)[,img_id]
        mask_images <- mcols(mask)[,img_id]
        if(!identical(mask_images, image_images)){
            stop("Mask and image ids must be identical.")
        }

        image_dims <- unlist(lapply(image, function(x){dim(x)[c(1,2)]}))
        mask_dims <- unlist(lapply(mask, function(x){dim(x)[c(1,2)]}))
        if(!identical(as.numeric(image_dims), as.numeric(mask_dims))){
            stop("Mask and image entries must have the same dimensions.")
        }
    }

    if(!is.null(object)){
        if(is.null(img_id)){
            stop("'img_id' is missing.")
        }
        image_images <- mcols(image)[,img_id]
        sce_images <- unique(colData(object)[,img_id])
        if(all(!(sce_images %in% image_images))){
            stop("Image ids in 'mcols(image)' and",
                    " 'colData(object)' do not match")
        }
    }
}

#' @importFrom S4Vectors mcols
.valid.matchObjects.measureObjects <- function(mask, image, img_id){
    if(is.null(img_id)){
        stop("'img_id' is missing.")
    }
    
    image_images <- mcols(image)[,img_id]
    mask_images <- mcols(mask)[,img_id]
    if(!identical(mask_images, image_images)){
        stop("Mask and image ids must be identical.")
    }
        
    image_dims <- unlist(lapply(image, function(x){dim(x)[c(1,2)]}))
    mask_dims <- unlist(lapply(mask, function(x){dim(x)[c(1,2)]}))
    if(!identical(as.numeric(image_dims), as.numeric(mask_dims))){
        stop("Mask and image entries must have the same dimensions.")
    }
}

#' @importFrom S4Vectors isEmpty
.valid.colour_by <- function(colour_by, object, image,
        call.arg = c("plotCells", "plotPixels")){
    call.arg <- match.arg(call.arg)

    if (call.arg == "plotCells"){
        # colour_by takes either the rownames or colData entries
        # check if colour_by is either in the rownames
        # or in the colData slot
        # Check if all colour_by entries are in either
        # the rownames or colData slot
        if(is.null(object)){
            stop("Please provide a SingleCellExperiment 'object'.")
        }

        if(is.null(colData(object)) || isEmpty(colData(object))){
            if(!all(colour_by %in% rownames(object))){
                stop(paste("'colour_by' not in 'rownames(object)'",
                            "or the 'colData(object)' slot."))
            }
        } else {
            if(sum(colour_by %in% rownames(object)) > 0L &&
                sum(colour_by %in% colnames(colData(object))) > 0L){
                stop("'colour_by' entries found in 'rownames(object)'\n",
                    "and 'colData(object)' slot.\n",
                    "Please select either rownames or colData entries.")
            }
            if(!all(colour_by %in% rownames(object)) &&
                !all(colour_by %in% colnames(colData(object)))){
                stop(paste("'colour_by' not in 'rownames(object)'",
                            "or the 'colData(object)' slot."))
            }
            if(all(colour_by %in% colnames(colData(object))) &&
                length(colour_by) > 1L){
                stop(paste("Only one 'colour_by' entry allowed",
                            "when selecting a 'colData(object)' slot."))
            }
            if(all(colour_by %in% rownames(object)) && length(colour_by) > 6L){
                stop(paste("Only six 'colour_by' entries",
                            "allowed when selecting marker expression."))
            }
        }
    }

    if (call.arg == "plotPixels"){
        # Here, colour_by takes only the channelNames entries
        # check if colour_by is the channelNames slot
        if(is.null(channelNames(image))){
            stop("'channelNames(image)' not set.")
        }
        if(!all(colour_by %in% channelNames(image))){
            stop("'colour_by' not in 'channelNames(image)' slot.")
        }
        if(length(colour_by) > 6L){
            stop("Only six 'colour_by' entries allowed.")
        }
    }
}

#' @importFrom S4Vectors isEmpty
.valid.outline_by <- function(outline_by, object, mask, image){
    # outline_by only takes entries from the colData slot
    # Check if all outline_by entries are in the colData slot
    if(!is.null(image) && (is.null(object) || is.null(mask))){
        stop("Outlining cells: provide a SingleCellExperiment 'object' \n",
            "and segmentation 'mask' object.")
    }

    if(is.null(object)){
        stop("Please provide a SingleCellExperiment 'object'.")
    }

    if(length(outline_by) > 1L){
        stop("Only one 'outline_by' entry allowed.")
    }

    if(is.null(colData(object)) || isEmpty(colData(object))){
        stop("'outline_by' not in the 'colData(object)' slot.")
    } else {
        if(!all(outline_by %in% colnames(colData(object)))){
            stop("'outline_by' not in 'colData(object)' slot.")
        }
    }
}

.valid.colour <- function(colour, colour_by, outline_by, object, image){
    if(!is.list(colour)){
        stop("'colour' is a list of entries in which each name specifies\n",
            "an entry of 'colour_by' and/or 'outline_by'")
    }
    if(is.null(names(colour))){
        stop("'colour': please specify the entries that should be coloured.")
    }
    #if(!is.null(colour_by) || !is.null(outline_by)){
    #    valid_names <- c(colour_by, outline_by)
    #    if(!all(names(colour) %in% valid_names)){
    #        stop(paste("'names(colour)' do not match",
    #                    "with 'colour_by' and/or 'outline_by'"))
    #    }
    #}
    cur_entries <- unlist(lapply(colour, is.null))
    if(sum(cur_entries) > 0L){
        stop("Empty entries not allowed in 'colour'")
    }

    # Error if only few markers should be coloured
    if(!is.null(image)){
        cur_logical <- !is.null(colour_by)
    } else{
        cur_logical <- !is.null(colour_by) &&
                all(colour_by %in% rownames(object))
    }
    if(cur_logical){
        if(sum(colour_by %in% names(colour)) > 0L &&
            sum(colour_by %in% names(colour)) < length(colour_by)){
            stop("Please specify colour gradients for all features.")
        }
        if(all(colour_by %in% names(colour)) &&
            sum(unlist(lapply(colour[colour_by], length)) <= 1L)){
            stop(paste("Please specify at least two",
                        "colours when colouring features."))
        }
    }
    if(!is.null(object) && !is.null(colour_by) &&
        all(colour_by %in% colnames(colData(object))) &&
        !is.null(colour[[colour_by]])){
        cur_entries <- unique(colData(object)[,colour_by])
        if(length(cur_entries) > 23L && is.numeric(cur_entries) &&
            is.null(names(colour[[colour_by]]))){
            if(length(colour[[colour_by]]) <= 1){
                stop(paste("Please specify at least two",
                            "colours when colouring continous entries."))
            }
        } else if(!all(cur_entries %in% names(colour[[colour_by]]))){
            stop("Please specify colours for all 'colour_by' levels.")
        }
    }
    if(!is.null(outline_by) &&
        all(outline_by %in% colnames(colData(object))) &&
        !is.null(colour[[outline_by]])){
        cur_entries <- unique(colData(object)[,outline_by])
        if(length(cur_entries) > 23L && is.numeric(cur_entries) &&
            is.null(names(colour[[outline_by]]))){
            if(length(colour[[outline_by]]) <= 1){
                stop(paste("Please specify at least two",
                            "colours when colouring continous entries."))
            }
        } else if(!all(cur_entries %in% names(colour[[outline_by]]))){
            stop("Please specify colours for all 'outline_by' levels.")
        }
    }
}

.valid.bcg <- function(bcg, colour_by){
    if(!is.list(bcg)){
        stop("'bcg': please specify a list object")
    }

    if(is.null(names(bcg))){
        stop("'bcg': please indicate which channels to modify")
    }

    if(is.null(colour_by)){
        stop("'colour_by': please indicate which channels to modify")
    }

    if(sum(names(bcg) %in% colour_by) == 0L){
        stop("'bcg': names do not match 'colour_by' argument")
    }

    cur_length <- unlist(lapply(bcg, length))
    if(!all(cur_length == 3L)){
        stop("'bcg': specify in form of c(0,1,1)")
    }

    cur_logical <- unlist(lapply(bcg, is.numeric))
    if(!all(cur_logical)){
        stop("'bcg': specify in form of numeric entries")
    }
}

# Valid features for measuring objects
.valid.features <- function(feature_types, basic_feature, shape_feature, moment_feature, 
                            haralick_feature, basic_quantiles,
                            haralick_nbins, haralick_scales){
    if (!all(feature_types %in% c("basic", "shape", "moment", "haralick"))) {
        stop("Only features of type 'basic', 'shape', 'moment' and 'haralick' are allowed.")
    }
    
    if (!("basic" %in% feature_types) | is.null(basic_feature)) {
        stop("Please specify a basic feature to characterise the marker expression per cell.")
    }

    if (length(basic_feature) > 1) {
        stop("Only one intensity feature can be used to characterise the expression of each marker in each object.")
    }
    
    if (!is.null(basic_quantiles)) {
        
        if (!all(is.numeric(basic_quantiles)) | !all(basic_quantiles < 1) | !all(basic_quantiles > 0)){
            stop("Only numeric quantiles between 0 and 1 allowed.")
        }
        
        str_quantiles <- lapply(strsplit(as.character(basic_quantiles), "\\."), paste, collapse = "")
        basic_features_allowed <- c("mean", "sd", "mad", paste0("q", unlist(str_quantiles)))
    } else {
        basic_features_allowed <- c("mean", "sd", "mad")
    }
        
    if (!basic_feature %in% basic_features_allowed) {
        stop("Only basic features of type 'mean', 'sd', 'mad' or the selected quantiles allowed.")
    }
    
    if ("shape" %in% feature_types) {
        if (is.null(shape_feature)) {
            stop("Specify at least one shape feature.")
        }
        
        if (!all(shape_feature %in% c("area", "perimeter", "radius.mean", "radius.sd", "radius.max", "radius.min"))) {
            stop("Only shape features of type 'area', 'perimeter', 'radius.mean', 'radius.sd', 'radius.max', 'radius.min' allowed.")
        }
    }
    
    if ("moment" %in% feature_types) {
        if (is.null(moment_feature)) {
            stop("Specify at least one moment feature.")
        }
    
        if (!all(moment_feature %in% c("cx", "cy", "majoraxis", "eccentricity", "theta"))) {
            stop("Only moment features of type 'cx', 'cy', 'majoraxis', 'eccentricity', 'theta' allowed.")
        }
    }
    
    if ("haralick" %in% feature_types) {
        if (is.null(haralick_feature)) {
            stop("Specify at least one haralick feature.")
        }
        
        if (is.null(haralick_nbins) | !is.numeric(haralick_nbins) | length(haralick_nbins) > 1) {
            stop("Specify the number of bins into which intensity levels are binned.")
        }
        
        if (is.null(haralick_scales) | !is.numeric(haralick_scales)) {
            stop("Specify the scale (in pixels) over which the haralick features are computed.")
        }
        
        haralick_features_allowed <- c("asm", "con", "cor", "var", "idm", "sav", "sva",
                                       "sen", "ent", "dva", "den", "f12",
                                       "f13")
        haralick_features_allowed <- paste0(rep(haralick_features_allowed, length(haralick_scales)), 
                                            ".",
                                            rep(paste0("s", haralick_scales), each = length(haralick_features_allowed)))
        
        if (!all(haralick_feature %in% haralick_features_allowed)) {
            stop("Only haralick features of type ", paste(haralick_features_allowed, collapse = ", "), " allowed.")
        }
    }
}
    
.valid.compImage.input <- function(object, sm){
        
    # Input checks
    if (!is(object,  "CytoImageList")) {
        stop("'object' not of type 'CytoImageList'")
    }
    if (!is(sm, "matrix")) {
        stop("'sm' is not of type 'matrix'")
    }
    if (!all(sm >= 0) | !all(sm <= 1)) {
        stop("'sm' contains values outside 0-1 range.")
    }
        
    # Check channelNames
    cur_chs <- channelNames(object)
    
    if (!all(grepl("^[A-Za-z]{1,2}[0-9]{2,3}Di$", cur_chs))) {
        stop("Not all 'channelNames(object)' are of the format (mt)(mass)Di.")
    }
        
    # Check spillover matrix
    if (!isTRUE(all.equal(colnames(sm), channelNames(object)))) {
        stop("'channelNames(object)' and 'colnames(sm)' do not match.")
    }
}

