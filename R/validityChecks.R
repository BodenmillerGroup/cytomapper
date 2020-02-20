#' @importFrom tools file_ext
.valid.loadImage.input <- function(x, pattern){

  # Check if input is character
  if(!is.character(x)){
    stop("Please provide a string input indicating a single file/n",
         ", a path or a vector of files.")
  }

  # Further checks depending on length of object
  if(length(x) == 1){
    if(!file.exists(x)){

      stop("The provided file or path does not exist.\n",
           "Make sure the file or path is accessible from the current location.")

    }

    if(dir.exists(x) & is.null(pattern)){

      # Check if path only contains images
      exten <- sapply(list.files(x), tools::file_ext)

      if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff", "tif", "jpg"))) > 0){
        stop("The provided path contains file-types other than 'jpeg', 'tiff', or 'png'.\n",
             "Please provide a correct regular expression in the 'pattern' argument to select correct images.")
      }

      message("All files in the provided location will be read in.")
      out <- list.files(x, full.names = TRUE)

    } else if(dir.exists(x) & !is.null(pattern)){

      # Check pattern
      if(!is.character(pattern) & !is.factor(pattern)){
        stop("Please provide a single character, character vector or factor as pattern input .\n")
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
        stop("The pattern does not match any of the files in the provided directory.")
      }

      # Check if all of the files are of the supported format
      exten <- sapply(out, tools::file_ext)

      if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff", "tif", "jpg"))) > 0){
        stop("The provided path contains file-types other than 'jpeg', 'tiff' or 'png'.\n",
             "Please provide a correct regular expression in the 'pattern' argument to select correct images.")
      }

    } else {
      if(!tools::file_ext(x) %in% c("jpeg", "png", "tiff", "tif", "jpg")){
        stop("The provided file is not of type 'jpeg', 'tiff' or 'png'.\n",
             "Other image types are not supported.")
      }
      out <- x
    }
  } else {
    # Check if files exists
    cur_check <- sapply(x, file.exists)
    if(sum(!cur_check) > 0){
      stop("One or multiple files do not exist.\n",
           "Please correct the input.")
    }

    # Check if files are os supported format
    exten <- sapply(x, tools::file_ext)
    if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff", "tif", "jpg"))) > 0){
      stop("The files are of type other than 'jpeg', 'tiff' or 'png'.\n",
           "Please only provide files of the supported file-type..")
    }

    out <- x

  }
  return(out)
}


# Function to check if ImageList elements can be correctly replaced
#' @importFrom methods is
.valid.Image.setting <- function(x, i, value){
  # Check if value is Image or ImageList
  if(!is.null(value) && !(is(value, "Image") ||
                          is(value, "ImageList"))){
    stop("Invalid replacement operation: \n",
         "Only 'Image' or 'ImageList' objects allowed.")
  }

  # If i is not character, both x and value need to be named,
  # or both x and value need to be unnamed
  error <- c()
  if(!is.null(value)){
    if(!is.character(i)){
      if(is.null(names(x))){
        if(is(value, "ImageList") && !is.null(names(value))){
          error <- "Cannot merge named and unnamed ImageList object."
        }
      } else {
        if(is(value, "Image")){
          error <- "Cannot set Image object to named ImageList."
        } else if(is.null(names(value))){
          error <- "Cannot merge named and unnamed ImageList object."
        }
      }
    } else {
      if(is.null(names(x))){
        error <- paste("'i' is of type character. \n",
        "This setting is only allowed for named ImageList objects")
      }
    }
  }

  if(length(error > 0)){
    stop("Invalid replacement operation: \n",
         error)
  }
}

# Chcks if channels can be replaced
#' @importFrom methods is
.valid.Channel.setting <- function(x, i, value){
  # Only ImageList objects are supported
  if(!is.null(value) && !is(value, "ImageList")){
    stop("Invalid replacement operation: \n",
         "Only 'ImageList' objects allowed.",
         "To alter Image objects, see ?Image.")
  }

  # Check if replacement has the same length
  if(!is.null(value) && length(x) != length(value)){
    stop("Invalid replacement operation: \n",
         "Replacement needs to have same length as 'x'")
  }

  # Check if names of x and value match
  if(!is.null(value) && !is.null(names(x)) && !is.null(names(value))){
    if(!identical(names(x), names(value))){
      stop("Invalid replacement operation: \n",
           "Names of 'x' and 'value' do not match.")
    }
  }

  # Check if number of channels is same as length(i)
  if(!is.null(value) && length(i) != dim(value[[1]])[3]){
    stop("Invalid replacement operation: \n",
         "Number of replacement channels is not the same as \n",
         "number of channels to replace.")
  }

  # Check if channelNames are set if is.character(i)
  if(is.character(i) && is.null(channelNames(x))){
    stop("Invalid replacement operation: \n",
         "Trying to set a named channel in an unnamed ImageList.")
  }
}

# Check sce validity
#' @importFrom SummarizedExperiment assayNames
.valid.sce <- function(object, image_ID, cell_ID, exprs_values){
  if(!is(object, SingleCellExperiment)){
    stop("'object' is not of type 'SingleCellExperiment'.")
  }

  if(is.null(image_ID) || is.null(cell_ID)){
    stop("Please provide an 'image_ID' and 'cell_ID' argument")
  }

  if(!is.character(image_ID) || length(image_ID) > 1 ||
     !is.character(cell_ID) || length(cell_ID) > 1){
    stop("Invalid argument for 'image_ID' and/or 'cell_ID'.")
  }

  if(is.null(colData(object))){
    stop("Please store the image- and cell-level metadata in the 'colData' slot of 'object'.")
  }

  if(!(image_ID %in% colnames(colData(object))) ||
     !(cell_ID %in% colnames(colData(object)))){
    stop("'image_ID' or 'cell_ID' not in 'colData(object)'.")
  }

  if(!all(colData(sce)[,cell_ID] == floor(colData(sce)[,cell_ID]))){
    stop("Cell IDs should only contain integer values.")
  }

  if(!(exprs_values %in% assayNames(object))){
    stop("'exprs_values' not an assay entry in 'object'.")
  }
}

# Check mask valididty
.valid.mask <- function(mask, image_ID){
  if(!is(mask, "ImageList")){
    stop("Please provide the segementation mask(s) in form of an 'ImageList' object")
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

  # Check if Image_ID exists in elementMetadata
  if(!(image_ID %in% colnames(mcols(mask)))){
    stop("'image_ID' not in 'mcols(mask)'.")
  }
}

# Check image valididty
.valid.image <- function(image, ...){
  # TODO
}

# Check if entries in objects are matching
.valid.matchObjects <- function(object, image, image_ID, cell_ID){
  # Check if image IDs match
  sce_images <- unique(colData(object[,image_ID]))
  image_images <- mcols(image)[,image_ID]
  if(all(!(image_images %in% sce_images))){
    stop("None of the images appear in 'object'.\n",
         "Please make sure to set the image IDs correctly.")
  }
}

# Check plotCells input
.valid.plotCells.input <- function(object, mask, image_ID, colour_by, outline_by,
                                   subset_images, save_images,
                                   return_images, col, missing_col,
                                   scale_bar){

  # colour_by takes either the rownames or colData entries
  # check if colour_by is either in the rownames
  # or in the colData slot
  # Check if all colour_by entries are in either
  # the rownames or colData slot
  if(!is.null(colour_by)){
    if(is.null(colData(object)) || isEmpty(colData(object))){
      if(!all(colour_by %in% rownames(object))){
        stop("'colour_by' not in 'rownames(object)' or the 'colData(object)' slot.")
      }
    } else {
      if(sum(colour_by %in% rownames(object)) > 0L &&
         sum(colour_by %in% colnames(colData(object))) > 0L){
        stop("'colour_by' entries found in 'rownames(object)' and 'colData(object)' slot.\n",
             "Please select either rownames or colData entries.")
      }
      if(!all(colour_by %in% rownames(object)) ||
         !all(colour_by %in% colnames(colData(object)))){
        stop("'colour_by' not in 'rownames(object)' and 'colData(object)' slot.")
      }
      if(all(colour_by %in% colnames(colData(object))) && length(colour_by) > 1L){
        stop("Only one 'colour_by' entry allowed when selecting a 'colData(object)' slot.")
      }
      if(all(colour_by %in% rownames(object)) && length(colour_by) > 6L){
        stop("Only six 'colour_by' entries allowed when selecting marker expression.")
      }
    }
  }

  # outline_by only takes entries from the colData slot
  # Check if all outline_by entries are in the colData slot
  if(!is.null(outline_by)){
    if(is.null(colData(object)) || isEmpty(colData(object))){
      stop("'outline_by' not in the 'colData(object)' slot.")
    } else {
      if(!all(colour_by %in% colnames(colData(object)))){
        stop("'colour_by' not in 'colData(object)' slot.")
      }
    }
  }

  # subset_images need to be either numeric, a logical,
  # a character and part of names(mask) or a character
  # and part of mcols(mask)$image_ID
  if(!is.null(subset_images)){
    if(!is.numeric(subset_images) && !is.character(subset_images) &&
       !is.logical(subset_images)){
      stop("'subset_images' has to be numeric, logical or a character")
    }
    if(is.logical(subset_images) &&
       length(subset_images) != length(mask)){
      stop("Invalid 'subset_images' argument.")
    }
    if(is.character(subset_images)){
      if(is.null(names(mask)) && !(image_ID %in% colnames(mcols(mask)))){
        stop("'subset_images' not part of names(mask) or mcols(mask)[,image_ID]")
      }
      if(!is.null(names(mask)) && sum(subset_images %in% names(mask) == 0)){
        if(!(image_ID %in% colnames(mcols(mask)))){
          stop("If 'mask' is unnamed, mask IDs must be provided in the mcols(mask)[,image_ID] slot.")
        }
      }
    }
  }

  # save_images is either NULL or a character string
  if(!is.null(save_images)){
    if(!is.character(save_images) || length(save_images) > 0){
      stop("'save_images' has to be a single character specifying a path and filename.")
    }
  }

  # return_images is either TRUE or FALSE
  if(!isTRUEorFALSE(return_images)){
    stop("'return_images' has to be a single logical.")
  }

  # col
  if(!is.null(col)){
    if(!is.list(col)){
      stop("'col' is a list of entries in which each name specifies\n",
           "an entry of 'colour_by' and/or 'outline_by'")
    }
    if(is.null(names(col))){
      stop("'col': please specify the entries that should be coloured.")
    }
    if(!is.null(colour_by) || !is.null(outline_by)){
      valid_names <- c(colour_by, outline_by)
      if(!all(names(col) %in% valid_names)){
        stop("'names(col)' do not match with 'colour_by' and/or 'outline_by'")
      }
    }
    cur_entries <- lapply(col, is.null)
    if(sum(cur_entries) > 0){
      stop("Empty entries not allowed in 'col'")
    }
  }

  # missing_col has to be a valid colour
  if(!is.null(missing_col)){
    res <- try(col2rgb(missing_col), silent=TRUE)
    if(class(res) == "try-error"){
      stop("'missing_col' not a valid colour.")
    }
  }

  # scale_bar has to be of the form list(length, label, position, lwd)
  if(!is.null(scale_bar)){
    if(!is.list(scale_bar)){
      stop("Invalid 'scale_bar' entry")
    }
    if(is.null(names(scale_bar)) || !all(names(scale_bar) %in%
                                         c("length", "label", "position", "lwd"))){
      stop("Invalid entry to the 'scale_bar' list object")
    }
  }
}




