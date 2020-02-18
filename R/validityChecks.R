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
.valid.sce <- function(object, image_ID, cell_ID){
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

  if(!(image_ID %in% colData(object)) || !(cell_ID %in% colData(object))){
    stop("'image_ID' or 'cell_ID' not in 'colData(object)'.")
  }
}

# Check mask valididty
.valid.mask <- function(mask, ...){

}

# Check image valididty
.valid.image <- function(image, ...){

}

# Check if entries in objects are matching
.valid.plotCells.matchObjects(object, mask,
                              image_ID, cell_ID)

# Check plotCells input
.valid.plotCells.input <- function(object, mask, image_ID,
                                   cell_ID, colour_by, outline_by,
                                   subset_images, save_image,
                                   return_image, col, scale_bar){

}




