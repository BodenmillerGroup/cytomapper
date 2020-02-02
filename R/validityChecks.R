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

      if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff"))) > 0){
        stop("The provided path contains file-types other than 'jpeg', 'tiff' or 'png'.\n",
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

      if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff"))) > 0){
        stop("The provided path contains file-types other than 'jpeg', 'tiff' or 'png'.\n",
             "Please provide a correct regular expression in the 'pattern' argument to select correct images.")
      }

    } else {
      if(!tools::file_ext(x) %in% c("jpeg", "png", "tiff")){
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
    if(sum(!(unique(exten) %in% c("jpeg", "png", "tiff"))) > 0){
      stop("The files are of type other than 'jpeg', 'tiff' or 'png'.\n",
           "Please only provide files of the supported file-type..")
    }

    out <- x

  }
  return(out)
}




