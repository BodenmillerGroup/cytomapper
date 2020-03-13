#' @title Further plotting parameters for the SingleCellMapper package
#' @name plotting-param
#'
#' @description
#' TODO
#'
#' @param missing_colour TODO
#' @param background_colour TODO
#' @param scale_bar TODO
#' @param image_title TODO
#' @param save_image TODO
#'
#'@section Setting further parameters:
#' TODO
#' # legend list(title, size) also allow NULL
#' # return_plot TODO
#' # return_images TODO
#' # margin between images
#'
#' @return TODO
#'
#' @examples
#' # TODO
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
NULL

# Plotting parameters checks
.plottingParam <- function(dotArgs, image){

  # Check supported names
  cur_entries <- names(dotArgs)
  supported <- c("scale_bar", "image_title", "missing_colour",
                 "background_colour", "save_image")
  not_supported <- cur_entries[!(cur_entries %in% supported)]
  if(length(not_supported) > 0L){
    stop("Entries ", paste0("'", not_supported, "'", collapse = ", "), " are not supported")
  }

  # scale_bar
  if(!("scale_bar" %in% names(dotArgs))){
    cur_length <- 20
    scalebar <- list(length = cur_length,
                     label = as.character(cur_length),
                     cex = 1,
                     lwd = 2,
                     colour = "white",
                     position = "bottomright",
                     margin = c(10,10),
                     frame = "all")
    dotArgs$scale_bar <- scalebar
  } else {
    dotArgs$scale_bar <- .valid.scalebar(dotArgs$scale_bar)
  }

  # image_title
  if(!("image_title" %in% names(dotArgs))){
    imagetitle <- list(text = NULL,
                       position = "top",
                       colour = "white",
                       margin = c(10,10),
                       font = 2,
                       cex = 1)
    dotArgs$image_title <- imagetitle
  } else {
    dotArgs$image_title <- .valid.imagetitle(dotArgs$image_title, image)
  }

  # save_image
  if(!("save_image" %in% names(dotArgs))){
    dotArgs$save_image <- NULL
  } else {
    dotArgs$save_image <- .valid.saveimage(dotArgs$save_image)
  }

  # missing_colour
  dotArgs$missing_colour <- .valid.missingcolour(dotArgs$missing_colour)
  # missing_colour
  dotArgs$background_colour <- .valid.backgroundcolour(dotArgs$background_colour)

  return(dotArgs)
}

# Validity of scale_bar input
.valid.scalebar <- function(scalebar){
  error.scalebar <- "Invalid entry to the 'scale_bar' list object"
  # scale_bar has to be of the form list(length, label, lwd, colour, position, margin)
  if(!is.null(scalebar)){
    if(!is.list(scalebar)){
      stop(error.scalebar)
    }

    if(is.null(names(scalebar)) ||
       !all(names(scalebar) %in% c("length", "label", "cex", "lwd", "colour",
                                   "position", "margin", "frame"))){
      stop(error.scalebar)
    }

    if("position" %in% names(scalebar)){
      if(!(scalebar$position %in% c("topleft", "topright", "bottomleft", "bottomright"))){
        stop(paste0(error.scalebar, ": \n",
                    "position not correctly specified"))
      }
    } else {
      scalebar$position <- "bottomright"
    }

    if("margin" %in% names(scalebar)){
        if(length(scalebar$margin) != 2L ||
           !is.numeric(scalebar$margin)){
          stop(paste0(error.scalebar, ": \n",
                  "'margin' should contain two numeric elements corresponding to x and y margin"))
          }
    } else {
      scalebar$margin <- c(10,10)
    }

    if("length" %in% names(scalebar)){
      if(length(scalebar$length) != 1L ||
         !is.numeric(scalebar$length)){
        stop(paste0(error.scalebar, ": \n",
                    "'length' should be numeric and of length 1"))
        }
    } else {
      scalebar$length <- 20
    }

    if("cex" %in% names(scalebar)){
      if(!is.numeric(scalebar$cex) || length(scalebar$cex) != 1L){
        stop(paste0(error.scalebar, ": \n",
                    "'cex' should be a single number"))
      }
    } else {
      scalebar$cex <- 1
    }

    if("lwd" %in% names(scalebar)){
      if(!is.numeric(scalebar$lwd) || length(scalebar$lwd) != 1L){
        stop(paste0(error.scalebar, ": \n",
                   "'lwd' should be a single number"))
      }
    } else {
      scalebar$lwd <- 2
    }

    if("label" %in% names(scalebar)){
      if(length(scalebar$label) != 1L ||
         !is.character(scalebar$label)){
        stop(paste0(error.scalebar, ": \n",
                    "'label' should be a single character entry"))
      }
    } else {
      scalebar$label <- as.character(scalebar$length)
    }

    if("colour" %in% names(scalebar)){
      if(length(scalebar$colour) != 1L ||
         !is.character(scalebar$colour)){
        stop(paste0(error.scalebar, ": \n",
                    "'colour' should be a single character entry"))
      }
    } else {
      scalebar$colour <- "white"
    }

    if("frame" %in% names(scalebar)){
      if(length(scalebar$frame) != 1L && (!is.numeric(scalebar$frame) ||
         scalebar$frame != "all")){
        stop(paste0(error.scalebar, ": \n",
                    "'frame' should be a single integer or set to 'all'"))
      }
    } else {
      scalebar$frame <- "all"
    }
  }
  return(scalebar)
}

# Validity of image_title input
.valid.imagetitle <- function(imagetitle, image){
  error.imagetitle <- "Invalid entry to the 'image_title' list object"
  # image_title has to be of the form list(text, position, cex, colour, margin, font)
  if(!is.null(imagetitle)){
    if(!is.list(imagetitle)){
      stop(error.imagetitle)
    }

    if(is.null(names(imagetitle)) ||
       !all(names(imagetitle) %in% c("text", "position", "cex", "colour", "margin", "font"))){
      stop(error.imagetitle)
    }

    if("text" %in% names(imagetitle)){
      if(length(imagetitle$text) != length(image)){
        stop(paste0(error.imagetitle, ": \n",
                    "Please specify one title per image."))
      }
    } else {
      imagetitle$text <- NULL
    }

    if("position" %in% names(imagetitle)){
       if(!(imagetitle$position %in%
          c("top", "bottom", "topleft", "bottomleft", "topright", "bottomright"))){
          stop(paste0(error.imagetitle, ": \n",
                      "position not correctly specified"))
       }
    } else {
      imagetitle$position <- "top"
    }

    if("font" %in% names(imagetitle)){
      if(!is.numeric(imagetitle$font) ||
         length(imagetitle$font) != 1L){
        stop(paste0(error.imagetitle, ": \n",
                    "'font' should be a single number"))
      }
    } else {
      imagetitle$font <- 2
    }

    if("cex" %in% names(imagetitle)){
      if(!is.numeric(imagetitle$cex) ||
         length(imagetitle$cex) != 1L){
        stop(paste0(error.imagetitle, ": \n",
                    "'cex' should be a single number"))
      }
    } else {
      imagetitle$cex <- 1
    }

    if("margin" %in% names(imagetitle)){
      if(!is.numeric(imagetitle$margin) ||
         length(imagetitle$margin) != 2L){
        stop(paste0(error.imagetitle, ": \n",
                    "'margin' should contain two numeric elements corresponding to x and y margin"))
      }
    } else {
      imagetitle$margin <- c(10,10)
    }

    if("colour" %in% names(imagetitle)){
      if(length(imagetitle$colour) != 1L ||
         !is.character(imagetitle$colour)){
        stop(paste0(error.imagetitle, ": \n",
                    "'colour' should be a single character entry"))
      }
    } else {
      imagetitle$colour <- "white"
    }

  }
  return(imagetitle)
}

# Validity of missing_colour input
#' @importFrom grDevices col2rgb
.valid.missingcolour <- function(missingcolour){
  if(!is.null(missingcolour)){
    res <- try(col2rgb(missingcolour), silent=TRUE)
    if(class(res) == "try-error"){
      stop("'missing_colour' not a valid colour.")
    }
  } else {
    missingcolour <- "gray"
  }
  return(missingcolour)
}

# Validity of background_colour input
#' @importFrom grDevices col2rgb
.valid.backgroundcolour <- function(backgroundcolour){
  if(!is.null(backgroundcolour)){
    res <- try(col2rgb(backgroundcolour), silent=TRUE)
    if(class(res) == "try-error"){
      stop("'background_colour' not a valid colour.")
    }
  } else {
    backgroundcolour <- "#000000"
  }
  return(backgroundcolour)
}

# Validity of save_image input
#' @importFrom tools file_ext
.valid.saveimage <- function(saveimage){

  error.saveimage <- "Invalid entry to the 'save_image' list object"

  if(!is.null(saveimage)){
    if(is.null(names(saveimage)) ||
       !all(names(saveimage) %in% c("filename", "scale"))){
      stop(error.saveimage)
    }

    if("filename" %in% names(saveimage)){
      if(length(saveimage$filename) != 1L && !is.character(saveimage$filename)){
        stop(paste0(error.saveimage, ": \n",
                    "Invalid entry of 'filename'"))
      }

      if(is.character(saveimage)){
        cur_ext <- file_ext(saveimage)
        if(cur_ext == ""){
          stop(paste0(error.saveimage, ": \n",
                      "Please provide a file extension indicating in format to save the image."))
        }
        if(!(cur_ext %in% c("tiff", "png", "jpeg"))){
          stop(paste0(error.saveimage, ": \n",
                      "'filename' only supports 'tiff', 'png' and 'jpeg' file types."))
        }
      }
    } else {
      stop(paste0(error.saveimage, ": \n",
                  "'filename' not provided."))
    }

    if("scale" %in% names(saveimage)){
      if(length(saveimage$scale) != 1L && !is.numeric(saveimage$scale)){
        stop(paste0(error.saveimage, ": \n",
                    "Invalid entry of 'scale'"))
      }
    } else {
      saveimage$scale <- 1
    }
  }

  return(saveimage)
}
