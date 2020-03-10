#' @title Further plotting parameters for the SingleCellMapper package
#' @name plotting-param
#'
#' @description
#' TODO
#'
#' @param legend list(title, size) also allow NULL
#' @param return_plot TODO
#' @param return_images TODO
#' @param save_images TODO
#' @param missing_colour TODO
#' @param scale_bar TODO
#' @param image_title TODO
#'
#'@section Setting further parameters:
#' TODO
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
.plottingParam <- function(dotArgs){

  # scale_bar
  .valid.scalebar(scale_bar)

  # image_title
  .valid.imagetitle(image_title)

  missing_colour <- "gray"
  scale_bar <- list(length = 20,
                   label = NULL,
                   cex = 1,
                   lwd = 2,
                   colour = "white",
                   position = "bottomright",
                   margin = c(10,10))
  image_title <- list(text =  NULL,
                     position = "top",
                     cex = 1,
                     colour = "white",
                     margin = c(0,0),
                     font = 2)
}

# missing_colour has to be a valid colour
if(!is.null(missing_colour)){
  res <- try(col2rgb(missing_colour), silent=TRUE)
  if(class(res) == "try-error"){
    stop("'missing_colour' not a valid colour.")
  }
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
       !all(names(scalebar) %in% c("length", "label", "cex", "lwd", "colour", "position", "margin"))){
      stop(error.scalebar)
    }
    if(!is.null(scalebar$position) && !scalebar$position %in% c("topleft", "topright", "bottomleft", "bottomright")){
      stop(paste0(error.scalebar, ": position should be 'topleft', 'topright', 'bottomleft', or 'bottomright'"))
    }
    if(!is.null(scalebar$margin) && length(scalebar$margin) != 2){
      stop(paste0(error.scalebar, ": 'margin' should contain two elements corresponding to x and y margin"))
    }
    if(!is.null(scalebar$length) && !is.numeric(scalebar$length)){
      stop(paste0(error.scalebar, ": 'length' should be numeric"))
    }
    if(!is.null(scalebar$margin) && !is.numeric(scalebar$margin)){
      stop(paste0(error.scalebar, ": 'margin' should be numeric"))
    }
    if(!is.null(scalebar$cex) && !is.numeric(scalebar$cex)){
      stop(paste0(error.scalebar, ": 'cex' should be numeric"))
    }
    if(!is.null(scalebar$lwd) && !is.numeric(scalebar$lwd)){
      stop(paste0(error.scalebar, ": 'lwd' should be numeric"))
    }
  }
}

# Validity of image_title input
.valid.imagetitle <- function(imagetitle){
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
    if(!is.null(imagetitle$position) &&
       !imagetitle$position %in% c("top", "bottom", "topleft", "bottomleft", "topright", "bottomright")){
      stop(paste0(error.imagetitle, ": position should be 'top', 'bottom',
                  'topleft', 'topright', 'bottomleft' or 'bottomright'"))
    }
    if(!is.null(imagetitle$cex) && !is.numeric(imagetitle$cex)){
      stop(paste0(error.imagetitle, ": 'cex' should be numeric"))
    }
    if(!is.null(imagetitle$margin) && length(imagetitle$margin) != 2){
      stop(paste0(error.imagetitle, ": 'margin' should contain two elements corresponding to x and y margin"))
    }
    if(!is.null(imagetitle$margin) && !is.numeric(imagetitle$margin)){
      stop(paste0(error.imagetitle, ": 'margin' should be numeric"))
    }
  }
}

