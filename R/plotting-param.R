#' @title Further plotting parameters for the cytomapper package
#' @name plotting-param
#'
#' @description
#' The \code{\link{plotCells}} and \code{\link{plotPixels}} functions share a
#' number of parameter that can be set to change the visual representation of
#' plotted images.
#'
#' @param missing_colour a single character specifying a valid colour. Cells
#'   that are not contained in the SingleCellExperiment object will be coloured
#'   based on \code{missing_colour}. In the \code{\link{plotPixels}} function,
#'   \code{missing_colour} defines the outline of the cells if \code{outline_by}
#'   is not set.
#' @param background_colour (only \code{\link{plotCells}}) a single character
#'   specifying a valid colour that is set as the background of the image.
#' @param scale_bar a list specifying features of the scale bar. One or multiple
#'   of the following entries are supported:
#' \itemize{
#'   \item \code{length}: numeric length in pixels (default 20).
#'   \item \code{label}: single character specifying the scale bar label (default
#'   "20").
#'   \item \code{cex}: numeric value indicating the size of the scale bar label.
#'   \item \code{lwd}: numeric value indicating the line width of the scale bar.
#'   \item \code{colour}: single character specifying the colour of scale bar and
#'   label (default "white").
#'   \item \code{position}: position of scale bar. Supported values: "topleft",
#'   "topright", "bottomleft", "bottomright" (default "bottomright").
#'   \item \code{margin}: vector of two numeric entries specifying the x and y margin
#'   between image boundary and the scale bar (default c(10,10)).
#'   \item \code{frame}: either "all" to display scale bar on all images or a single
#'   number specifying the image for which the scale bar should be displayed (default "all").
#' }
#' Plotting of the scale bar is suppressed if set to \code{NULL}.
#' @param image_title a list specifying features of the image titles. One or multiple
#'   of the following entries are supported:
#' \itemize{
#'   \item \code{text}: character vector of image titles. Same length as the
#'   \code{CytoImageList} object.
#'   \item \code{position}: single character specifying the position of the
#'   title. Supported entries: "top", "bottom", "topleft", "bottomleft",
#'   "topright", "bottomright" (default "top").
#'   \item \code{colour}: single character specifying the colour of image title
#'   (default "white").
#'   \item \code{margin}: vector of two numeric entries specifying the x and y margin
#'   between image boundary and the image title (default c(10,10)).
#'   \item \code{font}: numeric entry specifying the font of the image title
#'   (default 1, see \code{\link{par}} for details)
#'   \item \code{cex}: numeric value indicating the size of the image title.
#' }
#' Plotting of the image title is suppressed if set to \code{NULL}.
#' @param save_plot a list specifying how to save the plot. One or multiple
#'   of the following entries are supported:
#' \itemize{
#'   \item \code{filename}: single character specifying a valid file name.
#'   The file extension specifies the format in which the file is saved.
#'   Supported formats are: jpeg, tiff and png. If \code{display = "single"},
#'   each image will be written in an individual file.
#'   \item \code{scale}: by default the height and width of the saved image is
#'   defined by the maximum image size times the number of rows or numer of
#'   columns. This resolution is often not suffcient to clearly display the
#'   text. The \code{scale} parameter can be set to increase the resolution of
#'   the image while keeping the text size constant (default 1 but should be
#'   increased for optimal results).
#' }
#' @param return_plot logical indicating whether to return the plot (see
#'   \code{\link[grDevices]{recordPlot}} for more infos).
#' @param return_images logical indicating whether to return the coloured images
#'   in form of a \code{\linkS4class{SimpleList}} object. Each entry to this
#'   list is a three-colour \code{\linkS4class{Image}} object. However, the
#'   image title and scale bar are not retained.
#' @param legend a list specifying features of the legend. One or multiple
#'   of the following entries are supported:
#' \itemize{
#'   \item \code{colour_by.title.font}: numeric entry specifying the font of the
#'   legend title for features specified by \code{colour_by}.
#'   \item \code{colour_by.title.cex}: numeric entry specifying the size of the
#'   legend title for features specified by \code{colour_by}.
#'   \item \code{colour_by.labels.cex}: numeric entry specifying the size of the
#'   legend labels for features specified by \code{colour_by}.
#'   \item \code{colour_by.legend.cex}: (only discrete features) numeric entry
#'   specifying the size of the legend for features specified by
#'   \code{colour_by}.
#'   \item \code{outline_by.title.font}: numeric entry specifying the font of the
#'   legend title for features specified by \code{outline_by}.
#'   \item \code{outline_by.title.cex}: numeric entry specifying the size of the
#'   legend title for features specified by \code{outline_by}.
#'   \item \code{outline_by.labels.cex}: numeric entry specifying the size of the
#'   legend labels for features specified by \code{outline_by}.
#'   \item \code{outline_by.legend.cex}: (only discrete features) numeric entry
#'   specifying the size of the legend for features specified by
#'   \code{outline_by}.
#'   \item \code{margin}: numeric value indicating the margin between the
#'   legends and the outer boundary (default 2)
#' }
#' Plotting of the legend is suppressed if set to \code{NULL}.
#' @param margin numeric value indicating the gap between individual images
#'   (default 0).
#' @param display one of two possible values: "all" or "single". When set to
#'   "all", all images are displayed aat once in a grid-like fashion. When
#'   set to "single", individual images are plotted in single graphics devices.
#'   The second option is useful when saving individual images in pdf format or
#'   when displaying in Rmarkdown files.
#' @param scale logical indicating whether to scale each feature individually to
#'   its minimum/maximum across the SingleCellExperiment object (see
#'   \code{\link{plotCells}}) or across all displayed images (see
#'   \code{\link{plotCells}}). If set to \code{FALSE} each value is displayed
#'   relative to the maximum of all selected features.
#' @param interpolate a logical indicating whether to apply
#'   linear interpolation to the image when drawing (see
#'   \code{\link[graphics]{rasterImage}}) (default TRUE).
#'
#' @return a list if return_images and/or return_plot is TRUE.
#' \itemize{
#'   \item \code{plot}: a single plot object (\code{display = "all"}) or a list
#'   of plot objects (\code{display = "single"})
#'   \item \code{images}: a \code{\linkS4class{SimpleList}} object containing
#'   three-colour \code{\linkS4class{Image}} objects.
#' }
#'
#' @seealso \code{\link{plotCells}} and \code{\link{plotCells}} for the main plotting functions
#'
#' @examples
#' data("pancreasImages")
#' data("pancreasMasks")
#' data("pancreasSCE")
#'
#' # Setting missing colour
#' plotCells(pancreasMasks, missing_colour = "blue")
#'
#' # Setting background colour
#' plotCells(pancreasMasks, background_colour = "blue")
#'
#' # Setting the scale bar
#' plotCells(pancreasMasks, scale_bar = list(length = 10,
#'                                           cex = 2,
#'                                           lwd = 3,
#'                                           colour = "red",
#'                                           position = "bottomleft",
#'                                           margin = c(5,5),
#'                                           frame = 3))
#'
#' # Setting the image title
#' plotCells(pancreasMasks,
#'           image_title = list(text = c("image1", "image2", "image3"),
#'                              position = "topleft",
#'                              colour = "blue",
#'                              margin = c(0,5),
#'                              font = 2,
#'                              cex = 2))
#'
#' # Return plot
#' cur_out <- plotPixels(pancreasImages, return_plot = TRUE)
#' cur_out$plot
#'
#' # Return images
#' cur_out <- plotPixels(pancreasImages, return_images = TRUE)
#' cur_out$images
#'
#' # Setting the legend
#' plotCells(pancreasMasks, object = pancreasSCE,
#'           img_id = "ImageNb", cell_id = "CellNb",
#'           colour_by = c("SMA", "CD44"),
#'           outline_by = "CellType",
#'           legend = list(colour_by.title.font = 0.5,
#'                         colour_by.title.cex = 0.5,
#'                         colour_by.labels.cex = 0.5,
#'                         outline_by.legend.cex = 0.5,
#'                         margin = 0))
#'
#' # Setting the margin between images
#' plotPixels(pancreasImages, margin = 3)
#'
#' # Displaying individual images
#' plotPixels(pancreasImages, display = "single")
#'
#' # Supress scaling
#' plotPixels(pancreasImages, colour_by = c("SMA", "INS"),
#'            scale = TRUE)
#' plotPixels(pancreasImages, colour_by = c("SMA", "INS"),
#'            scale = FALSE)
#'
#' # Suppress interpolation
#' plotPixels(pancreasImages, colour_by = c("SMA", "INS"),
#'            interpolate = TRUE)
#' plotPixels(pancreasImages, colour_by = c("SMA", "INS"),
#'            interpolate = FALSE)
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' @author Nicolas Damond (\email{nicolas.damond@@dqbm.uzh.ch})
NULL

# Plotting parameters checks
.plottingParam <- function(dotArgs, image){

  # Check supported names
  cur_entries <- names(dotArgs)
  supported <- c("scale_bar", "image_title", "missing_colour",
                 "background_colour", "save_plot", "return_plot",
                 "return_images", "legend", "margin", "display",
                 "scale", "interpolate")
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
                       cex = NULL)
    dotArgs$image_title <- imagetitle
  } else {
    dotArgs$image_title <- .valid.imagetitle(dotArgs$image_title, image)
  }

  # legend
  if(!("legend" %in% names(dotArgs))){
    legendparam <- list(colour_by.title.font = 1,
                        colour_by.title.cex = NULL,
                        colour_by.labels.cex = NULL,
                        colour_by.legend.cex = NULL,
                        outline_by.title.font = 1,
                        outline_by.title.cex = NULL,
                        outline_by.labels.cex = NULL,
                        outline_by.legend.cex = NULL,
                        margin = 2)
    dotArgs$legend <- legendparam
  } else {
    dotArgs$legend <- .valid.legendparam(dotArgs$legend)
  }

  # save_plot
  if(!("save_plot" %in% names(dotArgs))){
    dotArgs$save_plot <- NULL
  } else {
    dotArgs$save_plot <- .valid.saveplot(dotArgs$save_plot)
  }

  # return_plot
  if(!("return_plot" %in% names(dotArgs))){
    dotArgs$return_plot <- FALSE
  } else {
    if(!is.logical(dotArgs$return_plot)){
      stop("Invalid 'return_plot' entry.")
    }
    dotArgs$return_plot <- dotArgs$return_plot
  }

  # return_images
  if(!("return_images" %in% names(dotArgs))){
    dotArgs$return_images <- FALSE
  } else {
    if(!is.logical(dotArgs$return_images)){
      stop("Invalid 'return_images' entry.")
    }
    dotArgs$return_images <- dotArgs$return_images
  }

  # return_images
  if(!("margin" %in% names(dotArgs))){
    dotArgs$margin <- 0L
  } else {
    if(!is.numeric(dotArgs$margin) ||
       length(dotArgs$margin) != 1L ||
       dotArgs$margin < 0L){
      stop("Invalid 'margin' entry.")
    }
    dotArgs$margin <- dotArgs$margin
  }

  # return_images
  if(!("display" %in% names(dotArgs))){
    dotArgs$display <- "all"
  } else {
    if(!is.character(dotArgs$display) ||
       !(dotArgs$display %in% c("all", "single"))){
      stop("Invalid 'display' entry.")
    }
    dotArgs$display <- dotArgs$display
  }

  # scale
  if(!("scale" %in% names(dotArgs))){
    dotArgs$scale <- TRUE
  } else {
    if(!is.logical(dotArgs$scale)){
      stop("Invalid 'scale' entry.")
    }
    dotArgs$scale <- dotArgs$scale
  }

  # scale
  if(!("interpolate" %in% names(dotArgs))){
    dotArgs$interpolate <- TRUE
  } else {
    if(!is.logical(dotArgs$interpolate)){
      stop("Invalid 'interpolate' entry.")
    }
    dotArgs$interpolate <- dotArgs$interpolate
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
      if(length(scalebar$label) != 1L){
        stop(paste0(error.scalebar, ": \n",
                    "'label' should be a single entry"))
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
      imagetitle$cex <- NULL
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

# Validity of legend input
.valid.legendparam <- function(legendparam){
  error.legendparam <- "Invalid entry to the 'legend' list object"

  if(!is.null(legendparam)){
    if(!is.list(legendparam)){
      stop(error.legendparam)
    }

    if(is.null(names(legendparam)) ||
       !all(names(legendparam) %in% c("colour_by.title.font",
                                     "colour_by.title.cex",
                                     "colour_by.labels.cex",
                                     "colour_by.legend.cex",
                                     "outline_by.title.font",
                                     "outline_by.title.cex",
                                     "outline_by.labels.cex",
                                     "outline_by.legend.cex",
                                     "margin"))){
      stop(error.legendparam)
    }

    if("colour_by.title.font" %in% names(legendparam)){
      cur_param <- legendparam$colour_by.title.font
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'colour_by.title.font' should be a single number"))
      }
    } else {
      legendparam$colour_by.title.font <- 1
    }

    if("colour_by.title.cex" %in% names(legendparam)){
      cur_param <- legendparam$colour_by.title.cex
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'colour_by.title.cex' should be a single number"))
      }
    } else {
      legendparam$colour_by.title.cex <- NULL
    }

    if("colour_by.labels.cex" %in% names(legendparam)){
      cur_param <- legendparam$colour_by.labels.cex
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'colour_by.labels.cex' should be a single number"))
      }
    } else {
      legendparam$colour_by.labels.cex <- NULL
    }

    if("colour_by.legend.cex" %in% names(legendparam)){
      cur_param <- legendparam$colour_by.legend.cex
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'colour_by.legend.cex' should be a single number"))
      }
    } else {
      legendparam$colour_by.legend.cex <- NULL
    }

    if("outline_by.title.font" %in% names(legendparam)){
      cur_param <- legendparam$outline_by.title.font
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'outline_by.title.font' should be a single number"))
      }
    } else {
      legendparam$outline_by.title.font <- 1
    }

    if("outline_by.title.cex" %in% names(legendparam)){
      cur_param <- legendparam$outline_by.title.cex
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'outline_by.title.cex' should be a single number"))
      }
    } else {
      legendparam$outline_by.title.cex <- NULL
    }

    if("outline_by.labels.cex" %in% names(legendparam)){
      cur_param <- legendparam$outline_by.labels.cex
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'outline_by.labels.cex' should be a single number"))
      }
    } else {
      legendparam$outline_by.labels.cex <- NULL
    }

    if("outline_by.legend.cex" %in% names(legendparam)){
      cur_param <- legendparam$outline_by.legend.cex
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'outline_by.legend.cex' should be a single number"))
      }
    } else {
      legendparam$outline_by.legend.cex <- NULL
    }

    if("margin" %in% names(legendparam)){
      cur_param <- legendparam$margin
      if(length(cur_param) != 1L ||
         !is.numeric(cur_param)){
        stop(paste0(error.legendparam, ": \n",
                    "'margin' should be a single number"))
      }
    } else {
      legendparam$margin <- 2
    }
  }
  return(legendparam)
}

# Validity of missing_colour input
#' @importFrom grDevices col2rgb
.valid.missingcolour <- function(missingcolour){
  if(!is.null(missingcolour)){
    res <- try(col2rgb(missingcolour), silent=TRUE)
    if(class(res)[1] == "try-error"){
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
    if(class(res)[1] == "try-error"){
      stop("'background_colour' not a valid colour.")
    }
  } else {
    backgroundcolour <- "#000000"
  }
  return(backgroundcolour)
}

# Validity of save_plot input
#' @importFrom tools file_ext
.valid.saveplot <- function(saveplot){

  error.saveplot <- "Invalid entry to the 'save_plot' list object"

  if(!is.null(saveplot)){
    if(is.null(names(saveplot)) ||
       !all(names(saveplot) %in% c("filename", "scale"))){
      stop(error.saveplot)
    }

    if("filename" %in% names(saveplot)){
      if(length(saveplot$filename) != 1L || !is.character(saveplot$filename)){
        stop(paste0(error.saveplot, ": \n",
                    "Invalid entry of 'filename'"))
      }

      if(is.character(saveplot$filename)){
        cur_ext <- file_ext(saveplot$filename)
        if(cur_ext == ""){
          stop(paste0(error.saveplot, ": \n",
                      "Please provide a file extension indicating in format to save the image."))
        }
        if(!(cur_ext %in% c("tiff", "png", "jpeg"))){
          stop(paste0(error.saveplot, ": \n",
                      "'filename' only supports 'tiff', 'png' and 'jpeg' file types."))
        }
      }
    } else {
      stop(paste0(error.saveplot, ": \n",
                  "'filename' not provided."))
    }

    if("scale" %in% names(saveplot)){
      if(length(saveplot$scale) != 1L || !is.numeric(saveplot$scale)){
        stop(paste0(error.saveplot, ": \n",
                    "Invalid entry of 'scale'"))
      }
    } else {
      saveplot$scale <- 1
    }
  }

  return(saveplot)
}
