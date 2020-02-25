# -----------------------------------------------------------------------------
# Helper functions for plotting cells and pixels
# -----------------------------------------------------------------------------

# Selection of images based on entries in SCE object or by subset
.select_images <- function(object, images, image_ID, subset_images){

  # If subset_images is not given, images are selected based on the cells
  # in the SCE object
  if(!is.null(subset_images)){
    images <- images[subset_images]
  } else {
    cur_image_ids <- unique(colData(object)[,image_ID])
    images <- images[mcols(images)[,image_ID] %in% cur_image_ids]
  }

  return(images)
}

# Colour segmentation masks based on metadata
.colourMaskByMeta <- function(object, mask, cell_ID, image_ID,
                              colour_by, cur_col){

  for(i in seq_along(mask)){
    cur_mask <- mask[[i]]
    cur_sce <- object[,colData(object)[,image_ID] == mcols(mask)[i,image_ID]]
    col_ind <- cur_col[colData(cur_sce)[,colour_by] ]

    # Colour first the background
    cur_mask[cur_mask == 0L] <- "#000000"

    # Then colour cells that are not in sce
    cur_m <- as.vector(cur_mask != "#000000") &
      !(cur_mask %in% as.character(colData(cur_sce)[,cell_ID]))
    cur_mask <- replace(cur_mask, which(cur_m), cur_col["missing_col"])

    # Next, colour cells that are present in sce object
    cur_m <- match(cur_mask, as.character(colData(cur_sce)[,cell_ID]))
    cur_ind <- which(!is.na(cur_m))
    col_ind <- col_ind[cur_m[!is.na(cur_m)]]

    cur_mask <- replace(cur_mask, cur_ind, col_ind)

    if(!is.null(names(mask))){
      ind <- names(mask)[i]
    } else{
      ind <- i
    }
    setImages(mask, ind) <- cur_mask
  }

  return(as(mask, "SimpleList"))

}

# Colour segmentation masks based on features
#' @importFrom grDevices colorRampPalette
.colourMaskByFeature <- function(object, mask, cell_ID, image_ID,
                     colour_by, exprs_values, cur_col){

  for(i in seq_along(mask)){
    cur_mask <- mask[[i]]
    cur_sce <- object[,colData(object)[,image_ID] == mcols(mask)[i,image_ID]]

    # Colour first the background
    cur_mask[cur_mask == 0L] <- "#000000"

    # Then colour cells that are not in sce
    cur_m <- as.vector(cur_mask != "#000000") &
      !(cur_mask %in% as.character(colData(cur_sce)[,cell_ID]))
    cur_mask <- replace(cur_mask, which(cur_m), cur_col[["missing_colour"]])

    # Next, colour cells that are present in sce object
    # For this, we will perform a min/max scaling on the provided counts
    # Based on this, we will first merge the colours and colour
    # the mask accordingly
    cur_m <- match(cur_mask, as.character(colData(cur_sce)[,cell_ID]))
    cur_ind <- which(!is.na(cur_m))
    cur_col_list <- lapply(colour_by, function(x){
      cur_frame <- cur_mask
      col_ind <- colorRampPalette(cur_col[[x]])(101)
      col_ind[round(100*.minMaxScaling(assay(cur_sce, exprs_values)[x,])) + 1]
    })

    col_ind <- apply(data.frame(cur_col_list), 1, .mixColours)
    col_ind <- col_ind[cur_m[!is.na(cur_m)]]

    cur_mask <- replace(cur_mask, cur_ind, col_ind)

    if(!is.null(names(mask))){
      ind <- names(mask)[i]
    } else{
      ind <- i
    }
    setImages(mask, ind) <- Image(cur_mask)
  }

  return(as(mask, "SimpleList"))
}

# Colour segmentation masks based on metadata
.outlineMaskByMeta <- function(object, mask, img, cell_ID, image_ID,
                               outline_by, cur_col){

  for(i in seq_along(mask)){
    cur_mask <- mask[[i]]
    cur_img <- img[[i]]
    cur_sce <- object[,colData(object)[,image_ID] == mcols(mask)[i,image_ID]]

    # Loop through entries in outline_by entry
    for(j in unique(colData(cur_sce)[,outline_by])){
      meta_mask <- cur_mask
      cur_cell_ID <- colData(cur_sce)[colData(cur_sce)[,outline_by] == j,cell_ID]
      meta_mask[!(meta_mask %in% cur_cell_ID)] <- 0L
      cur_img <- paintObjects(meta_mask, Image(cur_img), col = cur_col[j])
    }

    img[[i]] <- Image(cur_img)
  }

  return(img)
}

# Selecting the colours for plotting
#' @importFrom grDevices colorRampPalette
.selectColours <- function(object, colour_by, colour, missing_colour){

  # We seperate this function between colouring based on metadata
  # or the marker expression (rownames)
  if(all(colour_by %in% colnames(colData(object)))){
    # If colour is not specified, we select a number of default colours
    cur_entries <- unique(colData(object)[,colour_by])
    if(is.null(colour)){
      if(length(cur_entries) > 23){
        cur_col <- viridis(length(cur_entries))
        names(cur_col) <- cur_entries
        cur_col <- c(cur_col, missing_colour = missing_colour)
      } else {
        cur_col <- c(brewer.pal(12, "Paired"),
                     brewer.pal(8, "Pastel2")[-c(3,5,8)],
                     brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
        cur_col <- cur_col[1:length(cur_entries)]
        names(cur_col) <- cur_entries
        cur_col <- c(cur_col, missing_colour = missing_colour)
      }
    } else {
      cur_col <- colour[colour_by]
      cur_col <- c(cur_col, missing_colour = missing_colour)
    }
  } else {
    if(is.null(colour)){
      if(length(colour_by) > 1){
        col_list <- list(colorRampPalette(c("black", "red"))(100),
                         colorRampPalette(c("black", "green"))(100),
                         colorRampPalette(c("black", "blue"))(100),
                         colorRampPalette(c("black", "cyan"))(100),
                         colorRampPalette(c("black", "magenta"))(100),
                         colorRampPalette(c("black", "yellow"))(100))
        col_list <- col_list[1:length(colour_by)]
        names(col_list) <- colour_by
        col_list <- c(col_list, missing_colour = missing_colour)
        cur_col <- col_list
      } else {
        cur_col <- list(viridis(100))
        names(cur_col) <- colour_by
        cur_col <- c(cur_col, missing_colour = missing_colour)
      }
    } else {
      cur_col <- colour[colour_by]
      cur_col <- c(cur_col, missing_colour = missing_colour)
    }
  }

  return(cur_col)
}

# Min/max scaling of expression counts
.minMaxScaling <- function(x){
    return((x - min(x))/(max(x) - min(x)))
}

# Function to mix colours similar to how EBImage is creating an RGB
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
.mixColours <- function(col_vector){
  args <- as.list(col_vector)
  cols <- lapply(args, function(x){col2rgb(x)/255})
  cur_mix <- Reduce("+", cols)
  cur_mix[cur_mix > 1] <- 1
  cur_mix <- cur_mix
  cur_mix <- rgb(t(cur_mix), maxColorValue = 1)
  return(cur_mix)
}

# Adds scale bar to images
.addScaleBar <- function(scale_bar){
  length <- scale_bar$length
  label <- scale_bar$label
  position <- scale_bar$position
  lwd <- scale_bar$lwd
  col <- scale_bar$col

  # Get coordinates
}

# Custom function to display images
.displayImages <- function(object, outline_by, colour_by, img,
                           scale_bar, cur_col){
  # Number of images
  # The first space is used for the figure legend
  ni <- length(img) + 1

  # Size of images
  si <- lapply(img, function(x)dim(x)[1:2])

  # Ncols and nrow
  nc <- ceiling(sqrt(ni))
  nr <- ceiling(ni/nc)

  # We will take the largest image and
  # build the grid based on its size
  cur_dims <- data.frame(lapply(img, dim))
  m_height <- max(cur_dims[1,])
  m_width <- max(cur_dims[2,])
  # Add empty image to list
  img <- c(SimpleList(Image("#FFFFFF",
                 dim = c(m_height, m_width))),
           img)
  cur_dims_x <- c(m_width, as.numeric(cur_dims[2,]))
  cur_dims_y <- c(m_height, as.numeric(cur_dims[1,]))

  # Build the grid
  x_len <- c(0, nc * m_width)
  y_len <- c(0, nr * m_height)
  par(bty="n", mai=c(0,0,0,0), xaxs="i",
      yaxs="i", xaxt="n", yaxt="n", col = "white")
  plot(x_len, y_len, type="n", xlab="", ylab="",
       asp = 1, ylim = rev(y_len))

  # Plot the images
  for(i in seq_len(nr)){
    for(j in seq_len(nc)){
      ind <- (i-1)*nc +j
      dim_x <- cur_dims_x[ind]
      dim_y <- cur_dims_y[ind]
      xleft <- (j-1)*m_width + (m_width - dim_x)/2
      ybottom <- i*m_height - (m_height - dim_y)/2
      xright <- j*m_width - (m_width - dim_x)/2
      ytop <- (i-1)*m_height + (m_height - dim_y)/2
      rasterImage(img[[ind]],
                  xleft,
                  ybottom,
                  xright,
                  ytop)

      if(ind == 1L){
        # Plot legend
        #.plotLegend(object, outline_by, colour_by,
         #           m_width, m_height, cur_col)
      }

      if(ind != 1L && !is.null(scale_bar)){
        # Plot scale bar
        #.plotScaleBar(scale_bar,
        #              cur_dims_x[ind], cur_dims_y[ind],
        #              m_width, m_height,)
      }
    }
  }
}


# Plot legend
.plotLegend(object, outline_by, colour_by,
            m_width, m_height, cur_col){
  # Build one legend per feature or metadata entry
  nlegend <- length(outline_by) + length(colour_by)
  margin <- 10

  # Plot feature legends first
  if(!is.null(colour_by) && all(colour_by %in% rownames(object))){
      for(i in seq_along(colour_by)){
        if(i < 4){
          cur_x <- ((m_width-(2*margin))/6 * i) + margin
          cur_y <- (m_height-(2*margin))/4 + margin
        } else {
          cur_x <- ((m_width-(2*margin))/6 * (i - 3)) + margin
          cur_y <- ((m_height-(2*margin))/4 * 3) + margin
        }
        cur_space_x <- (m_width-(2*margin))/6
        cur_space_y <- (m_height-(2*margin))/2

        # Adjust text size based on size of image
        title_width <- strwidth(colour_by[i], font = 2)
        title_height <- abs(strheight(colour_by[i], font = 2))
        label_width <- max(strwidth(rev(seq(0, 1, length.out = 3))))

        cur_legend <- as.raster(matrix(rev(cur_col$colour_by[[colour_by[i]]]),
                                       ncol=1))
        text(x = cur_x, y = cur_y - cur_space_y/2 + title_height,
             label = colour_by[i], col = "black", font = 2)
        text(x=cur_x + cur_space_x/4,
             y = seq(cur_y - cur_space_y/2 + title_height*2,
                     cur_y + cur_space_y/2- title_height, length.out = 3),
             labels = rev(seq(0, 1, length.out = 3)), col = "black",
             adj = 0.5, cex = (cur_space_x/3)/label_width)
        rasterImage(cur_legend,
                    cur_x - cur_space_x/2,
                    cur_y + cur_space_y/2 - title_height,
                    cur_x,
                    cur_y - cur_space_y/2 + title_height*2)
      }
  }

  # Next metadata legends
  if(!is.null(colour_by) && all(colour_by %in% colnames(colData(object)))){
    cur_space_x <- (m_width-(2*margin))/6
    cur_x <- m_width/2 + cur_space_x
    cur_y <- margin
    cur_colouring <- cur_col$colour_by[1:(length(cur_col$colour_by) - 1)]
    legend_w <- legend(x = cur_x, y = cur_y, legend = names(cur_colouring),
                       fill = cur_colouring,
                       text.col = "black", plot = FALSE)
    legend(x = cur_x, y = cur_y, legend = names(cur_colouring),
           fill = cur_colouring,
           text.col = "black", cex = (m_width-margin-cur_x)/legend_w$rect$w)
  }

  if(!is.null(outline_by)){
    cur_space_x <- (m_width-(2*margin))/6
    cur_x <- m_width/2 + cur_space_x
    cur_y <- margin
    cur_colouring <- cur_col$colour_by[1:(length(cur_col$colour_by) - 1)]
    legend_w <- legend(x = cur_x, y = cur_y, legend = names(cur_colouring),
           fill = cur_colouring,
           text.col = "black", plot = FALSE)
    legend(x = cur_x, y = cur_y, legend = names(cur_colouring),
           fill = cur_colouring,
           text.col = "black", plot = FALSE, cex = (m_width-margin-cur_x)/legend_w$rect$w)
  }
}

# Plot scale_bar
.plotScaleBar(scale_bar, dim_x, dim_y,
            max_width, max_height){

}

