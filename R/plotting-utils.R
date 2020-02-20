# -----------------------------------------------------------------------------
# Helper functions for plotting cells and pixels
# -----------------------------------------------------------------------------

# Selection of images based on entries in SCE object or by subset
.select_images <- function(object, images, image_ID, subset_images){

  # If subset_images is not given, images are selected based on the cells
  # in the SCE object
  if(is.null(subset_images)){
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

  return(mask)

}

# Colour segmentation masks based on features
.colourMaskByFeature(object, mask, cell_ID, image_ID,
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

    cur_image_list <- lapply()
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

  return(mask)
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
