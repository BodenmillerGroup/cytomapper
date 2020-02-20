
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
