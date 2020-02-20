
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
