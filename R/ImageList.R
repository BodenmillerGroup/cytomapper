# Constructor function
ImageList <- function(..., elementMetadata=NULL){
  new("ImageList", ..., elementMetadata=elementMetadata)
}
