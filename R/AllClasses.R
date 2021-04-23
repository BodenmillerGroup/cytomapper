# -------------------------------------------------------------------------
# The Image_OR_DelayedArray class
# -------------------------------------------------------------------------

#' @importClassesFrom HDF5Array HDF5Array
#' @importClassesFrom DelayedArray DelayedArray
#' @importFrom EBImage Image 
setClassUnion("Image_OR_DelayedArray", c("Image", "DelayedArray", "HDF5Array"))
