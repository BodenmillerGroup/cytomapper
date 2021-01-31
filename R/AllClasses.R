# -------------------------------------------------------------------------
# The Image_OR_DelayedArray class
# -------------------------------------------------------------------------

# We want to extend the CytoImageList object to also contain DelayedArray objects.
# For this, we'll merge the Image, DelayedArray and DelayedMatrix class

#' @importClassesFrom HDF5Array HDF5Array
#' @importClassesFrom DelayedArray DelayedArray
#' @importFrom EBImage Image 
setClassUnion("Image_OR_DelayedArray", c("Image", "DelayedArray", "HDF5Array"))
