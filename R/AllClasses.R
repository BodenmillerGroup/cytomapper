# -------------------------------------------------------------------------
# The Image_OR_DelayedArray class
# -------------------------------------------------------------------------

# We want to extend the CytoImageList object to also contain DelayedArray objects.
# For this, we'll merge the Image, DelayedArray and DelayedMatrix class

#' @import HDF5Array  
#' @importFrom EBImage Image 
setClassUnion("Image_OR_DelayedArray", c("Image", "DelayedArray", "DelayedMatrix",
                                         "HDF5Array", "HDF5Matrix"))
