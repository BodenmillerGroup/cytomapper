# -------------------------------------------------------------------------
# The Image_OR_HDF5Array class
# -------------------------------------------------------------------------

# We want to extend the CytoImageList object to also contain HDF5Array objects.
# For this, we'll merge the Image and HDF5Array class

#' @importFrom HDF5Array HDF5Array 
#' @importFrom EBImage Image 
setClassUnion("Image_OR_HDF5Array", c("Image", "HDF5Array"))