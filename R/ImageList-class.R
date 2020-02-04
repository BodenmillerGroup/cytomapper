#' @export
#' @rdname ImageList
#' @importFrom utils packageVersion
#' @importFrom S4Vectors setValidity2
#' @importClassesFrom EBImage Image
#' @importClassesFrom S4Vectors SimpleList
setClass(
  Class="ImageList",
  package="SingleCellMapper",
  representation(int_metadata="list"),
  contains = "SimpleList",
  prototype = prototype(
    int_metadata=list(
      version=utils::packageVersion("SingleCellMapper"),
      channelNames=""),
    elementType="Image"
    ),
  )

# Validity checks
S4Vectors:::setValidity2(Class="ImageList",
            method=function(object) {

              # Check if all images have the same number of channels
              dims <- unlist(lapply(object, function(x){
                dim(x)[3]
              }))
              if(length(unique(dims)) > 1){
                stop("The images contain different number of channels.\n")
              }

              # Check if all channels have the same names
              cur_names <- dimnames(object[[1]])[[3]]
              errors <- unlist(lapply(object, function(x){
                !identical(cur_names, dimnames(x)[[3]])
              }))
              if(sum(errors) > 0){
                stop("Not all channels have the same names.\n",
                     "Please use the 'channelNames' function to alter channel names.")
              }

              # Check if colourmode of each Image is "Grayscale"
              colour.modes <- unlist(lapply(object, colorMode))
              if("Color" %in% colour.modes){
                stop("Only Grayscale images are supported for ImageList objects.")
              }
            }
)


#setMethod("show")

#setMethod("plot")





