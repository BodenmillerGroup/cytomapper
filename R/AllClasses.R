#' @export
#' @rdname ImageList
#' @importFrom utils packageVersion
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
setValidity(Class="ImageList",
            method=function(object) {

              # Check if all entries are Image class objects
              errors <- unlist(lapply(object, function(x){
                !is(x, "Image")
              }))
              if(sum(errors) > 0){
                stop("Not all entries are Image objects.\n",
                    "The ImageList object requires a list of Image objects.")
              }

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
                stop("Not all entries are Image objects.\n",
                     "The ImageList object requires a list of Image objects.")
              }

              return(TRUE)
            }
)



