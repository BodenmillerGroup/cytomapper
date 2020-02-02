#' @export
#' @rdname ImageList
#' @importFrom utils packageVersion
#' @importFrom S4Vectors SimpleList
#' @importClassesFrom EBImage Image
#' @importClassesFrom S4Vectors SimpleList
setClass(
  Class="ImageList",
  package="SingleCellMapper",
  representation(int_metadata="list"),
  contains = "SimpleList",
  prototype = prototype(
    int_metadata=list(
      version=utils::packageVersion("SingleCellMapper")),
    elementType="Image"
    )
  )

# Validity checks
setValidity(Class="ImageList",
            method=function(object) {

              # Check if all entries are Image class objects
              errors <- unlist(lapply(object, function(x){
                is(x, "Image")
              }))
              if(sum(!errors) > 0){
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

              return(TRUE)
            }
)



