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
      version=utils::packageVersion("SingleCellMapper")),
    elementType="Image"
    )
  )

# Validity checks
#' @importFrom S4Vectors setValidity2
S4Vectors:::setValidity2(Class="ImageList", .ImageList_validity)

.ImageList_validity <- function(object) {

              msg <- NULL

              # Check if all images have the same number of channels
              dims <- unlist(lapply(object, function(x){
                dim(x)[3]
              }))
              if(length(unique(dims)) > 1L){
                msg <- c(msg, "The images contain different number of channels.\n")
              }

              # Check if all channels have the same names
              if(length(dim(object[[1]])) == 3L){
                cur_names <- dimnames(object[[1]])[[3]]
                errors <- unlist(lapply(object, function(x){
                  !identical(cur_names, dimnames(x)[[3]])
                }))
                if(sum(errors) > 0){
                  msg <- c(msg, "Not all channels have the same names.")
                }
              }

              # Check if entry names are unique
              if(!is.null(names(object)) &&
                 length(unique(names(object))) < length(names(object))){
                msg <- c(msg, "Only unique entries allowed in an ImageList object.")
              }

              # Check if names contain NA or empties
              if(!is.null(names(object)) && (sum(is.na(names(object))) > 0 ||
                                             sum(names(object) %in% "") > 0)){
                msg <- c(msg, "Empty or NA names not supported.")
              }

              # Check if colourmode of each Image is "Grayscale"
              colour.modes <- unlist(lapply(object, colorMode))
              if("Color" %in% colour.modes){
                msg <- c(msg, "Only Grayscale images are supported for ImageList objects.")
              }

              if (length(msg)) { return(msg) }
              return(TRUE)
            }

# Coercion from list
setAs("list", "ImageList", function(from) {
  # Use constructor function
  ImageList(from)
})

# Coercion from ANY
setAs("ANY", "ImageList", function(from) {
  # Use constructor function
  ImageList(from)
})

# Expanded show method
setMethod("show", signature = signature(x="ImageList"),
          definition = function(object){
            lo <- length(object)
            cat(class(pancreasImages)[1], " containing ", lo,
                " images\n", sep = "")
            if (!is.null(names(object)))
              cat("names(", lo, "):", names(object), "\n", sep = "")
            if(length(dim(object[[1]])) > 2){
              cat("Each image contains ", dim(object[[1]])[3],
                  " channel(s)\n", sep = "")
            } else {
              cat("Each image contains 1 channel\n", sep = "")
            }
            if(!is.null(channelNames(object))){
              cat("channelNames(", length(channelNames(object)),
                  "):", channelNames(object), "\n", sep = "")
            }
          })

#setMethod("plot")
# Calls plotCells or plotCounts by default

# Expand bracket functions to check if valid object is returned
setReplaceMethod("[",
                 signature = signature(x="ImageList"),
                 definition = function(x, i, j, ..., value){
                   .Object <- callNextMethod()
                   .Object <- as(.Object, "ImageList")
                   methods::validObject(.Object)
                   return(.Object)
                 })

setReplaceMethod("[[",
                 signature = signature(x="ImageList"),
                 definition = function(x, i, j, ..., value){
                 .Object <- callNextMethod()
                 .Object <- as(.Object, "ImageList")
                 methods::validObject(.Object)
                 return(.Object)
                 })

setReplaceMethod("names",
                 signature = signature(x="ImageList"),
                 definition = function(x, value){
                 .Object <- callNextMethod()
                 .Object <- as(.Object, "ImageList")
                 methods::validObject(.Object)
                 return(.Object)
                 })





