#' @rdname loadImages
#' @title
#'
#' @description Returns an ImageList of EBImage objects
#'
#' @param image.path # vector or name of a column that contains the paths to the images
#' @param image.name # vector or name of a column that contains the names of the images
#' @param image.id # vector or name of a column that contains image ids
#' @param image.list # a dataset that contains 
#'
#' @return image.list
#'
#' @examples
#' # TODO
#'
#' @author Nils Eling \email{nils.eling@@dqbm.uzh.ch},
#' Nicolas Damond \email{nicolas.damond@@dqbm.uzh.ch}
#'
#' @import EBImage
#' @import S4Vectors
#' @importFrom SingleCellExperiment colData
#' @export


### Example of inputs
## loadImages("ImagePath", "ImageName", "ImageId", '/home/nicolasd/Git/SingleCellMapper/inst/extdata/image_list.csv')
##
## sce <- readRDS('/home/nicolasd/Git/SingleCellMapper/inst/extdata/sce.rds')
## loadImages(colData(sce)$ImagePath, colData(sce)$ImageName, colData(sce)$ImageNb)
## loadImages("ImagePath", "ImageName", "ImageNb", sce)


loadImages <- function(image.path,
                       image.name,
                       image.id,
                       image.list = NULL
                       ) {

  
  # If an external csv file is provided, import it
  if(!is.null(image.list) & is.character(image.list)){
    image.list <- importCSV(image.list)
  }

  # Create a SimpleList containing image paths, names and ids
  image.list <- createImageList(path=image.path, name=image.name, id=image.id, image.list=image.list)
  
  # Read the images with EBImage
  images <- readImages(image.list)
  
  return(images)
}

#' readImages
#'
#' read the images and store them in an ImageList (currently, a SimpleList, should be adapted)
#'
#' @param image.list
#' @keywords image read
#' @return images
#' @import 
#' @importFrom EBImage readImage
#' @export

readImages <- function (image.list) {
  fn <- file.path(image.list$imagePath, image.list$imageName)

  SimpleList(images)
  images <- lapply(fn, EBImage::readImage)
}



#' createImageList
#'
#' Store the image paths and names in a DataFrame
#'
#' @param path
#' @param name
#' @param id
#' @param image.list
#' @keywords image list path
#' @return image.list
#' @import 
#' @importFrom S4Vectors DataFrame
#' @export

createImageList <- function (path, name, id, image.list=NULL) {

  if(is(image.list, "SingleCellExperiment")){
    image.list <- colData(sce)
  }
  
  if(is.null(image.list)){
    image.list <- DataFrame(imagePath = path,
                            imageName = name,
                            imageID = id)
  } else if (!is.null(image.list)){
    image.list <- DataFrame(imagePath = image.list[, (path)],
                            imageName = image.list[, (name)],
                            imageID = image.list[, (id)])
  }
  return(unique(image.list))
}


#' importCSV
#'
#' If an external CSV file is provided, import it
#'
#' @param image.list
#' @keywords image list csv import
#' @return image.list
#' @import 
#' @importFrom tools file_ext
#' @export

importCSV <- function (image.list) {

  if (file.exists(image.list) & tools::file_ext(image.list) == "csv")
    image.list <- read.csv(image.list, header = T, stringsAsFactors = F)

  return(image.list)
}




