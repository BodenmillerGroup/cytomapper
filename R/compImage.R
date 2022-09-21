#' @title Performs channel compensation on multi-channel images
#'
#' @description Corrects the intensity spillover between neighbouring channels
#' of multi-channel images using a non-negative least squares approach.
#'
#' @param object a \code{CytoImageList} object containing pixel
#' intensities for all channels. The \code{channelNames} must be in the form
#' of \code{(mt)(mass)Di} (e.g. \code{Sm152Di} for Samarium isotope with the
#' atomic mass 152) and match with the column names in \code{sm}.
#' @param sm numeric matrix containing the spillover estimated between channels.
#' The column names must be of the form \code{(mt)(mass)Di} (e.g.
#' \code{Sm152Di} for Samarium isotope with the atomic mass 152) and match to
#' the \code{channelNames} of \code{object}.
#' @param overwrite (for images stored on disk) should the original image array
#' be overwritten by the compensated image array? By default (\code{overwrite
#' = FALSE}), a new entry called "XYZ_comp" will be written to the .h5  file
#' (see below).
#' @param BPPARAM parameters for parallelised processing. 
#'
#' @return returns the compensated pixel intensities in form of a 
#' \code{CytoImageList} object.
#' 
#' @section The input object:
#' The \code{channelNames} of \code{object} need to match the column names
#' of \code{sm}. To adapt the spillover matrix accordingly, please use the
#' \code{\link[CATALYST]{adaptSpillmat}} function.
#' 
#' @section Images stored on disk:
#' Image compensation also works for images stored on disk. By default,
#' the compensated images are stored as a second entry called "XYZ_comp"
#' in the .h5 file. Here "XYZ" specifies the name of the original entry.
#' By storing the compensated next to the original images on disk, space
#' usage increases. To avoid storing duplicated data, one can specify
#' \code{overwrite = TRUE}, therefore deleting the original images
#' and only storing the compensated images. However, the original images
#' cannot be accessed anymore after compensation.
#'
#' @examples
#' data("pancreasImages")
#' 
#' # Generate example spillover matrix
#' metals <- c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")
#' sm <- matrix(c(1, 0.033, 0.01, 0.007, 0,
#'                0.016, 1, 0.051, 0.01, 0,
#'                0.004, 0.013, 1, 0.023, 0,
#'                0.005, 0.008, 0.029, 1, 0.006,
#'                0, 0, 0, 0.001, 1), byrow = TRUE,
#'              ncol = 5, nrow = 5, 
#'              dimnames = list(metals, metals))
#'              
#' # Rename channels - just used as example
#' channelNames(pancreasImages) <- metals
#' 
#' # Perform channel spillover
#' comp_images <- compImage(pancreasImages, sm) 
#' 
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#' 
#' @seealso 
#' \code{\link[nnls]{nnls}}, for the underlying algorithm
#' 
#' \code{\link[CATALYST]{compCytof}}, for how to compensate single-cell data
#' 
#' @references
#' \href{https://www.sciencedirect.com/science/article/pii/S2405471217305434}{
#' Chevrier, S. et al., Compensation of Signal Spillover in Suspension 
#' and Imaging Mass Cytometry., Cell Systems 2018 6(5):612-620.e5}
#'
#' @export
#' @importFrom nnls nnls
compImage <- function(object, sm, overwrite = FALSE, BPPARAM = SerialParam()){
    
    if (attr(class(object), "package") == "cytomapper") {
        warning("Please update the CytoImageList object by calling 'updateObject(object)'")
        object <- CytoImageList::updateObject(object)
    }
    
    .valid.compImage.input(object, sm)
    
    cur_channels <- channelNames(object)
    cur_meta <- mcols(object)
    
    if (is(object[[1]], "Image")) {
        # in memory    
        object <- CytoImageList(bplapply(object, function(x){
            cur_out <- apply(as.array(x), c(1,2), 
                             function(u){nnls(t(sm), u)$x})
            cur_out <- aperm(cur_out, c(2,3,1))
            return(Image(cur_out))
        }, BPPARAM = BPPARAM))
        
    } else {
        # on disk
        object <- CytoImageList(bplapply(object, function(x){
            cur_out <- apply(as.array(x), c(1,2), 
                             function(u){nnls(t(sm), u)$x})
            cur_out <- aperm(cur_out, c(2,3,1))
            
            cur_out <- .add_h5(x, cur_out, overwrite, suffix = "_comp")
            
            return(cur_out)
        }, BPPARAM = BPPARAM), on_disk = TRUE) 
    }
    
    channelNames(object) <- cur_channels
    mcols(object) <- cur_meta
    
    return(object)
}
