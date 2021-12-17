#' @title Peforms channel compensation on multi-channel images
#'
#' @description 
#'
#' @param object a \code{CytoImageList} object containing pixel
#' intensities for all channels. 
#' @param sm numeric matrix containing the spillover estimated between channels.
#'
#' @return returns the compensated pixel intensities in form of a 
#' \code{CytoImageList} object
#'
#' @examples
#' # TODO
#' 
#' @seealso 
#' 
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch})
#'
#' @export
compImage <- function(object, sm, overwrite = FALSE, BPPARAM = SerialParam()){
    
    # .valid.compImage.input(object, sm)
    
    cur_channels <- channelNames(object)
    
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
            
    }
    
    channelNames(object) <- cur_channels
    
    return(object)
}
