#' @title Read data from ZARR files
#' @name readZARR
#'
#' @description
#' Reads in data from a ZARR file
#'
#' @param file
#' @param 
#'
#' @return A \linkS4class{CytoImageList} object
#' 
#' @examples
#' # TODO
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch}),
#'
#' @export
readZARR <- function(file,
                     type = c("omengff", "spatialdata"),
                     what = c("images", "masks"),
                     resolution = NULL,
                     z = 1,
                     t = 1,
                     fov_name = NULL) {
    
    # Check if file is spatialData or OME-NGFF
    #.valid.readZARR.input(file)
    
    type <- match.arg(type)
    what <- match.arg(what)
    
    if (type == "spatialdata") {
        if (is.null(fov_name)) {
            fov_name <- list.files(file.path(file, "images"))[1]
        }
    
        cur_out <- lapply(fov_name, function(x){
            
            if (is.null(resolution)) {
                cur_res <- list.files(file.path(file, "images", x))
                cur_res <- cur_res[length(cur_res)]
            }
        
            cur_name <- file.path(file, "images", x, cur_res)
            
            # Create index
            cur_zarr <- zarr_overview(cur_name, as_data_frame = TRUE)
            cur_dim <- cur_zarr$dim[[1]]
            
            if (length(cur_dim) == 3) {
                cur_ind <- list(NULL, NULL, NULL)
            } else if (length(cur_dim) == 4) {
                cur_ind <- list(NULL, NULL, NULL, z)
            } else if (length(cur_dim) == 5) {
                cur_ind <- list(NULL, NULL, NULL, z, t)
            }
            
            cur_arr <- read_zarr_array(cur_name, index = cur_ind)
            cur_arr <- Image(aperm(cur_arr, rev(seq_along(dim(cur_arr)))))
            
            return(cur_arr)
        })
    
    }
    
}