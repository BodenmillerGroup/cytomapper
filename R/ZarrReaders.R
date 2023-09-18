#' @title Read data from ZARR files
#' @name readZARR
#'
#' @description
#' Reads in data from a ZARR file
#'
#' @param file TODO
#' @param type TODO
#' @param what TODO
#' @param resolution TODO
#' @param x TODO
#' @param y TODO
#' @param c TODO
#' @param z TODO
#' @param t TODO
#' @param fov_names TODO
#'
#' @return A \linkS4class{CytoImageList} object
#' 
#' @examples
#' # TODO
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch}),
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom Rarr read_zarr_array zarr_overview
readZARR <- function(file,
                     type = c("omengff", "spatialdata"),
                     what = c("images", "masks"),
                     resolution = NULL,
                     x = NULL,
                     y = NULL,
                     c = NULL,
                     z = 1,
                     t = 1,
                     fov_names = NULL) {
    
    # Check if file is spatialData or OME-NGFF
    #.valid.readZARR.input(file)
    
    type <- match.arg(type)
    what <- match.arg(what)
    
    if (what == "images") {
        if (type == "spatialdata") {
            if (is.null(fov_names)) {
                fov_names <- list.files(file.path(file, "images"))[1]
            }
            
            if (is.null(resolution)) {
                if (length(fov_names) > 1) {
                    resolution <- lapply(file.path(file, "images", fov_names), 
                                         function(cur_name) {
                                             cur_res <- list.files(cur_name)
                                             return(cur_res[length(cur_res)])
                                         })
                    resolution <- unlist(resolution)
                } else {
                    resolution <- list.files(file.path(file, "images", fov_names))
                    resolution <- resolution[length(resolution)]
                }
            }
            
            cur_names <- file.path(file, "images", fov_names, resolution)
            
            # Read in metadata
            cur_meta <- lapply(file.path(file, "images", fov_names),
                               function(cur_m) {
                                   return(fromJSON(file.path(cur_m, ".zattrs")))
                               })
            
            cur_channels <- lapply(cur_meta, function(cur_m){
                if ("omero" %in% names(cur_meta)) {
                    return(cur_meta$omero$channels$label)
                } else if ("channels_metadata" %in% names(cur_meta)) {
                    return(cur_meta$channels_metadata$channels$label)
                } 
            })
            
            if (length(unique(cur_channels)) > 1) {
                stop("Channel names need to match across images.")
            }
            
        } else if (type == "omengff") {
            if (is.null(resolution)) {
                resolution <- list.files(file, pattern = paste(seq(0, 10), collapse = "|"))
                resolution <- resolution[length(resolution)]
            }
            
            fov_names <- sub("\\.[^.]*$", "", basename(file))
            
            cur_names <- file.path(file, resolution)
            
            cur_meta <- fromJSON(file.path(file, ".zattrs"))
            
            if ("omero" %in% names(cur_meta)) {
                cur_channels <- cur_meta$omero$channels$label
            } else if ("channels_metadata" %in% names(cur_meta)) {
                cur_channels <- cur_meta$channels_metadata$channels$label
            } 
        }
        
        cur_out <- lapply(cur_names, function(cur_name){
            
            # Create index
            cur_zarr <- zarr_overview(cur_name, as_data_frame = TRUE)
            cur_dim <- cur_zarr$dim[[1]]
            
            if (length(cur_dim) == 3) {
                cur_ind <- list(c, y, x)
            } else if (length(cur_dim) == 4) {
                cur_ind <- list(z, c, y,  x)
            } else if (length(cur_dim) == 5) {
                cur_ind <- list(t, z, c, y, x)
            }
            
            cur_arr <- read_zarr_array(cur_name, index = cur_ind)
            cur_arr <- Image(aperm(cur_arr, rev(seq_along(dim(cur_arr)))))
            
            return(cur_arr)
        })
        
        names(cur_out) <- fov_names
        
        cur_CIL <- CytoImageList(cur_out)
        
        # Set channelNames
        if (exists("cur_channels")) {
            channelNames(cur_CIL) <- cur_channels
        } else {
            channelNames(cur_CIL) <- as.character(seq_len(dim(cur_CIL[[1]])[3]))
        }
        
        return(cur_CIL)
    } else if (what == "masks") {
        
    }

}