#' @title Read data from ZARR files
#' @name readImagesFromZARR
#'
#' @description
#' Reads in data from a ZARR file
#'
#' @param file TODO
#' @param type TODO
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
readImagesFromZARR <- function(file,
                     type = c("omengff", "spatialdata"),
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
    
    if (type == "spatialdata") {
        
        cur_meta <- fromJSON(file.path(file, "zmetadata"))
        cur_img_meta <- str_split(names(cur_meta$metadata), "/", simplify = TRUE)
        cur_img_meta <- cur_img_meta[cur_img_meta[,1] == "images" & !grepl("^\\.", cur_img_meta[,2]),]
        cur_fov_names <- unique(cur_img_meta[,2])
        
        if (is.null(fov_names)) {
            fov_names <- cur_fov_names[1]
        }
        
        if (is.null(resolution)) {
            if (length(fov_names) > 1) {
                resolution <- lapply(file.path(file, "images", fov_names), 
                                     function(cur_name) {
                                         cur_res <- fromJSON(file.path(cur_name, ".zattrs"))
                                         cur_res <- cur_res$multiscales$datasets[[1]]$path
                                         return(sort(cur_res)[length(cur_res)])
                                     })
                resolution <- unlist(resolution)
            } else {
                resolution <- fromJSON(file.path(file, "images", fov_names, ".zattrs"))
                resolution <- cur_res$multiscales$datasets[[1]]$path
                resolution <- sort(resolution)[length(resolution)]
            }
        }
        
        cur_names <- file.path(file, "images", fov_names, resolution)
        
        # Read in metadata
        cur_meta <- lapply(file.path(file, "images", fov_names),
                           function(cur_m) {
                               return(fromJSON(file.path(cur_m, ".zattrs")))
                           })
        
        cur_channels <- lapply(cur_meta, function(cur_m){
            if ("omero" %in% names(cur_m)) {
                return(cur_m$omero$channels$label)
            } else if ("channels_metadata" %in% names(cur_m)) {
                return(cur_m$channels_metadata$channels$label)
            } 
        })
        
        if (length(unique(cur_channels)) > 1) {
            stop("Channel names need to match across images.")
        }
        
        cur_channels <- unlist(unique(cur_channels))
        
        cur_index <- lapply(cur_meta, function(cur_m){
            if ("coordinateTransformations" %in% names(cur_m$multiscales)) {
                return(cur_m$multiscales$coordinateTransformations[[1]]$input$axes[[1]]$name)
            } else {
                return(cur_m$multiscales$axes[[1]]$name)
            } 
        })
        
        if (length(unique(cur_index)) > 1) {
            stop("Dimensions need to match across images.")
        }
        
        cur_index <- unlist(unique(cur_index))
        
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
        
        if ("coordinateTransformations" %in% names(cur_meta$multiscales)) {
            return(cur_meta$multiscales$coordinateTransformations[[1]]$input$axes[[1]]$name)
        } else {
            return(cur_meta$multiscales$axes[[1]]$name)
        } 
    }
    
    message("Resolution of the image: ", resolution)
    message("Channels of the image: ", cur_channels)
    
    cur_out <- lapply(cur_names, function(cur_name){
        
        cur_ind <- list(t, c, z, y, x)
        names(cur_ind) <-  c("t", "c", "z", "y", "x")
        cur_ind <- cur_ind[cur_index]
        
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
}