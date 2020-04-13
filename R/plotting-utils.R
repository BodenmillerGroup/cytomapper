# -----------------------------------------------------------------------------
# Helper functions for plotting cells and pixels
# -----------------------------------------------------------------------------

# Colour segmentation masks based on metadata
#' @importFrom S4Vectors mcols
.colourMaskByMeta <- function(object, mask, cell_id, img_id,
                                colour_by, cur_colour, missing_colour,
                                background_colour){
    
    for(i in seq_along(mask)){
        cur_mask <- mask[[i]]
        cur_sce <- object[,colData(object)[,img_id] == mcols(mask)[i,img_id]]
        if (is.null(names(cur_colour))) {
            col_ind <- colorRampPalette(cur_colour)(101)
            cur_scaling <- .minMaxScaling(colData(cur_sce)[,colour_by],
                                    min_x = min(colData(object)[,colour_by]),
                                    max_x = max(colData(object)[,colour_by]))
            col_ind <- col_ind[round(100*cur_scaling) + 1]
        } else {
            col_ind <- cur_colour[as.character(colData(cur_sce)[,colour_by])]
        }

        # Colour first the background
        cur_mask[cur_mask == 0L] <- background_colour

        # Then colour cells that are not in sce
        cur_m <- as.vector(cur_mask != background_colour) &
            !(cur_mask %in% as.character(colData(cur_sce)[,cell_id]))
        if (sum(cur_m) > 0) {
            cur_mask <- replace(cur_mask, which(cur_m), missing_colour)
        }

        # Next, colour cells that are present in sce object
        cur_m <- match(cur_mask, as.character(colData(cur_sce)[,cell_id]))
        cur_ind <- which(!is.na(cur_m))
        col_ind <- col_ind[cur_m[cur_ind]]

        cur_mask <- replace(cur_mask, cur_ind, col_ind)

        if (!is.null(names(mask))) {
            ind <- names(mask)[i]
        } else {
            ind <- i
        }
        setImages(mask, ind) <- cur_mask
    }

    return(as(mask, "SimpleList"))

}

# Function to mix colours
#' @importFrom grDevices col2rgb rgb
.mixColours <- function(col_vector){
    args <- as.list(col_vector)
    cols <- lapply(args, function(x){col2rgb(x)/255})
    cur_mix <- Reduce("+", cols)
    return(cur_mix)
}

# Function to create a composite colour vector
.createColourVector <- function(object, colour_by, 
                                exprs_values, cur_colour,
                                plottingParam){
    
    if (plottingParam$scale) {
        if (length(colour_by) == 1L) {
            cur_range <- quantile(assay(object, exprs_values)[colour_by,],
                                    probs = c(0,1))
            cur_range <- matrix(cur_range, ncol = 1, 
                                dimnames = list(c("1", "2"), colour_by))
        } else {
            cur_range <- apply(assay(object, exprs_values)[colour_by,], 1, 
                                quantile, probs = c(0,1))
        }
    } else {
        cur_range <- quantile(assay(object, exprs_values)[colour_by,],
                                probs = c(0,1))
    }
    
    cur_col_df <- vapply(colour_by, function(x){
        col_ind <- colorRampPalette(cur_colour[[x]])(101)
        
        if (plottingParam$scale) {
            cur_scaling <- .minMaxScaling(assay(object, exprs_values)[x,],
                                            min_x = cur_range[1,x],
                                            max_x = cur_range[2,x])
        } else {
            cur_scaling <- .minMaxScaling(assay(object, exprs_values)[x,],
                                            min_x = as.numeric(cur_range[1]),
                                            max_x = as.numeric(cur_range[2]))
        }
        
        return(col_ind[round(100*cur_scaling) + 1])
    }, FUN.VALUE = character(ncol(object)))
    
    # Hex colours to rgb
    col_ind <- apply(cur_col_df, 1, .mixColours)
    
    # Rescale to account for additive colour mixing
    col_ind <- col_ind / max(col_ind)
    
    # Convert to hex colour
    col_out <- apply(col_ind, 2, function(x){rgb(red = x[1], green = x[2], 
                                                    blue = x[3],
                                                    maxColorValue = 1)})

    # Store in internal colData
    int_colData(object)$CYTO_COLOUR <- col_out

    return(object)
}

# Colour segmentation masks based on features
#' @importFrom grDevices colorRampPalette
#' @importFrom SummarizedExperiment assay
#' @importFrom S4Vectors mcols
.colourMaskByFeature <- function(object, mask, cell_id, img_id,
                        colour_by, exprs_values, cur_colour,
                        missing_colour, background_colour, plottingParam){
    
    object <- .createColourVector(object, colour_by, 
                                    exprs_values, cur_colour,
                                    plottingParam)

    for(i in seq_along(mask)){
        cur_mask <- mask[[i]]
        cur_sce <- object[,colData(object)[,img_id] == mcols(mask)[i,img_id]]

        # Colour first the background
        cur_mask[cur_mask == 0L] <- background_colour

        # Then colour cells that are not in sce
        cur_m <- as.vector(cur_mask != background_colour) &
            !(cur_mask %in% as.character(colData(cur_sce)[,cell_id]))
        if (sum(cur_m) > 0) {
            cur_mask <- replace(cur_mask, which(cur_m), missing_colour)
        }

        # Next, colour cells that are present in sce object
        cur_m <- match(cur_mask, as.character(colData(cur_sce)[,cell_id]))
        cur_ind <- which(!is.na(cur_m))

        col_ind <- int_colData(cur_sce)$CYTO_COLOUR
        col_ind <- col_ind[cur_m[cur_ind]]

        cur_mask <- replace(cur_mask, cur_ind, col_ind)

        if(!is.null(names(mask))){
            ind <- names(mask)[i]
        } else{
            ind <- i
        }
        setImages(mask, ind) <- cur_mask
    }

    return(as(mask, "SimpleList"))
}

# Colour images based on features
#' @importFrom EBImage normalize
.colourImageByFeature <- function(image, colour_by, bcg,
                                    cur_colour, plottingParam){

    if (length(colour_by) > 1) {
        max.values <- vapply(getChannels(image, colour_by), function(x){
            apply(x, 3, max)
        }, FUN.VALUE = numeric(length(colour_by)))
        max.values <- apply(max.values, 1, max)
        
        min.values <- vapply(getChannels(image, colour_by), function(x){
            apply(x, 3, min)
        }, FUN.VALUE = numeric(length(colour_by)))
        min.values <- apply(min.values, 1, min)
    } else {
        max.values <- vapply(getChannels(image, colour_by), function(x){
            apply(x, 3, max)
        }, FUN.VALUE = numeric(1))
        max.values <- max(max.values)
        names(max.values) <- colour_by
        
        min.values <- vapply(getChannels(image, colour_by), function(x){
            apply(x, 3, min)
        }, FUN.VALUE = numeric(1))
        min.values <- min(min.values)
        names(min.values) <- colour_by
    }
    
    image <- as(image, "SimpleList")

    for(i in seq_along(image)){
        cur_image <- image[[i]][,,colour_by, drop = FALSE]

        # Colour pixels
        # For this, we will perform a min/max scaling on the pixel values per
        # channel. However, to keep pixel values comparable across images,
        # we will fix the scale across all images to the min/max of all
        # images per channel. Based on this, we will first merge the
        # colours and colour the images accordingly, We also allow the
        # user to change the scale thresholds using the 'bcg' object.
        # This will allow the user to change the brightness (b),
        # contrast (c) and gamma (g)
        cur_frame_list <- lapply(colour_by, function(x){
            if (x %in% names(bcg)) {
                cur_bcg <- bcg[[x]]
            } else {
                cur_bcg <- c(0, 1, 1)
            }

            # Select min and max values
            if (plottingParam$scale) {
                cur_min <- as.numeric(min.values[x])
                cur_max <- as.numeric(max.values[x])
            } else {
                cur_min <- min(min.values)
                cur_max <- max(max.values)
            }

            cur_frame <- cur_image[,,x]
            cur_frame <- ((cur_frame + cur_bcg[1]) * cur_bcg[2]) ^ cur_bcg[3]
            cur_frame <- normalize(cur_frame, separate=TRUE,
                                ft = c(0,1), 
                                inputRange = c(cur_min, cur_max))
            col_ind <- colorRampPalette(cur_colour[[x]])(101)
            cur_frame <- replace(cur_frame, seq_len(length(cur_frame)),
                            col_ind[round(100*cur_frame) + 1])
            return(Image(cur_frame))
        })
        
        cur_image <- Reduce("+", cur_frame_list)

        image[[i]] <- cur_image
    }
    
    # Rescale images to account for additive colour merging
    if (length(colour_by) > 1) {
        cur_range <- vapply(X = image, FUN = quantile, 
                            FUN.VALUE = numeric(2), probs = c(0, 1))
        cur_min <- min(cur_range[1,])
        cur_max <- max(cur_range[2,])
        
        if (cur_max > 1) {
            image <- endoapply(image, normalize, separate = FALSE, 
                                ft = c(0,1), inputRange = c(cur_min, cur_max))
        }
    }

    return(image)
}

# Outline image based on metadata
#' @importFrom EBImage paintObjects
#' @importFrom S4Vectors mcols
.outlineImageByMeta <- function(object, mask, out_img, cell_id, img_id,
                                outline_by, cur_colour){

    for(i in seq_along(mask)){
        cur_mask <- mask[[i]]
        cur_img <- out_img[[i]]
        cur_sce <- object[,colData(object)[,img_id] == mcols(mask)[i,img_id]]

        if (is.null(names(cur_colour))) {
            col_ind <- colorRampPalette(cur_colour)(101)
            cur_scaling <- .minMaxScaling(colData(cur_sce)[,outline_by],
                                    min_x = min(colData(object)[,outline_by]),
                                    max_x = max(colData(object)[,outline_by]))

            for(j in seq_along(cur_scaling)){
                meta_mask <- cur_mask
                cur_cell_id <- colData(cur_sce)[j,cell_id]
                meta_mask[meta_mask != cur_cell_id] <- 0L
                cur_img <- paintObjects(meta_mask, Image(cur_img),
                                col = col_ind[round(100*cur_scaling[j]) + 1])
            }
        } else {
            cur_vec <- as.character(colData(cur_sce)[,outline_by])
            for(j in unique(cur_vec)){
                meta_mask <- cur_mask
                ind <- cur_vec == j
                cur_cell_id <- colData(cur_sce)[ind, cell_id]
                meta_mask[!(meta_mask %in% cur_cell_id)] <- 0L
                cur_img <- paintObjects(meta_mask, Image(cur_img),
                                        col = cur_colour[j])
            }
        }

        out_img[[i]] <- Image(cur_img)
    }

    return(out_img)
}

# Selecting the colours for plotting
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis inferno
.selectColours <- function(object, colour_by, colour,
                        call.arg = c("colour_by", "outline_by")){
    
    call.arg <- match.arg(call.arg)

    if (!is.null(object) && all(colour_by %in% colnames(colData(object)))) {

        cur_entries <- unique(colData(object)[,colour_by])
        if (is.null(colour[[colour_by]])) {
            if (length(cur_entries) > 23) {
                if (is.numeric(cur_entries)) {
                    if (call.arg == "colour_by") {
                        cur_col <- viridis(100)
                    } else {
                        cur_col <- inferno(100)
                    }
                } else {
                    if (call.arg == "colour_by") {
                        cur_col <- viridis(length(cur_entries))
                    } else {
                        cur_col <- inferno(length(cur_entries))
                    }
                    names(cur_col) <- as.character(cur_entries)
                }
            } else {
                if (call.arg == "colour_by") {
                    cur_col <- c(brewer.pal(12, "Paired"),
                        brewer.pal(8, "Pastel2")[-c(3,5,8)],
                        brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
                } else {
                    cur_col <- rev(c(brewer.pal(12, "Paired"),
                        brewer.pal(8, "Pastel2")[-c(3,5,8)],
                        brewer.pal(12, "Set3")[-c(2,3,7,8,9,11,12)],
                        "brown3"))
                }
                cur_col <- cur_col[seq_len(length(cur_entries))]
                names(cur_col) <- as.character(cur_entries)
            }
            col_out <- list(cur_col)
            names(col_out) <- colour_by
        } else {
            col_out <- colour[colour_by]
        }
    } else {
        if (!all(colour_by %in% names(colour))) {
            if (length(colour_by) > 1) {
                col_list <- list(colorRampPalette(c("black", "red"))(100),
                            colorRampPalette(c("black", "green"))(100),
                            colorRampPalette(c("black", "blue"))(100),
                            colorRampPalette(c("black", "cyan"))(100),
                            colorRampPalette(c("black", "magenta"))(100),
                            colorRampPalette(c("black", "yellow"))(100))
                col_list <- col_list[seq_len(length(colour_by))]
                names(col_list) <- colour_by
                col_out <- col_list
            } else {
                col_out <- list(viridis(100))
                names(col_out) <- colour_by
            }
        } else {
            col_out <- colour[colour_by]
        }
    }

    return(col_out)
}

# Min/max scaling of expression counts
.minMaxScaling <- function(x, min_x, max_x){
    return( (x - min_x) / (max_x - min_x) )
}

# Custom function to display images
#' @importFrom S4Vectors SimpleList mcols
#' @importFrom EBImage Image
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom graphics par rasterImage strheight text
#' @importFrom grDevices png jpeg tiff dev.off recordPlot
.displayImages <- function(object, image, exprs_values, outline_by,
                            colour_by, mask, out_img,
                            img_id, cur_col, plottingParam){

    # We will take the largest image and
    # build the grid based on its size
    cur_dims <- vapply(out_img, dim, 
                        FUN.VALUE = numeric(length(dim(out_img[[1]]))))
    m_width <- max(cur_dims[1L,])
    m_height <- max(cur_dims[2L,])

    cur_dims_x <- c(m_width, as.numeric(cur_dims[1L,]))
    cur_dims_y <- c(m_height, as.numeric(cur_dims[2L,]))

    # Add empty image to list for legend
    if (!is.null(plottingParam$legend)) {
        out_img <- c(SimpleList(Image("#FFFFFF",
                                dim = c(m_height, m_width))),
                    out_img)
        legend_ind <- 1L
    } else {
        legend_ind <- 0L
    }

    # Number of images
    # The first space is used for the figure legend
    ni <- length(out_img)
    
    # Ncols and nrow
    nc <- ceiling(sqrt(ni))
    nr <- ceiling(ni/nc)

    # Define margin
    margin <- plottingParam$margin

    # Build the grid
    x_len <- c(0, (nc * m_width) + (nc - 1) * margin)
    y_len <- c(0, (nr * m_height) + (nr - 1) * margin)

    # Initialize list for storing plots
    if (plottingParam$return_plot && plottingParam$display == "single") {
        cur_out <- list()
    }

    cur_par <- par(bty="n", mai=c(0,0,0,0), xaxs="i",
                    yaxs="i", xaxt="n", yaxt="n", col = "white")
    on.exit(par(cur_par))

    if (!is.null(plottingParam$save_plot) && plottingParam$display == "all") {
        image_location <- plottingParam$save_plot$filename
        image_scale <- plottingParam$save_plot$scale
        cur_ext <- file_ext(image_location)
        if (cur_ext == "png") {
            png(filename = image_location, 
                width = image_scale * nc * m_width,
                height = image_scale * nr * m_height, 
                units = "px",
                pointsize = 12 * image_scale)
        } else if (cur_ext == "jpeg") {
            jpeg(filename = image_location, 
                width = image_scale * nc * m_width,
                height = image_scale * nr * m_height, 
                units = "px",
                pointsize = 12 * image_scale)
        } else if (cur_ext == "tiff") {
            tiff(filename = image_location, 
                width = image_scale * nc * m_width,
                height = image_scale * nr * m_height, 
                units = "px",
                pointsize = 12 * image_scale)
        }
    } else {
        image_scale <- 1
    }

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    if(plottingParam$display == "all"){
        plot(x_len, y_len, type="n", xlab="", ylab="",
            asp = 1, ylim = rev(y_len))
    }

    # Plot the images
    for(i in seq_len(nr)){
        for(j in seq_len(nc)){
            ind <- ((i - 1) * nc) + j

            if (ind > ni) {break}

            dim_x <- cur_dims_x[ind]
            dim_y <- cur_dims_y[ind]
            
            xleft <- (j - 1) * m_width + 
                (m_width - dim_x) / 2 + (j - 1) * margin
            ybottom <- i * m_height - 
                (m_height - dim_y) / 2 + (i - 1) * margin
            xright <- j * m_width - 
                (m_width - dim_x) / 2 + (j - 1) * margin
            ytop <- (i - 1) * m_height + 
                (m_height - dim_y) / 2 + (i - 1) * margin

            # If Images should be saved
            if (!is.null(plottingParam$save_plot) &&
                plottingParam$display == "single") {
                image_location <- plottingParam$save_plot$filename
                image_scale <- plottingParam$save_plot$scale
                cur_ext <- file_ext(image_location)

                if (ind == legend_ind) {
                    cur_name <- paste0(file_path_sans_ext(image_location),
                                        "_legend.", cur_ext)
                } else {
                    cur_name <- paste0(file_path_sans_ext(image_location),
                                        "_", ind - legend_ind, ".", cur_ext)
                }

                if (cur_ext == "png") {
                    png(filename = cur_name, 
                        width = image_scale * dim_x,
                        height = image_scale * dim_y, units = "px",
                        pointsize = 12 * image_scale)
                } else if (cur_ext == "jpeg") {
                    jpeg(filename = cur_name, 
                        width = image_scale * dim_x,
                        height = image_scale * dim_y, units = "px",
                        pointsize = 12 * image_scale)
                } else if (cur_ext == "tiff") {
                    tiff(filename = cur_name, 
                        width = image_scale * dim_x,
                        height = image_scale * dim_y, units = "px",
                        pointsize = 12 * image_scale)
                }

                par(bty="n", mai=c(0,0,0,0), xaxs="i",
                    yaxs="i", xaxt="n", yaxt="n", col = "white")
            }

            if (plottingParam$display == "all") {
                rasterImage(Image(out_img[[ind]]),
                            xleft = xleft, ybottom = ybottom,
                            xright = xright, ytop = ytop,
                            interpolate = plottingParam$interpolate)
            } else {
                plot(c(0, dim_x), c(0, dim_y), type="n", xlab="", ylab="",
                    asp = 1, ylim = rev(c(0, dim_y)))
                rasterImage(Image(out_img[[ind]]),
                            xleft = 0, ybottom = dim_y,
                            xright = dim_x, ytop = 0,
                            interpolate = plottingParam$interpolate)
            }
            
            # Plot legend
            if (ind == legend_ind && !is.null(plottingParam$legend)) {
                .plotLegend(object = object, image = image, 
                            exprs_values = exprs_values, 
                            outline_by = outline_by, colour_by = colour_by, 
                            m_width = m_width, m_height = m_height, 
                            cur_col = cur_col, plottingParam = plottingParam)
            }

            # Plot scale bar
            if (ind != legend_ind && !is.null(plottingParam$scale_bar)) {
                if (plottingParam$scale_bar$frame == "all") {
                    if (plottingParam$display == "all") {
                        .plotScaleBar(plottingParam$scale_bar,
                                xl = xleft, xr = xright,
                                yt = ytop, yb = ybottom,
                                m_w = m_width, m_h = m_height)
                    } else {
                        .plotScaleBar(plottingParam$scale_bar,
                                xl = 0, xr = dim_x,
                                yt = 0, yb = dim_y,
                                m_w = m_width, m_h = m_height)
                    }
                } else {
                    frame_ind <- as.integer(plottingParam$scale_bar$frame)
                    cur_ind <- legend_ind + frame_ind
                    if (ind == cur_ind && !is.null(plottingParam$scale_bar)) {
                        if (plottingParam$display == "all") {
                            .plotScaleBar(plottingParam$scale_bar,
                                    xl = xleft, xr = xright,
                                    yt = ytop, yb = ybottom,
                                    m_w = m_width, m_h = m_height)
                        } else {
                            .plotScaleBar(plottingParam$scale_bar,
                                    xl = 0, xr = dim_x,
                                    yt = 0, yb = dim_y,
                                    m_w = m_width, m_h = m_height)
                        }
                    }
                }
            }

            # Plot title on images
            if (ind != legend_ind && !is.null(plottingParam$image_title)) {
                if (plottingParam$display == "all") {
                    .plotImageTitle(out_img = out_img, mask = mask, 
                                image = image, img_id = img_id,
                                ind = ind, legend_ind = legend_ind, 
                                image_title = plottingParam$image_title,
                                dim_x = dim_x, xl = xleft, xr = xright,
                                yt = ytop, yb = ybottom, m_h = m_height)
                } else {
                    .plotImageTitle(out_img = out_img, mask = mask, 
                                image = image, img_id = img_id,
                                ind = ind, legend_ind = legend_ind, 
                                image_title = plottingParam$image_title,
                                dim_x = dim_x, xl = 0, xr = dim_x,
                                yt = 0, yb = dim_y, m_h = m_height)
                }
            }

            # Close device
            if (!is.null(plottingParam$save_plot) &&
                plottingParam$display == "single") {
                dev.off()
            }

            if (plottingParam$return_plot && 
                plottingParam$display == "single") {
                cur_plot <- recordPlot()

                if (ind == legend_ind && !is.null(plottingParam$legend)) {
                    cur_out[["legend"]] <- cur_plot
                    next
                }

                # Set the title correctly
                image_title <- plottingParam$image_title
                if (!is.null(image_title$text)) {
                    cur_title <- image_title$text[ind - legend_ind]
                } else if (!is.null(mask) && !is.null(img_id)) {
                    cur_title <- mcols(mask)[ind - legend_ind,img_id]
                } else if (!is.null(image) && !is.null(img_id)) {
                    cur_title <- mcols(image)[ind - legend_ind,img_id]
                } else if (!is.null(names(out_img))) {
                    cur_title <- names(out_img)[ind]
                } else {
                    cur_title <- as.character(ind - legend_ind)
                }

                cur_out[[as.character(cur_title)]] <- cur_plot
            }
        }
    }

    if (plottingParam$return_plot && plottingParam$display == "all") {
        cur_out <- recordPlot()
    }

    if (!is.null(plottingParam$save_plot) && plottingParam$display == "all") {
        dev.off()
    }

    if (plottingParam$return_plot) {
        return(cur_out)
    } else {
        return(NULL)
    }
}

# Plot legend
#' @importFrom graphics strwidth strheight text rasterImage legend
#' @importFrom raster as.raster
.plotLegend <- function(object, image, exprs_values, outline_by, colour_by,
                        m_width, m_height, cur_col, plottingParam){
    # Build one legend per feature or metadata entry
    margin <- plottingParam$legend$margin
    colour_by.title.font <- plottingParam$legend$colour_by.title.font
    colour_by.title.cex <- plottingParam$legend$colour_by.title.cex
    colour_by.labels.cex <- plottingParam$legend$colour_by.labels.cex
    colour_by.legend.cex <- plottingParam$legend$colour_by.legend.cex
    outline_by.title.font <- plottingParam$legend$outline_by.title.font
    outline_by.title.cex <- plottingParam$legend$outline_by.title.cex
    outline_by.labels.cex <- plottingParam$legend$outline_by.labels.cex
    outline_by.legend.cex <- plottingParam$legend$outline_by.legend.cex

    # Plot feature legends first
    if (!is.null(colour_by) &&
        (all(colour_by %in% rownames(object)) || !is.null(image))) {

        # Maximum title height
        title_height <- max(abs(strheight(colour_by, 
                                            font = colour_by.title.font)))

        # Maximum label width
        if (is.null(image)) {
            all_max <- max(assay(object, exprs_values)[colour_by,])
        } else {
            all_max <- max(unlist(lapply(getChannels(image, colour_by), max)))
        }
        label_width <- strwidth(format(round(all_max, 1), nsmall = 2))

        # If scale = FALSE, define maximum and minimum
        if(!plottingParam$scale){
            if(is.null(image)){
                cur_min <- min(assay(object, exprs_values)[colour_by,])
                cur_max <- max(assay(object, exprs_values)[colour_by,])
            } else {
                cur_min <- unlist(lapply(getChannels(image, colour_by), min))
                cur_max <- unlist(lapply(getChannels(image, colour_by), max))
                cur_min <- min(cur_min)
                cur_max <- max(cur_max)
            }
        }

        for(i in seq_along(colour_by)){
            col_n <- colour_by[i]
            
            if (i < 4) {
                cur_x <- (((m_width - (2 * margin)) / 6) * (i - 1)) + margin
                cur_y <- margin
            } else {
                cur_x <- ((m_width - (2 * margin)) / 6 * (i - 4)) + margin
                cur_y <- m_height / 2 
            }
            cur_space_x <- (m_width - (2 * margin)) / 6
            cur_space_y <- (m_height - (2 * margin)) / 2

            if (plottingParam$scale) {
                if (is.null(image)) {
                    cur_min <- min(assay(object, exprs_values)[col_n,])
                    cur_max <- max(assay(object, exprs_values)[col_n,])
                } else {
                    cur_min <- min(unlist(lapply(getChannels(image,
                                                            col_n), min)))
                    cur_max <- max(unlist(lapply(getChannels(image,
                                                            col_n), max)))
                }
            }

            cur_labels <- c(format(round(cur_min, 1), nsmall = 1),
                            format(round(cur_max, 1)/2),
                            format(round(cur_max, 1), nsmall = 1))
            
            # Define title cex
            if (is.null(colour_by.title.cex)) {
                title_cex <- (cur_space_y / 10) / title_height
            } else {
                title_cex <- colour_by.title.cex
            }

            # Define label cex
            if (is.null(colour_by.labels.cex)) {
                label_cex <- (cur_space_x / 2) / label_width
            } else {
                label_cex <- colour_by.labels.cex
            }

            col_ramp <- colorRampPalette(cur_col$colour_by[[col_n]])(101)
            cur_legend <- as.raster(matrix(rev(col_ramp), ncol=1))
            
            text(x = cur_x + cur_space_x/2, 
                y = cur_y,
                label = col_n, col = "black",
                font = colour_by.title.font,
                cex = title_cex, adj = c(0.5, 1))
            
            text(x = cur_x + cur_space_x / 2,
                y = seq(cur_y + cur_space_y/4,
                cur_y + cur_space_y - cur_space_y/8, length.out = 3),
                labels = rev(cur_labels), col = "black",
                adj = c(0, 0.5), cex = label_cex)
            
            rasterImage(cur_legend, 
                    xleft = cur_x,
                    ybottom =  cur_y + cur_space_y - cur_space_y / 8, 
                    xright = cur_x + cur_space_x / 3, 
                    ytop = cur_y + cur_space_y / 4)
        }
    }

    # Next metadata legends
    if (!is.null(object) &&
        !is.null(colour_by) &&
        all(colour_by %in% colnames(colData(object)))) {

        if (is.null(names(cur_col$colour_by[[1]]))) {
            cur_space_x <- (m_width - (2 * margin)) / 4
            cur_space_y <- (m_height - (2 * margin)) / 2
            cur_x <- m_width / 2 + cur_space_x
            cur_y <- margin
            cur_min <- min(colData(object)[,colour_by])
            cur_max <- max(colData(object)[,colour_by])
            cur_labels <- c(format(round(cur_min, 1), nsmall = 1),
                            format(round(cur_max, 1)/2),
                            format(round(cur_max, 1), nsmall = 1))
            label_width <- max(strwidth(rev(cur_labels)))
            title_height <- abs(strheight(colour_by, 
                                        font = colour_by.title.font))

            col_ramp <- colorRampPalette(cur_col$colour_by[[1]])(101)
            cur_legend <- as.raster(matrix(rev(col_ramp), ncol=1))

            # Define title cex
            if (is.null(colour_by.title.cex)) {
                title_cex <- (cur_space_y / 10) / title_height
            } else {
                title_cex <- colour_by.title.cex
            }

            text(x = cur_x + cur_space_x / 2, y = cur_y,
                label = colour_by, col = "black",
                font = colour_by.title.font,
                cex = title_cex, adj = c(0.5, 1))

            # Define label cex
            if (is.null(colour_by.labels.cex)) {
                label_cex <- (cur_space_x / 2) / label_width
            } else {
                label_cex <- colour_by.labels.cex
            }

            text(x= cur_x + cur_space_x / 2,
                y = seq(cur_y + cur_space_y / 4,
                        cur_y + cur_space_y - cur_space_y/8,
                        length.out = 3),
                labels = rev(cur_labels), col = "black",
                adj = 0, cex = label_cex)
            
            rasterImage(cur_legend,
                        xleft = cur_x,
                        ybottom = cur_y + cur_space_y - cur_space_y / 8,
                        xright = cur_x + cur_space_x / 3,
                        ytop = cur_y + cur_space_y / 4)
            
            cur_legend_height <- cur_space_y - cur_space_y/8
            
        } else {
            cur_space_x <- (m_width - (2 * margin)) / 6
            cur_x <- m_width / 2 + cur_space_x
            cur_y <- margin
            cur_colouring <- cur_col$colour_by[[1]]
            legend_c <- legend(x = cur_x, y = cur_y,
                                legend = names(cur_colouring),
                                fill = cur_colouring, title = colour_by,
                                text.col = "black", plot = FALSE)

            # Define legend cex
            if (is.null(colour_by.legend.cex)) {
                legend_cex <- (m_width - margin - cur_x) / legend_c$rect$w
            } else {
                legend_cex <- colour_by.legend.cex
            }

            legend_c <- legend(x = cur_x, y = cur_y,
                                legend = names(cur_colouring),
                                fill = cur_colouring, title = colour_by,
                                text.col = "black", cex = legend_cex)
            cur_legend_height <- abs(legend_c$rect$h)
        }
    }

    # Outline
    if (!is.null(outline_by)) {
        if (!is.null(colour_by) &&
            all(colour_by %in% colnames(colData(object)))) {
            cur_y <- margin + abs(cur_legend_height) + m_width / 20
        } else {
            cur_y <- margin
        }

        # Continous scale
        if (is.null(names(cur_col$outline_by[[1]]))) {
            cur_space_x <- (m_width - (2 * margin)) / 4
            cur_space_y <- (m_height - (2 * margin)) / 2
            cur_x <- m_width / 2 + cur_space_x
            cur_min <- min(colData(object)[,outline_by])
            cur_max <- max(colData(object)[,outline_by])
            cur_labels <- c(format(round(cur_min, 1), nsmall = 1),
                            format(round(cur_max, 1)/2),
                            format(round(cur_max, 1), nsmall = 1))
            label_width <- max(strwidth(rev(cur_labels)))
            title_height <- abs(strheight(outline_by, 
                                        font = colour_by.title.font))

            col_ramp <- colorRampPalette(cur_col$outline_by[[1]])(101)
            cur_legend <- as.raster(matrix(rev(col_ramp), ncol=1))

            # Define title cex
            if (is.null(colour_by.title.cex)) {
                title_cex <- (cur_space_y / 10) / title_height
            } else {
                title_cex <- outline_by.title.cex
            }

            text(x = cur_x + cur_space_x/2, y = cur_y,
                    label = outline_by, col = "black",
                    font = outline_by.title.font,
                    cex = title_cex, adj = c(0.5, 1))

            # Define label cex
            if (is.null(outline_by.labels.cex)) {
                label_cex <- (cur_space_x / 2) / label_width
            } else {
                label_cex <- outline_by.labels.cex
            }

            text(x=cur_x + cur_space_x / 2,
                y = seq(cur_y + cur_space_y / 4,
                        cur_y + cur_space_y - cur_space_y / 8, 
                        length.out = 3),
                labels = rev(cur_labels), col = "black",
                adj = 0, cex = label_cex)
            
            rasterImage(cur_legend,
                        cur_x,
                        cur_y + cur_space_y - cur_space_y/8,
                        cur_x + cur_space_x / 3,
                        cur_y + cur_space_y / 4)
        } else {
            cur_space_x <- (m_width - (2 * margin)) / 6
            cur_x <- m_width / 2 + cur_space_x
            cur_colouring <- cur_col$outline_by[[1]]
            legend_o <- legend(x = cur_x, y = cur_y,
                            legend = names(cur_colouring),
                            fill = cur_colouring, title = outline_by,
                            text.col = "black", plot = FALSE)

            # Define legend cex
            if (is.null(outline_by.legend.cex)) {
                legend_cex <- (m_width - margin - cur_x) / legend_o$rect$w
            } else {
                legend_cex <- outline_by.legend.cex
            }

            legend(x = cur_x, y = cur_y, legend = names(cur_colouring),
                    fill = cur_colouring, title = outline_by,
                    text.col = "black", cex = legend_cex)
        }
    }
}

# Plot scale_bar
#' @importFrom graphics strheight text rect
#' @importFrom raster as.raster
.plotScaleBar <- function(scale_bar, xl, xr, yt, yb, m_w, m_h){
    # Set default scale bar length
    if (is.null(scale_bar$length)) {
        cur_length <- ifelse(m_w > 25, round(m_w / 5, digits = -1), 10)
    } else {
        cur_length <- scale_bar$length
    }
    
    if (is.null(scale_bar$label)) {
        cur_label <- as.character(cur_length)
    } else {
        cur_label <- scale_bar$label
    }
    
    if (is.null(scale_bar$cex)) {
        label_height <- abs(strheight(cur_label))
        # Target size is 5% of max image height
        cur_cex <- (m_h / 20) / label_height
    } else {
        cur_cex <- scale_bar$cex
    }
    
    if (is.null(scale_bar$lwidth)) {
        # Target size is 2% of max image height
        cur_lwidth <- ifelse(m_h >= 50, round(m_h / 50, digits = 0), 1)
    } else {
        cur_lwidth <- scale_bar$lwidth 
    }
    
    cur_col <- scale_bar$colour
    cur_position <- scale_bar$position
    cur_margin.x <- scale_bar$margin[1]
    cur_margin.y <- scale_bar$margin[2]

    # Plot scale bar
    label_height <- abs(strheight(cur_label, cex = cur_cex))
    rect_params <- list(col = cur_col)
    text_params <- list(labels = cur_label, cex = cur_cex,
                        col = cur_col, adj = c(0.5, 0))
    label_dist <- m_h / 40

    if (cur_position == "bottomright") {
        rect(xleft = xr - cur_length - cur_margin.x,
            xright = xr - cur_margin.x,
            ybottom = yb - cur_margin.y,
            ytop = yb - cur_margin.y - cur_lwidth,
            col = cur_col, border = NA)
        do.call(text, append(list(x = xr - cur_length / 2 - cur_margin.x,
                        y = yb - cur_margin.y - cur_lwidth - label_dist),
                            text_params))
    } else if (cur_position == "bottomleft") {
        rect(xleft = xl + cur_margin.x,
            xright = xl + cur_length + cur_margin.x,
            ybottom = yb - cur_margin.y,
            ytop = yb - cur_margin.y - cur_lwidth,
            col = cur_col, border = NA)
        do.call(text, append(list(x = xl + cur_length / 2 + cur_margin.x,
                        y = yb - cur_margin.y - cur_lwidth - label_dist),
                        text_params))
    } else if (cur_position == "topright") {
        rect(xleft = xr - cur_length - cur_margin.x,
            xright = xr - cur_margin.x,
            ybottom = yt + cur_margin.y + cur_lwidth,
            ytop = yt + cur_margin.y,
            col = cur_col, border = NA)
        do.call(text, append(list(x = xr - cur_length / 2 - cur_margin.x,
                        y = yt + cur_margin.y - label_dist),
                        text_params))
    } else if (cur_position == "topleft") {
        rect(xleft = xl + cur_margin.x,
                xright = xl + cur_length + cur_margin.x,
                ybottom = yt + cur_margin.y + cur_lwidth,
                ytop = yt + cur_margin.y,
                col = cur_col, border = NA)
        do.call(text, append(list(x = xl + cur_length / 2 + cur_margin.x,
                        y = yt + cur_margin.y - label_dist),
                        text_params))
    }
}


# Plot Title
#' @importFrom graphics strwidth strheight text rasterImage legend
#' @importFrom raster as.raster
#' @importFrom S4Vectors mcols
.plotImageTitle <- function(out_img, mask, image, img_id, ind, legend_ind,
                            image_title, dim_x,
                            xl, xr, yt, yb, m_h){

    if (!is.null(image_title$text)) { 
        cur_title <- image_title$text[ind - legend_ind]
    } else if (!is.null(mask) && !is.null(img_id)) {
        cur_title <- mcols(mask)[ind - legend_ind, img_id]
    } else if (!is.null(image) && !is.null(img_id)) {
        cur_title <- mcols(image)[ind - legend_ind, img_id]
    } else if (!is.null(names(out_img))) {
        cur_title <- names(out_img)[ind]
    } else {
        cur_title <- as.character(ind - legend_ind)
    }
    
    cur_font <- image_title$font
    
    if (is.null(image_title$cex)) {
        title_height <- abs(strheight(cur_title, font = cur_font))
        # Target size is 5% of max image height
        cur_cex <- (m_h / 20) / title_height
    } else {
        cur_cex <- image_title$cex
    }

    cur_position <- image_title$position
    cur_col <- image_title$colour
    cur_margin.x <- image_title$margin[1]
    cur_margin.y <- image_title$margin[2]

    text_params <- list(labels = cur_title, col = cur_col,
                        cex = cur_cex, font = cur_font)

    if(cur_position == "top"){
        do.call(text, append(list(x = xl + dim_x/2,
                                y = yt + cur_margin.y,
                                adj = c(0.5, 1)), text_params))
    } else if (cur_position == "bottom") {
        do.call(text, append(list(x = xl + dim_x/2,
                                y = yb - cur_margin.y,
                                adj = c(0.5, 0)), text_params))
    } else if (cur_position == "topleft") {
        do.call(text, append(list(x = xl + cur_margin.x,
                                y = yt + cur_margin.y,
                                adj = c(0, 1)), text_params))
    } else if (cur_position == "topright") {
        do.call(text, append(list(x = xr - cur_margin.x,
                                y = yt + cur_margin.y,
                                adj = c(1, 1)), text_params))
    } else if (cur_position == "bottomleft") {
        do.call(text, append(list(x = xl + cur_margin.x,
                                y = yb - cur_margin.y,
                                adj = c(0, 0)), text_params))
    } else if (cur_position == "bottomright") {
        do.call(text, append(list(x = xr - cur_margin.x,
                                y = yb - cur_margin.y,
                                adj = c(1, 0)), text_params))
    }
}

