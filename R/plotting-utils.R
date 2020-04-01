# -----------------------------------------------------------------------------
# Helper functions for plotting cells and pixels
# -----------------------------------------------------------------------------

# Selection of images based on entries in SCE object or by subset
#' @importFrom S4Vectors mcols
.select_images <- function(object, images, img_id, subset_images){

    if(!is.null(subset_images)){
        if(!is.null(img_id) &&
            all(subset_images %in% mcols(images)[,img_id])){
            images <- images[mcols(images)[,img_id] %in% subset_images]
        } else {
            images <- images[subset_images]
        }
    }

    return(images)
}

# Colour segmentation masks based on metadata
.colourMaskByMeta <- function(object, mask, cell_id, img_id,
                                colour_by, cur_colour, missing_colour,
                                background_colour){
    for(i in seq_along(mask)){
        cur_mask <- mask[[i]]
        cur_sce <- object[,colData(object)[,img_id] == mcols(mask)[i,img_id]]
        if(is.null(names(cur_colour))){
            col_ind <- colorRampPalette(cur_colour)(101)
            cur_scaling <- .minMaxScaling(colData(cur_sce)[,colour_by],
                                    min_x = min(colData(object)[,colour_by]),
                                    max_x = max(colData(object)[,colour_by]))
            col_ind <- col_ind[round(100*cur_scaling) + 1]
        } else {
        col_ind <- cur_colour[colData(cur_sce)[,colour_by]]
        }

        # Colour first the background
        cur_mask[cur_mask == 0L] <- background_colour

        # Then colour cells that are not in sce
        cur_m <- as.vector(cur_mask != background_colour) &
            !(cur_mask %in% as.character(colData(cur_sce)[,cell_id]))
            cur_mask <- replace(cur_mask, which(cur_m), missing_colour)

        # Next, colour cells that are present in sce object
        cur_m <- match(cur_mask, as.character(colData(cur_sce)[,cell_id]))
        cur_ind <- which(!is.na(cur_m))
        col_ind <- col_ind[cur_m[!is.na(cur_m)]]

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

# Colour segmentation masks based on features
#' @importFrom grDevices colorRampPalette
#' @importFrom SummarizedExperiment assay
.colourMaskByFeature <- function(object, mask, cell_id, img_id,
                        colour_by, exprs_values, cur_colour,
                        missing_colour, background_colour, plottingParam){

    for(i in seq_along(mask)){
        cur_mask <- mask[[i]]
        cur_sce <- object[,colData(object)[,img_id] == mcols(mask)[i,img_id]]

        # Colour first the background
        cur_mask[cur_mask == 0L] <- background_colour

        # Then colour cells that are not in sce
        cur_m <- as.vector(cur_mask != background_colour) &
            !(cur_mask %in% as.character(colData(cur_sce)[,cell_id]))
        cur_mask <- replace(cur_mask, which(cur_m), missing_colour)

        # Next, colour cells that are present in sce object
        # For this, we will perform a min/max scaling on the provided counts
        # However, to keep counts comparable across images, we will fix
        # the scale across all images to the min/max of the whole sce object
        # Based on this, we will first merge the colours and colour
        # the mask accordingly
        cur_m <- match(cur_mask, as.character(colData(cur_sce)[,cell_id]))
        cur_ind <- which(!is.na(cur_m))
        cur_col_list <- lapply(colour_by, function(x){
            cur_frame <- cur_mask
            col_ind <- colorRampPalette(cur_colour[[x]])(101)
            if(plottingParam$scale){
                cur_min <- min(assay(object, exprs_values)[x,])
                cur_max <- max(assay(object, exprs_values)[x,])
                cur_scaling <- .minMaxScaling(assay(cur_sce, exprs_values)[x,],
                                min_x = cur_min,
                                max_x = cur_max)
            } else {
                cur_min <- min(assay(object, exprs_values)[colour_by,])
                cur_max <- max(assay(object, exprs_values)[colour_by,])
                cur_scaling <- .minMaxScaling(assay(cur_sce, exprs_values)[x,],
                                min_x = cur_min,
                                max_x = cur_max)
            }
            col_ind[round(100*cur_scaling) + 1]
        })

        col_ind <- apply(data.frame(cur_col_list), 1, .mixColours)
        col_ind <- col_ind[cur_m[!is.na(cur_m)]]

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

    max.values <- as.data.frame(lapply(getChannels(image, colour_by),
                                        function(x){
        apply(x, 3, max)
    }))
    max.values <- apply(max.values, 1, max)

    min.values <- as.data.frame(lapply(getChannels(image, colour_by),
                                        function(x){
        apply(x, 3, min)
    }))
    min.values <- apply(min.values, 1, min)
    out.list <- as(image, "SimpleList")

    for(i in seq_along(image)){
        cur_image <- getChannels(image, colour_by)[[i]]

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
            if(x %in% names(bcg)){
                cur_bcg <- bcg[[x]]
            } else {
                cur_bcg <- c(0, 1, 1)
            }

            # Select min and max values
            if(plottingParam$scale){
                cur_min <- min.values[x]
                cur_max <- max.values[x]
            } else {
                cur_min <- min(min.values)
                cur_max <- max(max.values)
            }

            cur_frame <- cur_image[,,x]
            cur_frame <- ((cur_frame + cur_bcg[1]) * cur_bcg[2]) ^ cur_bcg[3]
            cur_frame <- normalize(cur_frame, separate=TRUE,
                                ft = c(0,1), inputRange = c(cur_min, cur_max))
            col_ind <- colorRampPalette(cur_colour[[x]])(101)
            cur_frame <- replace(cur_frame, seq_len(length(cur_frame)),
                            col_ind[round(100*cur_frame) + 1])
            cur_frame
        })

        cur_image <- Reduce("+", lapply(cur_frame_list, Image))

        if(!is.null(names(image))){
            ind <- names(image)[i]
        } else{
            ind <- i
        }
        out.list[[ind]] <- cur_image
    }

    return(out.list)
}

# Outline image based on metadata
#' @importFrom EBImage paintObjects
.outlineImageByMeta <- function(object, mask, out_img, cell_id, img_id,
                                outline_by, cur_colour, missing_colour){

    for(i in seq_along(mask)){
        cur_mask <- mask[[i]]
        cur_img <- out_img[[i]]
        cur_sce <- object[,colData(object)[,img_id] == mcols(mask)[i,img_id]]

        if(is.null(names(cur_colour))){
            col_ind <- colorRampPalette(cur_colour)(101)
            cur_scaling <- .minMaxScaling(colData(cur_sce)[,outline_by],
                                    min_x = min(colData(object)[,outline_by]),
                                    max_x = max(colData(object)[,outline_by]))

            for(j in seq_along(cur_scaling)){
                meta_mask <- cur_mask
                cur_cell_id <- colData(cur_sce)[j,cell_id]
                meta_mask[!(meta_mask %in% cur_cell_id)] <- 0L
                cur_img <- paintObjects(meta_mask, Image(cur_img),
                                col = col_ind[round(100*cur_scaling[j]) + 1])
            }
        } else {
            for(j in unique(colData(cur_sce)[,outline_by])){
                meta_mask <- cur_mask
                ind <- colData(cur_sce)[,outline_by] == j
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

    # We seperate this function between colouring based on metadata
    # or the marker expression (rownames)
    if(!is.null(object) && all(colour_by %in% colnames(colData(object)))){
        # If colour is not specified, we select a number of default colours
        cur_entries <- unique(colData(object)[,colour_by])
        if(is.null(colour[[colour_by]])){
            if(length(cur_entries) > 23){
                if(is.numeric(cur_entries)){
                    if(call.arg == "colour_by"){
                        cur_col <- viridis(100)
                    } else {
                        cur_col <- inferno(100)
                    }
                } else {
                    if(call.arg == "colour_by"){
                        cur_col <- viridis(length(cur_entries))
                    } else {
                        cur_col <- inferno(length(cur_entries))
                    }
                    names(cur_col) <- cur_entries
                }
            } else {
                if(call.arg == "colour_by"){
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
                names(cur_col) <- cur_entries
            }
            col_out <- list(cur_col)
            names(col_out) <- colour_by
        } else {
            col_out <- colour[colour_by]
        }
    } else {
        if(!all(colour_by %in% names(colour))){
            if(length(colour_by) > 1){
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
    return((x - min_x)/(max_x - min_x))
}

# Function to mix colours similar to how EBImage is creating an RGB
#' @importFrom grDevices col2rgb rgb
.mixColours <- function(col_vector){
    args <- as.list(col_vector)
    cols <- lapply(args, function(x){col2rgb(x)/255})
    cur_mix <- Reduce("+", cols)
    cur_mix[cur_mix > 1] <- 1
    cur_mix <- cur_mix
    cur_mix <- rgb(t(cur_mix), maxColorValue = 1)
    return(cur_mix)
}

# Custom function to display images
#' @importFrom S4Vectors SimpleList
#' @importFrom EBImage Image
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom graphics par rasterImage strheight text
#' @importFrom grDevices png jpeg tiff dev.off recordPlot
.displayImages <- function(object, image, exprs_values, outline_by,
                            colour_by, mask, out_img,
                            img_id, cur_col, plottingParam){

    # We will take the largest image and
    # build the grid based on its size
    cur_dims <- data.frame(lapply(out_img, dim))
    m_width <- max(cur_dims[1L,])
    m_height <- max(cur_dims[2L,])

    cur_dims_x <- c(m_width, as.numeric(cur_dims[1L,]))
    cur_dims_y <- c(m_height, as.numeric(cur_dims[2L,]))

    # Add empty image to list for legend
    if(!is.null(plottingParam$legend)){
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

    # Size of images
    si <- lapply(out_img, function(x){dim(x)[c(1,2)]})

    # Ncols and nrow
    nc <- ceiling(sqrt(ni))
    nr <- ceiling(ni/nc)

    # Define margin
    margin <- plottingParam$margin

    # Build the grid
    x_len <- c(0, (nc * m_width) + (nc - 1) * margin)
    y_len <- c(0, (nr * m_height) + (nr - 1) * margin)

    # Initialize list for storing plots
    if(plottingParam$return_plot && plottingParam$display == "single"){
        cur_out <- list()
    }

    cur_par <- par(bty="n", mai=c(0,0,0,0), xaxs="i",
                    yaxs="i", xaxt="n", yaxt="n", col = "white")
    on.exit(par(cur_par))

    if(!is.null(plottingParam$save_plot) && plottingParam$display == "all"){
        image_location <- plottingParam$save_plot$filename
        image_scale <- plottingParam$save_plot$scale
        cur_ext <- file_ext(image_location)
        if(cur_ext == "png"){
            png(image_location, width = image_scale * nc * m_width,
                height = image_scale * nr * m_height, units = "px",
                pointsize = 12 * image_scale)
        } else if(cur_ext == "jpeg"){
            jpeg(image_location, width = image_scale * nc * m_width,
                height = image_scale * nr * m_height, units = "px",
                pointsize = 12 * image_scale)
        } else if(cur_ext == "tiff"){
            tiff(image_location, width = image_scale * nc * m_width,
                height = image_scale * nr * m_height, units = "px",
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
            ind <- (i-1)*nc + j

            if(ind > ni){break}

            dim_x <- cur_dims_x[ind]
            dim_y <- cur_dims_y[ind]
            xleft <- (j-1)*m_width + (m_width - dim_x)/2 + (j-1) * margin
            ybottom <- i*m_height - (m_height - dim_y)/2 + (i-1) * margin
            xright <- j*m_width - (m_width - dim_x)/2 + (j-1) * margin
            ytop <- (i-1)*m_height + (m_height - dim_y)/2 + (i-1) * margin

            # If Images should be saved
            if(!is.null(plottingParam$save_plot) &&
                plottingParam$display == "single"){
                image_location <- plottingParam$save_plot$filename
                image_scale <- plottingParam$save_plot$scale
                cur_ext <- file_ext(image_location)

                # File name
                cur_name <- paste0(file_path_sans_ext(image_location),
                                    "_", ind, ".", cur_ext)
                if(cur_ext == "png"){
                    png(cur_name, width = image_scale * dim_x,
                        height = image_scale * dim_y, units = "px",
                        pointsize = 12 * image_scale)
                } else if(cur_ext == "jpeg"){
                    jpeg(image_location, width = image_scale * dim_x,
                        height = image_scale * dim_y, units = "px",
                        pointsize = 12 * image_scale)
                } else if(cur_ext == "tiff"){
                    tiff(image_location, width = image_scale * dim_x,
                        height = image_scale * dim_y, units = "px",
                        pointsize = 12 * image_scale)
                }

                par(bty="n", mai=c(0,0,0,0), xaxs="i",
                    yaxs="i", xaxt="n", yaxt="n", col = "white")
            }

            if(plottingParam$display == "all"){
                rasterImage(Image(out_img[[ind]]),
                            xleft, ybottom,
                            xright, ytop,
                            interpolate = plottingParam$interpolate)
            } else {
                plot(c(0, dim_x), c(0, dim_y), type="n", xlab="", ylab="",
                    asp = 1, ylim = rev(c(0, dim_y)))
                rasterImage(Image(out_img[[ind]]),
                            xleft = 0, ybottom = dim_y,
                            xright = dim_x, ytop = 0,
                            interpolate = plottingParam$interpolate)
            }

            if(ind == legend_ind && !is.null(plottingParam$legend)){
                # Plot legend
                .plotLegend(object, image, exprs_values, outline_by, colour_by,
                            m_width, m_height, cur_col, plottingParam)
            }

            if(ind != legend_ind && !is.null(plottingParam$scale_bar)){
                if(plottingParam$scale_bar$frame == "all"){
                    if(plottingParam$display == "all"){
                        .plotScaleBar(plottingParam$scale_bar,
                                xl = xleft, xr = xright,
                                yt = ytop, yb = ybottom,
                                m_width, m_height,
                                image_scale)
                    } else {
                        .plotScaleBar(plottingParam$scale_bar,
                                xl = 0, xr = dim_x,
                                yt = 0, yb = dim_y,
                                m_width, m_height,
                                image_scale)
                    }
                } else {
                    frame_ind <- as.integer(plottingParam$scale_bar$frame)
                    cur_ind <- legend_ind + frame_ind
                    if(ind == cur_ind && !is.null(plottingParam$scale_bar)){
                        if(plottingParam$display == "all"){
                            .plotScaleBar(plottingParam$scale_bar,
                                    xl = xleft, xr = xright,
                                    yt = ytop, yb = ybottom,
                                    m_width, m_height,
                                    image_scale)
                        } else {
                            .plotScaleBar(plottingParam$scale_bar,
                                    xl = 0, xr = dim_x,
                                    yt = 0, yb = dim_y,
                                    m_width, m_height,
                                    image_scale)
                        }
                    }
                }
            }

            # Plot title on images
            if(ind != legend_ind && !is.null(plottingParam$image_title)){
                if(plottingParam$display == "all"){
                    .plotImageTitle(out_img, mask, image, img_id,
                                ind, legend_ind, plottingParam$image_title,
                                dim_x, xl = xleft, xr = xright,
                                yt = ytop, yb = ybottom)
                } else {
                    .plotImageTitle(out_img, mask, image, img_id,
                                ind, legend_ind, plottingParam$image_title,
                                dim_x, xl = 0, xr = dim_x,
                                yt = 0, yb = dim_y)
                }
            }

            # Close device
            if(!is.null(plottingParam$save_plot) &&
                plottingParam$display == "single"){
                dev.off()
            }

            if(plottingParam$return_plot && plottingParam$display == "single"){
                cur_plot <- recordPlot()

                if(ind == legend_ind && !is.null(plottingParam$legend)){
                    cur_out[["legend"]] <- cur_plot
                    next
                }

                # Set the title correctly
                image_title <- plottingParam$image_title
                if(!is.null(image_title$text)){
                    cur_title <- image_title$text[ind - legend_ind]
                } else if(!is.null(mask) && !is.null(img_id)){
                    cur_title <- mcols(mask)[ind - legend_ind,img_id]
                } else if(!is.null(image) && !is.null(img_id)){
                    cur_title <- mcols(image)[ind - legend_ind,img_id]
                } else if(!is.null(names(out_img))){
                    cur_title <- names(out_img)[ind]
                } else {
                    cur_title <- as.character(ind - legend_ind)
                }

                cur_out[[as.character(cur_title)]] <- cur_plot
            }
        }
    }

    if(plottingParam$return_plot && plottingParam$display == "all"){
        cur_out <- recordPlot()
    }

    if(!is.null(plottingParam$save_plot)){
        dev.off()
    }

    if(plottingParam$return_plot){
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
    if(!is.null(colour_by) &&
        (all(colour_by %in% rownames(object)) || !is.null(image))){

        # Maximum title width
        title_width <- max(strwidth(colour_by, font = colour_by.title.font))

        # Maximum label width
        if(is.null(image)){
            all_max <- max(assay(object, exprs_values)[colour_by,])
        } else {
            all_max <- unlist(lapply(getChannels(image, colour_by), max))
        }
        label_width <- max(strwidth(format(round(all_max, 1), nsmall = 1)))

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
            if(i < 4){
                cur_x <- ((m_width-(2*margin))/6 * i) + margin
                cur_y <- (m_height-(2*margin))/4 + margin
            } else {
                cur_x <- ((m_width-(2*margin))/6 * (i - 3)) + margin
                cur_y <- ((m_height-(2*margin))/4 * 3) + margin
            }
            cur_space_x <- (m_width-(2*margin))/6
            cur_space_y <- (m_height-(2*margin))/2

            if(plottingParam$scale){
                if(is.null(image)){
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
                            format(round(cur_max/2, 1), nsmall = 1),
                            format(round(cur_max, 1), nsmall = 1))
            # Define title cex
            if(is.null(colour_by.title.cex)){
                title_cex <- (cur_space_x/1.5)/title_width
            } else {
                title_cex <- colour_by.title.cex
            }

            # Define label cex
            if(is.null(colour_by.labels.cex)){
                label_cex <- (cur_space_x/2)/label_width
            } else {
                label_cex <- colour_by.labels.cex
            }

            col_ramp <- colorRampPalette(cur_col$colour_by[[col_n]])(101)
            cur_legend <- as.raster(matrix(rev(col_ramp), ncol=1))
            text(x = cur_x, y = cur_y - cur_space_y/2,
                label = col_n, col = "black",
                font = colour_by.title.font,
                cex = title_cex, adj = c(0.5, 1))
            text(x=cur_x- cur_space_x/4 + 2,
                y = seq(cur_y - cur_space_y/2 + cur_space_y/4,
                cur_y + cur_space_y/2 - cur_space_y/8, length.out = 3),
                labels = rev(cur_labels), col = "black",
                adj = 0, cex = label_cex)
            rasterImage(cur_legend,
                    cur_x - cur_space_x/2,
                    cur_y + cur_space_y/2 - cur_space_y/8,
                    cur_x - cur_space_x/4,
                    cur_y - cur_space_y/2 + cur_space_y/4)
        }
    }

    # Next metadata legends
    if(!is.null(object) &&
        !is.null(colour_by) &&
        all(colour_by %in% colnames(colData(object)))){
        # Continous scale
        if(is.null(names(cur_col$colour_by[[1]]))){
            cur_space_x <- (m_width-(2*margin))/4
            cur_space_y <- (m_height-(2*margin))/2
            cur_x <- m_width/2 + cur_space_x
            cur_y <- margin
            cur_min <- min(colData(object)[,colour_by])
            cur_max <- max(colData(object)[,colour_by])
            cur_labels <- c(format(round(cur_min, 1), nsmall = 1),
                            format(round(cur_max/2, 1), nsmall = 1),
                            format(round(cur_max, 1), nsmall = 1))
            label_width <- max(strwidth(rev(cur_labels)))
            title_width <- strwidth(colour_by, font = colour_by.title.font)

            col_ramp <- colorRampPalette(cur_col$colour_by[[1]])(101)
            cur_legend <- as.raster(matrix(rev(col_ramp), ncol=1))

            # Define title cex
            if(is.null(colour_by.title.cex)){
                title_cex <- (cur_space_x/1.5)/title_width
            } else {
                title_cex <- colour_by.title.cex
            }

            text(x = cur_x + cur_space_x/2, y = cur_y,
                label = colour_by, col = "black",
                font = colour_by.title.font,
                cex = title_cex, adj = c(0.5, 1))

            # Define label cex
            if(is.null(colour_by.labels.cex)){
                label_cex <- (cur_space_x/2)/label_width
            } else {
                label_cex <- colour_by.labels.cex
            }

            text(x=cur_x + cur_space_x/2,
                y = seq(cur_y + cur_space_y/4,
                        cur_y + cur_space_y - cur_space_y/8,
                        length.out = 3),
                labels = rev(cur_labels), col = "black",
                adj = 0, cex = label_cex)
            rasterImage(cur_legend,
                        cur_x,
                        cur_y + cur_space_y - cur_space_y/8,
                        cur_x + cur_space_x/2 - 5,
                        cur_y + cur_space_y/4)
            cur_legend_height <- cur_space_y - cur_space_y/8
        } else {
            cur_space_x <- (m_width-(2*margin))/6
            cur_x <- m_width/2 + cur_space_x
            cur_y <- margin
            cur_colouring <- cur_col$colour_by[[1]]
            legend_c <- legend(x = cur_x, y = cur_y,
                                legend = names(cur_colouring),
                                fill = cur_colouring, title = colour_by,
                                text.col = "black", plot = FALSE)

            # Define legend cex
            if(is.null(colour_by.legend.cex)){
                legend_cex <- (m_width-margin-cur_x)/legend_c$rect$w
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
    if(!is.null(outline_by)){
        if(!is.null(colour_by) &&
            all(colour_by %in% colnames(colData(object)))){
            cur_y <- margin + abs(cur_legend_height) + 10
        } else {
            cur_y <- margin
        }

        # Continous scale
        if(is.null(names(cur_col$outline_by[[1]]))){
            cur_space_x <- (m_width-(2*margin))/4
            cur_space_y <- (m_height-(2*margin))/2
            cur_x <- m_width/2 + cur_space_x
            cur_min <- min(colData(object)[,outline_by])
            cur_max <- max(colData(object)[,outline_by])
            cur_labels <- c(format(round(cur_min, 1), nsmall = 1),
                            format(round(cur_max/2, 1), nsmall = 1),
                            format(round(cur_max, 1), nsmall = 1))
            label_width <- max(strwidth(rev(cur_labels)))
            title_width <- strwidth(outline_by, font = colour_by.title.font)

            col_ramp <- colorRampPalette(cur_col$outline_by[[1]])(101)
            cur_legend <- as.raster(matrix(rev(col_ramp), ncol=1))

            # Define title cex
            if(is.null(colour_by.title.cex)){
                title_cex <- (cur_space_x/1.5)/title_width
            } else {
                title_cex <- outline_by.title.cex
            }

            text(x = cur_x + cur_space_x/2, y = cur_y,
                    label = outline_by, col = "black",
                    font = outline_by.title.font,
                    cex = title_cex, adj = c(0.5, 1))

            # Define label cex
            if(is.null(outline_by.labels.cex)){
                label_cex <- (cur_space_x/2)/label_width
            } else {
                label_cex <- outline_by.labels.cex
            }

            text(x=cur_x + cur_space_x/2,
                y = seq(cur_y + cur_space_y/4,
                        cur_y + cur_space_y - cur_space_y/8, length.out = 3),
                labels = rev(cur_labels), col = "black",
                adj = 0, cex = label_cex)
            rasterImage(cur_legend,
                        cur_x,
                        cur_y + cur_space_y - cur_space_y/8,
                        cur_x + cur_space_x/2 - 5,
                        cur_y + cur_space_y/4)
        } else {
            cur_space_x <- (m_width-(2*margin))/6
            cur_x <- m_width/2 + cur_space_x
            cur_colouring <- cur_col$outline_by[[1]]
            legend_o <- legend(x = cur_x, y = cur_y,
                            legend = names(cur_colouring),
                            fill = cur_colouring, title = outline_by,
                            text.col = "black", plot = FALSE)

            # Define legend cex
            if(is.null(outline_by.legend.cex)){
                legend_cex <- (m_width-margin-cur_x)/legend_o$rect$w
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
#' @importFrom graphics strheight text segments
#' @importFrom raster as.raster
.plotScaleBar <- function(scale_bar, xl, xr, yt, yb, m_w, m_h,image_scale){
    # Set default scale bar length
    if (is.null(scale_bar$length)) {
        cur_length <- ifelse(m_w > 25, round(m_w/5, digits = -1), 10)
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
        cur_cex <- (m_h/20)/label_height
    } else {
        cur_cex <- scale_bar$cex
    }
    if (is.null(scale_bar$lwd)) {
        label_height <- abs(strheight(cur_label))
        # Target size is 2% of max image height
        cur_lwd <- (m_h/50) 
    } else {
        cur_lwd <- scale_bar$lwd 
    }
    # Adjust lwd to image scale
    cur_lwd <- cur_lwd * image_scale
    
    print(c(cur_cex, cur_lwd))
    
    cur_col <- scale_bar$colour
    cur_position <- scale_bar$position
    cur_margin.x <- scale_bar$margin[1]
    cur_margin.y <- scale_bar$margin[2]

    # Plot scale bar
    label_height <- abs(strheight(cur_label, cex = cur_cex))
    segm_params <- list(lwd = cur_lwd, col = cur_col, lend = 1)
    text_params <- list(labels = cur_label, cex = cur_cex,
                        col = cur_col, adj = 0.5, lwd = cur_lwd)

    if(cur_position == "bottomright"){
        do.call(segments, append(list(x0 = xr - cur_length - cur_margin.x,
                                y0 = yb - cur_margin.y,
                                x1 = xr - cur_margin.x), segm_params))
        do.call(text, append(list(x = xr - cur_length/2 - cur_margin.x,
                        y = yb - cur_margin.y - label_height - label_height/4),
                            text_params))
    } else if(cur_position == "bottomleft"){
        do.call(segments, append(list(x0 = xl + cur_margin.x,
                                y0 = yb - cur_margin.y,
                                x1 = xl + cur_length + cur_margin.x),
                                segm_params))
        do.call(text, append(list(x = xl + cur_length/2 + cur_margin.x,
                        y = yb - cur_margin.y - label_height - label_height/4),
                        text_params))
    } else if(cur_position == "topright"){
        do.call(segments, append(list(x0 = xr - cur_length - cur_margin.x,
                                y0 = yt + cur_margin.y,
                                x1 = xr - cur_margin.x), segm_params))
        do.call(text, append(list(x = xr - cur_length/2 - cur_margin.x,
                        y = yt + cur_margin.y - label_height - label_height/4),
                        text_params))
    } else if(cur_position == "topleft"){
        do.call(segments, append(list(x0 = xl + cur_margin.x,
                                y0 = yt + cur_margin.y,
                                x1 = xl + cur_length + cur_margin.x),
                                segm_params))
        do.call(text, append(list(x = xl + cur_length/2 + cur_margin.x,
                        y = yt + cur_margin.y - label_height - label_height/4),
                        text_params))
    }
}


# Plot legend
#' @importFrom graphics strwidth strheight text rasterImage legend
#' @importFrom raster as.raster
.plotImageTitle <- function(out_img, mask, image, img_id, ind, legend_ind,
                            image_title, dim_x,
                            xl, xr, yt, yb){

    if(!is.null(image_title$text)){
        cur_title <- image_title$text[ind - legend_ind]
    } else if(!is.null(mask) && !is.null(img_id)){
        cur_title <- mcols(mask)[ind - legend_ind,img_id]
    } else if(!is.null(image) && !is.null(img_id)){
        cur_title <- mcols(image)[ind - legend_ind,img_id]
    } else if(!is.null(names(out_img))){
        cur_title <- names(out_img)[ind]
    } else {
        cur_title <- as.character(ind - legend_ind)
    }

    cur_position <- image_title$position
    cur_cex <- image_title$cex
    cur_col <- image_title$colour
    cur_margin.x <- image_title$margin[1]
    cur_margin.y <- image_title$margin[2]
    cur_font <- image_title$font

    text_params <- list(labels = cur_title, col = cur_col,
                        cex = cur_cex, font = cur_font)

    if(cur_position == "top"){
        do.call(text, append(list(x = xl + dim_x/2,
                                y = yt + cur_margin.y,
                                adj = 0.5), text_params))
    } else if(cur_position == "bottom"){
        do.call(text, append(list(x = xl + dim_x/2,
                                y = yb - cur_margin.y,
                                adj = 0.5), text_params))
    } else if(cur_position == "topleft"){
        do.call(text, append(list(x = xl + cur_margin.x,
                                y = yt + cur_margin.y,
                                adj = 0), text_params))
    } else if(cur_position == "topright"){
        do.call(text, append(list(x = xr - cur_margin.x,
                                y = yt + cur_margin.y,
                                adj = 1), text_params))
    } else if(cur_position == "bottomleft"){
        do.call(text, append(list(x = xl + cur_margin.x,
                                y = yb - cur_margin.y,
                                adj = 0), text_params))
    } else if(cur_position == "bottomright"){
        do.call(text, append(list(x = xr - cur_margin.x,
                                y = yb - cur_margin.y,
                                adj = 1), text_params))
    }
}

