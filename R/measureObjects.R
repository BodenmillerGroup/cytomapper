#' Compute morphological and intensity features from objects on images.
#'
#' For each object (e.g. cell) identified by segmentation, the
#' \code{measureObjects} function computes intensity features (also referred to
#' as basic features; e.g. mean intensity), shape features (e.g. area), moment
#' features (e.g. position) and haralick features. These features are returned
#' in form of a \linkS4class{SingleCellExperiment} or 
#' \linkS4class{SpatialExperiment} object.
#'
#' @param mask a \code{\linkS4class{CytoImageList}} object containing single-channel
#' \code{\linkS4class{Image}} or \code{\linkS4class{HDF5Array}} objects.
#' Segmentation masks must contain integer pixel values where groups of
#' pixels correspond to objects.
#' @param image a \code{\linkS4class{CytoImageList}} object containing single or
#' multi-channel \code{\linkS4class{Image}} or \code{\linkS4class{HDF5Array}}
#' objects, where each channel indicates the measured pixel intensities.
#' @param img_id character specifying the \code{mcols(image)} and
#' \code{mcols(mask)} entry, in which the image IDs are stored.
#' @param return_as single character specifying the class of the returned object.
#' This is either \code{"sce"} to return a \code{SingleCellExperiment} (default)
#' or \code{"spe"} to return a \code{SpatialExperiment} object. 
#' @param feature_types character vector or string indicating which features to
#' compute. Needs to contain \code{"basic"}. Optionally, \code{"shape"},
#' \code{"moment"} and \code{"haralick"} are allowed. Default \code{"basic"},
#' \code{"shape"} and \code{"moment"}.
#' @param basic_feature string indicating which intensity measurement per object
#' and channel should be used to populate the \code{counts(x)} slot; where
#' \code{x} is the returned object. Default
#' \code{"mean"} but \code{"sd"}, \code{"mad"} and \code{"q*"} allowed. Here,
#' \code{*} indicates the computed quantile (see \code{basic_quantiles}).
#' @param basic_quantiles numeric vector or single number indicating which
#' quantiles to compute. Default none.
#' @param shape_feature string or character vector specifying which shape
#' features to compute. Default \code{"area"} and \code{"radius.mean"}.
#' Allowed entries are: \code{"area"}, \code{"perimeter"},
#' \code{"radius.mean"}, \code{"radius.sd"}, \code{"radius.max"},
#' \code{"radius.min"}.
#' @param moment_feature string or character vector indicating which moment
#' features to compute. Default \code{"cx"}, \code{"cy"}, \code{"majoraxis"},
#' and \code{"eccentricity"}. Other allowed features are \code{"theta"}. Here
#' moment features are only computed on the segmentation mask without
#' incorporating pixel intensities. Therefore, \code{"cx"} and \code{"cy"} are
#' the x and y coordinates of the cell centroids.
#' @param haralick_feature string or character vector indicating which haralick
#' features to compute. Default none. Allowed are the 13 haralick features:
#' \code{"asm"}, \code{"con"}, \code{"cor"}, \code{"var"}, \code{"idm"},
#' \code{"sav"}, \code{"sva"}, \code{"sen"}, \code{"ent"}, \code{"dva"},
#' \code{"den"}, \code{"f12"}, \code{"f13"}
#' @param haralick_nbins an integer indicating the number of bins used to
#' compute the haralick matrix. Pixel intensities are binned in
#' \code{haralick_nbins} discrete gray levels before computing the haralick
#' matrix.
#' @param haralick_scales an integer vector indicating the number of scales
#' (distance at which to consider neighbouring pixels) to use to compute the
#' haralick features.
#' @param BPPARAM parameters for parallelised processing of images.
#' See \code{\linkS4class{MulticoreParam}} for information on how to use multiple
#' cores for parallelised processing.
#'
#' @return A \linkS4class{SingleCellExperiment} or 
#' \linkS4class{SpatialExperiment} object (see details)
#'
#' @section The returned objects:
#' By default, a \code{SingleCellExperiment} object is returned. When setting
#' \code{return_as = "spe"}, the returned object is of class
#' \code{SpatialExperiment}. The returned object contains a single assay. This
#' assay contains individual objects in columns and channels in rows. Each entry
#' summarises the intensities per object and channel. This summary statistic is
#' typically the mean intensity per object and channel. However, other summary
#' statistics can be computed. When the mean intensity per object and channel is
#' computed (default), the assay is accessible via \code{counts(sce)}.
#' Otherwise, the assay needs to be accessed via \code{assay(sce, "counts_*")},
#' where \code{*} indicates the argument to \code{basic_feature}.
#'
#' The \code{colData(x)} entry is populated by the computed shape, moment and
#' haralick features per object. The prefix of the feature names indicate
#' whether these features correspond to shape (\code{s.}), moment (\code{m.}) or
#' haralick (\code{h.}) features. Default features are the following:
#'
#' \itemize{
#' \item{s.area}{object size in pixels}
#' \item{s.radius.mean}{mean object radius in pixels}
#' \item{m.cx}{x centroid position of object}
#' \item{m.cy}{y centroid position of object}
#' \item{m.majoraxis}{major axis length in pixels of elliptical fit}
#' \item{m.eccentricity}{elliptical eccentricity. 1 meaning straight line and 0
#' meaning circle.}
#' }
#'
#' @section Computing quantiles:
#' Sometimes it can be useful to describe the summarised pixel intensity per
#' object and channel not in terms of the mean but some quantile of the pixel
#' distribution. For example, to compute the median pixel intensity per object
#' and channel, set \code{basic_feature = "q05"} and \code{basic_quantiles =
#' 0.5}.
#'
#' @examples
#' # Standard example
#' data(pancreasImages)
#' data(pancreasMasks)
#'
#' sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb")
#' sce
#'
#' # Compute only intensity feature
#' sce <- measureObjects(pancreasMasks, pancreasImages, img_id = "ImageNb",
#'                         feature_types = "basic")
#' colData(sce)
#'
#' # Visualize on segmentation masks
#' plotCells(pancreasMasks, object = sce, img_id = "ImageNb",
#'             cell_id = "object_id", colour_by = "PIN")
#'
#' @seealso
#' \code{\link{computeFeatures}}, for detailed explanation for the computed features.
#' \url{https://earlglynn.github.io/RNotes/package/EBImage/Haralick-Textural-Features.html}
#' for more discussion on the haralick features
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch}),
#'
#' @export
#' @importFrom EBImage computeFeatures.basic computeFeatures.shape computeFeatures.moment computeFeatures.haralick
#' @importFrom BiocParallel SerialParam bpmapply
#' @importFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment int_metadata
#' @importFrom SummarizedExperiment colData<- assayNames<-
#' @importFrom SpatialExperiment spatialCoords<- SpatialExperiment
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
measureObjects <- function(mask,
                           image,
                           img_id,
                           return_as = c("sce", "spe"),
                           feature_types = c("basic", "shape", "moment"),
                           basic_feature = "mean",
                           basic_quantiles = NULL,
                           shape_feature = c("area", "radius.mean"),
                           moment_feature = c("cx", "cy", "majoraxis", "eccentricity"),
                           haralick_feature = NULL,
                           haralick_nbins = 32,
                           haralick_scales = c(1, 2),
                           BPPARAM = SerialParam()) {

    # Validity checks
    .valid.mask(mask, img_id = img_id)
    .valid.image(image, img_id = img_id)
    .valid.matchObjects.measureObjects(mask, image, img_id)
    .valid.features(feature_types, basic_feature, shape_feature, moment_feature,
                    haralick_feature, basic_quantiles,
                    haralick_nbins, haralick_scales)
    
    return_as <- match.arg(return_as)

    # Define channelNames if not set
    if (is.null(channelNames(image))) {
        channelNames(image) <- paste0("ch", seq_len(dim(image[[1]])[3]))
    }

    # Define quantiles
    if (is.null(basic_quantiles)) {
        basic_quantiles <- 0
    }

    cur_out <- bpmapply(function(cur_mask, cur_image, id) {

        # Compute basic features
        cur_basic <- apply(as.array(cur_image), 3, function(x){
            cur_basic_ch <- computeFeatures.basic(x = as.array(cur_mask), ref = x,
                                                  basic.quantiles = basic_quantiles)
            cur_basic_ch[,grepl(basic_feature, colnames(cur_basic_ch))]
        })
        
        if (length(unique(cur_mask[cur_mask != 0])) == 1) {
            cur_basic <- t(as.matrix(cur_basic))
            rownames(cur_basic) <- unique(cur_mask[cur_mask != 0])
        }

        cur_coldata <- DataFrame(img_id = rep(id, nrow(cur_basic)),
                                 object_id = as.numeric(rownames(cur_basic)))
        names(cur_coldata)[1] <- img_id

        # Compute shape features
        if ("shape" %in% feature_types) {
            cur_shape <- computeFeatures.shape(as.array(cur_mask))
            cur_shape <- cur_shape[,sub("s.", "", colnames(cur_shape)) %in% shape_feature]
            
            if (length(unique(cur_mask[cur_mask != 0])) == 1) {
                cur_shape <- t(as.matrix(cur_shape))
            }

            cur_coldata <- cbind(cur_coldata, cur_shape)
        }

        # Compute moment features
        if ("moment" %in% feature_types) {
            cur_moment <- computeFeatures.moment(as.array(cur_mask))
            cur_moment <- cur_moment[,sub("m.", "", colnames(cur_moment)) %in% moment_feature]
            
            if (length(unique(cur_mask[cur_mask != 0])) == 1) {
                cur_moment <- t(as.matrix(cur_moment))
            }

            cur_coldata <- cbind(cur_coldata, cur_moment)
        }

        # Compute haralick features
        if ("haralick" %in% feature_types) {
            cur_haralick <- apply(as.array(cur_image), 3, function(x){
                cur_haralick_ch <- computeFeatures.haralick(x = as.array(cur_mask), ref = x,
                                                            haralick.nbins = haralick_nbins,
                                                            haralick.scales = haralick_scales)
                rownames(cur_haralick_ch) <- seq_len(nrow(cur_haralick_ch))
                cur_haralick_ch <- cur_haralick_ch[rownames(cur_basic),,drop=FALSE]
                cur_haralick_ch <- cur_haralick_ch[,sub("h.", "", colnames(cur_haralick_ch)) %in% haralick_feature, drop = FALSE]
                as.data.frame(cur_haralick_ch)
            })
            cur_haralick <- do.call("cbind", cur_haralick)

            if (ncol(cur_haralick) == dim(cur_image)[3]) {
                colnames(cur_haralick) <- paste(dimnames(cur_image)[[3]], colnames(cur_haralick), sep = ".")
            }

            cur_coldata <- cbind(cur_coldata, cur_haralick)
        }

        # Generate object
        assay_name <- ifelse(basic_feature == "mean", "counts", 
                             paste0("counts_", basic_feature))
        if (return_as == "sce") {
            cur_object <- SingleCellExperiment(assays = list(counts = t(cur_basic)))
            assayNames(cur_object) <- assay_name
            colData(cur_object) <- cur_coldata
        } else {
            cur_object <- SpatialExperiment(assays = list(counts = t(cur_basic)))
            assayNames(cur_object) <- assay_name
            if (all(c("m.cx", "m.cy") %in% names(cur_coldata))) {
                spatialCoords(cur_object) <- as.matrix(cur_coldata[,c("m.cx", "m.cy")])  
                cur_coldata <- cur_coldata[,!names(cur_coldata) %in% c("m.cx", "m.cy")]
            } 
            cur_coldata$sample_id <- cur_coldata[,img_id]
            colData(cur_object) <- cur_coldata
        }

        return(cur_object)

    }, mask, image, as.list(mcols(mask)[,img_id]), BPPARAM = BPPARAM)

    object <- do.call("cbind", cur_out)
    
    cur_colData <- colData(object)
    cur_df <- cbind(mcols(image), mcols(mask))
    cur_df <- cur_df[,unique(names(cur_df)), drop=FALSE]
    cur_colData <- merge(as.data.frame(cur_colData), 
                         as.data.frame(cur_df), 
                         by = img_id, sort = FALSE)
    colData(object) <- as(cur_colData, "DataFrame")
    
    # Avoid duplication of internal metadata
    int_metadata(object) <- int_metadata(cur_out[[1]])

    return(object)
}
