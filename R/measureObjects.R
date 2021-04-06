#' @title Compute morphological and intensity features from objects on images.
#' @name measureObjects
#'
#' @description
#'
#' @param mask
#' @param image
#' @param img_id
#' @param features allowed basic shape moment haralick
#' @param ... 
#'
#' @return A \linkS4class{SingleCellExperiment} object
#'
#' @section The returned SingleCellExperiment objects:
#' basic_feature defines the entry to counts assay
#' moment_features are only computed on masks
#' Rotation invariant haralick features: average in all four directions. 
#' 
#'
#' @examples
#' # Examples
#'
#' @seealso
#' \code{\link{computeFeatures}}, for reading in individual images.
#' \url{https://earlglynn.github.io/RNotes/package/EBImage/Haralick-Textural-Features.html} for more discussion on the haralick features
#'
#' @author Nils Eling (\email{nils.eling@@dqbm.uzh.ch}),
#'
#' @export
#' @importFrom EBImage computeFeatures.basic computeFeatures.shape computeFeatures.moment computeFeatures.haralick
#' @importFrom BiocParallel SerialParam bpmapply
measureObjects <- function(mask, 
                           image,
                           img_id,
                           feature_types = c("basic", "shape", "moment"), 
                           basic_feature = "mean", 
                           shape_feature = c("area", "radius.mean"),
                           moment_feature = c("cx", "cy", "majoraxis", "eccentricity"),
                           haralick_feature = NULL,
                           basic.quantiles = NULL,
                           haralick.nbins = 32,
                           haralick.scales = c(1, 2),
                           BPPARAM = SerialParam(),
                           ...) {

    # Validity checks
    .valid.mask(mask)
    .valid.image(image)
    .valid.matchObjects.measureObjects(mask, image, img_id)
    .valid.features(feature_types, basic_feature, shape_feature, moment_feature, 
                    haralick_feature, basic.quantiles,
                    haralick.nbins, haralick.scales)
    
    # Define quantiles
    if (is.null(basic.quantiles)) {
        basic.quantiles <- 0
    } 
    
    cur_out <- bpmapply(function(cur_mask, cur_image) {
        
        # Compute basic features
        cur_basic <- apply(as.array(cur_image), 3, function(x){
            cur_basic_ch <- computeFeatures.basic(x = cur_mask, ref = x, 
                                                  basic.quantiles = basic.quantiles)
            cur_basic_ch[,grepl(basic_feature, colnames(cur_basic_ch))]
        })
        
        # Compute shape features
        if ("shape" %in% feature_types) {
            cur_shape <- computeFeatures.shape(cur_mask)
            cur_shape <- cur_shape[,sub("s.", "", colnames(cur_shape)) %in% shape_feature]
        }
        
        # Compute moment features
        if ("moment" %in% feature_types) {
            cur_moment <- computeFeatures.moment(cur_mask)
            cur_moment <- cur_moment[,sub("m.", "", colnames(cur_moment)) %in% moment_feature]
        }
        
        # Compute haralick features
        if ("haralick" %in% feature_types) {
            cur_haralick <- apply(as.array(cur_image), 3, function(x){
                cur_haralick_ch <- computeFeatures.haralick(x = cur_mask, ref = x, 
                                                            haralick.nbins = haralick.nbins,
                                                            haralick.scales = haralick.scales)
                rownames(cur_haralick_ch) <- seq_len(nrow(cur_haralick_ch))
                cur_haralick_ch <- cur_haralick_ch[rownames(cur_basic),]
                cur_haralick_ch[,sub("h.", "", colnames(cur_haralick_ch)) %in% haralick_feature]
            })
        }
        
    }, mask, image, BPPARAM = BPPARAM)
    
    return(sce)
}
