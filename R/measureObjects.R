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
measureObjects <- function(mask, 
                           image,
                           img_id,
                           feature_types = c("basic", "shape", "moment"), 
                           basic_feature = "mean", 
                           shape_feature = c("area", "radius.mean"),
                           moment_feature = c("majoraxis", "eccentricity"),
                           haralick_feature = NULL,
                           basic.quantiles = NULL,
                           haralick.nbins = 32,
                           haralick.scales = c(1, 2),
                           ...) {

    # Validity checks
    .valid.mask(mask)
    .valid.image(image)
    .valid.matchObjects.measureObjects(mask, image, img_id)
    .valid.features(feature_types, basic_feature, shape_feature, moment_feature, haralick_feature, basic.quantiles,
                    haralick.nbins, haralick.scales)
    
    return(sce)
}
