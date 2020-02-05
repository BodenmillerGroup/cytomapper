#' Example SingleCellExperiment object
#'
#' This \linkS4class{SingleCellExperiment} object contains the expression values
#' of 5 proteins (rows) from 282 cells (columns) across 3 images. The data is part
#' of a imaging mass cytometry study on the progression of Type 1 diabetes
#' and therefore contains pancreas cells.
#'
#' @format A SingleCellExperiment object containing the raw and
#'   arcsinh-transformed mean pixel counts per cell as well as associated cell-
#'   and protein-specific metadata. Row names represent the names of the target proteins
#'   and column names represent the image name and cell id of each cell.
#'   \describe{
#'   \item{colData}{Cell-specific metadata where rownames represent the
#'   image name and cell id. It containins the
#'   \enumerate{
#'     \item image number (ImageNB),
#'     \item cell number/identifier (CellNB),
#'     \item spatial position on the image (Pos_X, Pos_Y),
#'     \item shape information (MajorAxisLength, MinorAxisLength),
#'     \item the associated image name (ImageName, see \code{?"\link{pancreasImages}"}),
#'     \item the associated mask name (MaskName, see \code{?"\link{pancreasMasks}"}),
#'     \item a randomized cell-type label (CellType)
#'   }
#'   }
#'   \item{rowData}{Protein-specific metadata where rownames represent
#'   the names of the target proteins. It contains the
#'   \enumerate{
#'     \item channel number (frame),
#'     \item metal tag of the antibody (MetalTag)
#'     \item Target (the expanded name of the targeted protein)
#'     \item clean_Target (the abbreviated name of the targeted protein)
#'   }
#'   }
#'   \item{assays}{List of protein expression counts containing:
#'   \enumerate{
#'     \item the raw expression counts (counts):
#'     mean pixel value per cell and protein,
#'     \item arcsinh-transformed raw expression counts
#'     using a co-factor of 1 (exprs)
#'   }
#'   }
#'   }
#'
#' @references Damond, N. et al., A Map of Human Type 1 Diabetes Progression by
#' Imaging Mass Cytometry, Cell Metabolism 29:3, 2019
"pancreasSCE"

#' Example ImageList object of image files
#'
#' TODO
#'
#' @format An ImageList object containing ...
#' \describe{
#'   \item{...}{...}
#'   \item{...}{...}
#'   ...
#' }
"pancreasImages"

#' Example ImageList object of segmentation masks
#'
#' TODO
#'
#' @format An ImageList object containing ...
#' \describe{
#'   \item{...}{...}
#'   \item{...}{...}
#'   ...
#' }
"pancreasMasks"

