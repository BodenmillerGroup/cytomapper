# Example data

Publication
Full dataset


## Getting single cell data and cell masks from raw data


The `inst/extdata` folder contains three multiplexed images (image stacks) and the corresponding cell masks.

Original files are Imaging Mass Cytometry (IMC)
Fluidigm Hyperion
`ImcSegmentationPipeline`(https://github.com/BodenmillerGroup/ImcSegmentationPipeline)

Generate cell masks and extract single-cell data



## Generating the toy dataset
The toy data used to illustrate the `cytomapper` package is generated as follows:



This folder is used to store the example data from the pancreas dataset after running the `LoadPancreasData.Rmd` and the `LoadPancreasImages.Rmd` R scripts.

Running these two scripts should generate the following files:
- `pancreas_sce.rds` : A `SingleCellExperiment` object containing the single cell data.
- `pancreas_images.rds`: A `CytoImageList` object containing the image data.
- `pancreas_masks.rds` A `CytoImageList` object containing the cell masks.

These files form a 100 image dataset that is used to illustrate the possibilities of the `cytomapper` package.



# Script to generate toy data for the `cytomapper` package
Running the `1_LoadPancreasData.Rmd` script generates a `SingleCellExperiment` object:  
- `pancreas_sce.rds`: contains the single cell data.  
Runing the `2_LoadPancreasImages.Rmd` generates two `CytoImageList` objects:  
- `pancreas_images.rds`: contains the multiplexed images.
- `pancreas_masks.rds`: contains the cell masks.

These three object form a dataset comprising a hundred multiplexed images that each
contain 38 channels and the associated single-cell data.

For space reasons, this dataset needs to be subsetted in order to generate the small
toy dataset used to illustrate the `cytomapper` package.

In this script, we will crop the images to 100 x 100 pixels and limit the number of
channels to five. We will subset the single-cell dataset accordingly, to retain only
the cells and channels that are present on the toy images.
