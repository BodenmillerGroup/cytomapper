# Example data

Publication
Full dataset

## Short overview
Sgementation
Link to single cell SingleCellExperiment

## Getting single cell data and cell masks from raw data


The `inst/extdata` folder contains three multiplexed images (image stacks) and the corresponding cell masks.

Original files are Imaging Mass Cytometry (IMC)
Fluidigm Hyperion
`ImcSegmentationPipeline`(https://github.com/BodenmillerGroup/ImcSegmentationPipeline)

Generate cell masks and extract single-cell data



## Downloading the dataset
The dataset can be downloaded and saved using the `1_LoadPancreasData.Rmd` and
the `2_LoadPancreasImages.Rmd` scripts.

The `1_LoadPancreasData.Rmd` script generates a `SingleCellExperiment` object:  
- `pancreas_sce.rds`: contains the single cell data.

The `2_LoadPancreasImages.Rmd` generates two `CytoImageList` objects:  
- `pancreas_images.rds`: contains the multiplexed images.
- `pancreas_masks.rds`: contains the cell masks.

These files are saved in the `inst/exdata`sufolder and form a 100 image dataset
that is a subset of the original publication dataset, which comprises 845
images.

## Generating the toy dataset
The toy data used to illustrate the `cytomapper` package is generated from the
three .rds files generated above by running the `3_GenerateToyData.Rmd`.


This folder is used to store the example data from the pancreas dataset after running the `LoadPancreasData.Rmd` and the `LoadPancreasImages.Rmd` R scripts.







# Script to generate toy data for the `cytomapper` package


These three object form a dataset comprising a hundred multiplexed images that each
contain 38 channels and the associated single-cell data.

For space reasons, this dataset needs to be subsetted in order to generate the small
toy dataset used to illustrate the `cytomapper` package.

In this script, we will crop the images to 100 x 100 pixels and limit the number of
channels to five. We will subset the single-cell dataset accordingly, to retain only
the cells and channels that are present on the toy images.
