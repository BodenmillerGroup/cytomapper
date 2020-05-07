# Example data

This folder describes the files contained in `inst/extdata`, how these files
were obtained and how to generate them by running the scripts contained in
`insts/scripts`.

Raw Imaging Mass Cytometry (IMC) data are computationally processed using a
segmentation pipeline (available at https://github.com/BodenmillerGroup/ImcSegmentationPipeline).
This produces image stacks containing the raw pixel values for > 35 channels,
masks containing the segmented cells, cell-level expression, metadata
information as well as a number of image-level meta information.

The dataset used here is associated with the publication [A Map of Human Type 1 Diabetes Progression by Imaging Mass Cytometry](https://doi.org/10.1016/j.cmet.2018.11.014)
and is available from Mendeley Data: [http://dx.doi.org/10.17632/cydmwsfztj.2](http://dx.doi.org/10.17632/cydmwsfztj.2).

The scripts contained in `insts/scripts` can be run to download the data and to
generate the toy dataset used to illustrate the `cytomapper` package.

## Content of the inst/extdata folder:

This folder contains the tiff files from which the toy dataset in `data` was
generated. There are three images stacks (suffix `_imc.tiff`), each containing
5 channels for a size of 100 x 100 pixels and three associated cell masks
(suffix `_mask.tiff`).
The image names (`E34`, `G01`, `J02`) are used to match the image stacks with
the associated mask.

## Downloading the dataset

The dataset can be downloaded and saved using the `1_LoadPancreasData.Rmd` and
the `2_LoadPancreasImages.Rmd` scripts.

The `1_LoadPancreasData.Rmd` script generates a `SingleCellExperiment` object:  
- `pancreas_sce.rds`:  contains the single cell data.

The `2_LoadPancreasImages.Rmd` generates two `CytoImageList` objects:  
- `pancreas_images.rds`: contains the multiplexed images.
- `pancreas_masks.rds`: contains the cell masks.

These files are saved in the `inst/exdata` subfolder and form a dataset
comprising a hundred multiplexed images that each contain 38 channels, as well
as the associated single-cell data. This represents a subset of the original
publication data set, which contains 845 images. The full dataset is also
available from Mendeley Data.

## Generating the toy dataset

For space reasons, the 100 images dataset needs to be subsetted in order to
generate the small toy dataset used to illustrate the `cytomapper` package.

This toy dataset is generated from the three .rds files generated above by
running the `3_GenerateToyData.Rmd` script.

In this script, a subset of three images (named `E34`, `G01`, and `J02`) is
extracted and these images are cropped to 100 x 100 pixels. The number of
channels is limited to five (`H3`, `CD99`, `PIN`, `CD8a` and `CDH`).  

The `SingleCellExperiment` object containing the single-cell data is subsetted
accordingly in order to retain only the cells and channels that are present on
the toy images.

When running the `3_GenerateToyData.Rmd` script, the following files are
created:
In `inst/extdata`:
- imc.tiff files: stacks corresponding to the subsetted images.
- mask.tiff files: masks associated with the imc.tiff files.

In `data`, the toy dataset:
- `pancreasImages.RData`: a `CytoImageList` object containing the images.
- `pancreasMasks.RData`: a `CytoImageList` object containing the masks.
- `pancreasSCE.RData`: a `SingleCellExperiment` object containing the single-
cell data.
