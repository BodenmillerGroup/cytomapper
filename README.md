<img src="inst/imgs/cytomapper_sticker.png" align="right" alt="" width="120" />

# cytomapper

<!-- badges: start -->
[![codecov](https://codecov.io/gh/BodenmillerGroup/cytomapper/branch/master/graph/badge.svg)](https://codecov.io/gh/BodenmillerGroup/cytomapper)
[![docs](https://github.com/BodenmillerGroup/cytomapper/workflows/docs/badge.svg?branch=master)](https://github.com/BodenmillerGroup/cytomapper/actions?query=workflow%3Adocs)
<!-- badges: end -->

R package to spatially visualize pixel- and cell-level information obtained from highly multiplexed imaging.

## Check status

| Bioc branch | Checks |
|:-----------:|:------:|
| Release     |[![build-check-release](https://github.com/BodenmillerGroup/cytomapper/workflows/build-checks-release/badge.svg)](https://github.com/BodenmillerGroup/cytomapper/actions?query=workflow%3Abuild-checks-release)|
| Devel       |[![build-check-devel](https://github.com/BodenmillerGroup/cytomapper/workflows/build-checks-devel/badge.svg)](https://github.com/BodenmillerGroup/cytomapper/actions?query=workflow%3Abuild-checks-devel)|


## Introduction

Highly multiplexed imaging acquires single-cell expression values of selected proteins in a spatially-resolved fashion. 
These measurements can be visualized across multiple length-scales. 
First, pixel-level intensities represent the spatial distributions of feature expression with highest resolution. 
Second, after segmentation, expression values or cell-level metadata (e.g. cell-type information) can be visualized on segmented cell areas. 
This package contains functions for the visualization of multiplexed read-outs and cell-level information obtained by multiplexed imaging cytometry. 
The main functions of this package allow 1. the visualization of pixel-level information across multiple channels (`plotPixels`), 2. the display of cell-level information (expression and/or metadata) on segmentation masks (`plotCells`) and 3. gating + visualization of cells on images (`cytomapperShiny`).

The `cytomapper` package provides toy data that were generated using imaging mass cytometry [1] taken from Damond _et al._ [2].
For further instructions to process raw imaging mass cytometry data, please refer to the [IMC Segmentation Pipeline](https://github.com/BodenmillerGroup/ImcSegmentationPipeline) and the [histoCAT](https://github.com/BodenmillerGroup/histoCAT) as alternative visualization tool.

## Requirements

The `cytomapper` package requires R version >= 4.0.
It builds on data objects and functions contained in the [SingleCellExperiment](https://bioconductor.org/packages/release/bioc/html/SingleCellExperiment.html) and [EBImage](https://bioconductor.org/packages/release/bioc/html/EBImage.html) packages.
Therefore, these packages need to be installed (see below).

## Installation

The `cytomapper` package can be installed from `Bioconductor` via:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install("cytomapper")
```

The development version of the `cytomapper` package can be installed from Github using `remotes` in R.
Please make sure to also install its dependecies:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install(c("EBImage", "SingleCellExperiment"))

# install.packages("remotes")

remotes::install_github("BodenmillerGroup/cytomapper", build_vignettes = TRUE, dependencies = TRUE)
```

To load the package in your R session, type the following:

```r
library(cytomapper)
```

## Functionality

The `cytomapper` package offers three main functions: `plotPixels`, `plotCells` and `cytomapperShiny`.

**plotPixels**

The function takes a `CytoImageList` object (available via the `cytomapper` package) containing multi-channel images representing pixel-level expression values and optionally a `CytoImageList` object containing segementation masks and a `SingleCellExperiment` object containing cell-level metadata.

It allows the visualization of pixel-level information of up to six channels and outlining cells based on cell-level metadata.
To see the full functionality in R type:

```r
?plotPixels
```

**plotCells**

This function takes a `CytoImageList` object containing segementation masks and a `SingleCellExperiment` object containing cell-level mean expression values and metadata information.

It allows the visualization of cell-level expression data and metadata information.
To see the full functionality in R type:

```r
?plotCells
```

**cytomapperShiny**

This Shiny application allows gating of cells based on their expression values and visualises selected cells on their corresponding images. 

It requires at least a `SingleCellExperiment` as input and optionally `CytoImageList` objects containing segmentation masks and multi-channel images.
For full details, please refer to:

```r
?cytomapperShiny
```

## Getting help

For more information on processing imaging mass cytometry data, please refer to the [IMC Segmentation Pipeline](https://github.com/BodenmillerGroup/ImcSegmentationPipeline). 
This pipeline generates multi-channel tiff stacks containing the pixel-level expression values and segementation masks that can be used for the plotting functions in the `cytomapper` package.

More information on how to work with and generate a `SingleCellExperiment` object can be obtained from: [Orchestrating Single-Cell Analysis with Bioconductor](https://osca.bioconductor.org/data-infrastructure.html)

An extensive introduction to image analysis in R can be found at: [Introduction to EBImage](https://bioconductor.org/packages/release/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html)

A full overview on the analysis workflow and functionality of the `cytomapper` package can be found by typing:

```r
vignette("cytomapper")
```

For common issues regarding the `cytomapper` package, please refer to the [wiki](https://github.com/BodenmillerGroup/cytomapper/wiki).

## Demonstrations

To see example usage of the `cytomapper` package, please refer to its [publication repository](https://github.com/BodenmillerGroup/cytomapper_publication) and a number of [workshop demonstrations](https://github.com/BodenmillerGroup/cytomapper_demos).

## Citation

Please cite `cytomapper` as:

```
Nils Eling, Nicolas Damond, Tobias Hoch, Bernd Bodenmiller (2020). cytomapper: an R/Bioconductor package for visualization of highly
  multiplexed imaging data. Bioinformatics, doi: 10.1093/bioinformatics/btaa1061
```

## Authors

[Nils Eling](https://github.com/nilseling) nils.eling 'at' dqbm.uzh.ch

[Nicolas Damond](https://github.com/ndamond)

[Tobias Hoch](https://github.com/toobiwankenobi)


## References

[1] [Giesen et al. (2014), Nature Methods, 11](https://www.nature.com/articles/nmeth.2869)

[2] [Damond et al. (2019), Cell Metabolism, 29](https://www.cell.com/cell-metabolism/fulltext/S1550-4131(18)30691-0)



