test_that("plotCells: Standard input testing works", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  # Add test if only mask can be displayed
  expect_silent(plotCells(mask = pancreasMasks))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb"))

  # Error
  expect_error(plotCells(mask = "test"),
               regexp = "Please provide the segmentation mask(s)\nin form of a 'CytoImageList' object",
               fix = TRUE)
  expect_error(plotCells(mask = pancreasMasks, object = "test"),
               regexp = "'object' is not of type 'SingleCellExperiment'.",
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE),
               regexp = 'argument "mask" is missing, with no default',
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE, mask = pancreasMasks,
                         img_id = "test"),
               regexp = "'img_id' not in 'mcols(mask)'.",
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE, mask = pancreasMasks,
                         img_id = "ImageNb", cell_id = "test"),
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.",
               fix = TRUE)
})

test_that("plotCells: Features can be displayed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "H3"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "SMA"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "INS"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CD38"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CD44"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = c("H3", "SMA")))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = c("H3", "SMA", "INS")))
  expect_silent(test1 <- plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = c("H3", "SMA", "INS", "CD38", "CD44"),
            return_plot = TRUE, display = "single"))
  expect_silent(test2 <- plotCells(object = pancreasSCE,
                          mask = rev(pancreasMasks), img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "SMA", "INS", "CD38", "CD44"),
                          return_plot = TRUE, display = "single"))
  expect_identical(test1$plot$A02_imc, test2$plot$A02_imc)
  expect_identical(test1$plot$D01_imc, test2$plot$D01_imc)
  expect_identical(test1$plot$F01_imc, test2$plot$F01_imc)

  # Error
  expect_error(plotCells(mask = pancreasMasks, colour_by = "H3"),
               regexp = "Please provide a SingleCellExperiment 'object'.",
               fix = TRUE)
  expect_error(plotCells(mask = pancreasMasks,
                         object = pancreasSCE,
                         colour_by = "H3"),
               regexp = "Please provide an 'img_id' and 'cell_id' argument",
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", colour_by = "test"),
               regexp = "'colour_by' not in 'rownames(object)' or the 'colData(object)' slot.",
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", colour_by = c("H3", "test")),
               regexp = "'colour_by' not in 'rownames(object)' or the 'colData(object)' slot.",
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", colour_by = 1),
               regexp = "'colour_by' not in 'rownames(object)' or the 'colData(object)' slot.",
               fix = TRUE)
})

test_that("plotCells: Metadata can be displayed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Area"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Pos_X"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Pos_Y"))

  # Error
  expect_error(plotCells(mask = pancreasMasks,
                         colour_by = "H3"),
               regexp = "Please provide a SingleCellExperiment 'object'.",
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE,
               mask = pancreasMasks, img_id = "ImageNb",
               cell_id = "CellNb",
               colour_by = c("CellType", "test")),
               regexp = "'colour_by' not in 'rownames(object)' or the 'colData(object)' slot.",
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb",
                         colour_by = c("CellType", "Area")),
               regexp = "Only one 'colour_by' entry allowed when selecting a 'colData(object)' slot.",
               fix = TRUE)

})

test_that("plotCells: Cells can be outlined correctly.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            outline_by = "CellType"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "SMA",
                          outline_by = "CellType"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "SMA",
            outline_by = "Area"))


  # Error
  expect_error(plotCells(mask = pancreasMasks,
                         outline_by = "CellType"),
               regexp = "Please provide a SingleCellExperiment 'object'.",
               fix = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "SMA",
                          outline_by = "test"),
               regexp = "'outline_by' not in 'colData(object)' slot.",
               fixed = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", colour_by = "SMA",
                         outline_by = c("CellType", "Area")),
               regexp = "Only one 'outline_by' entry allowed.",
               fixed = TRUE)
})

test_that("plotCells: Exprs values can be correctly set.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "counts",
            colour_by = "H3"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "exprs",
            colour_by = "H3"))

  # Error
  expect_error(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "logcounts",
                          colour_by = "H3"),
               regexp = "'exprs_values' not an assay entry in 'object'.",
               fixed = TRUE)

})

test_that("plotCells: images can be correctly subsetted.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks[1], img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "counts",
            colour_by = "SMA"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks[1:3], img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks[1:2], img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks[c(1,3)], img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks["A02_mask"], img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks[c("A02_mask", "F01_mask")], img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))
  
  cur_images <- pancreasMasks["A02_mask"]
  expect_silent(plotCells(object = pancreasSCE,
                          mask = cur_images, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))
  
  cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
  expect_silent(plotCells(object = cur_sce,
                          mask = cur_images, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))
  
  # Set image title
  cut_images <- getImages(pancreasMasks, c("A02_mask", "F01_mask"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = cut_images, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA",
                          image_title = list(text = c("test1", "test2"))))

  # Use mcols entry
  mcols(pancreasMasks)$MaskName <- paste0(names(pancreasMasks), ".tiff")
  cur_images <- getImages(pancreasMasks, mcols(pancreasMasks)$MaskName %in% c("A02_mask.tiff", "F01_mask.tiff"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = cur_images, img_id = "MaskName",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))
  cur_images <- getImages(pancreasMasks, 1:2)  
  expect_silent(plotCells(object = pancreasSCE,
                          mask = cur_images, img_id = "MaskName",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA"))

})

test_that("plotCells: colour can be correctly adjusted.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "SMA",
                          colour = list(SMA = colorRampPalette(c("black", "red"))(100))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "SMA",
                          colour = list(SMA = c("black", "red"))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "CD44"),
                          colour = list(H3 = colorRampPalette(c("black", "red"))(100),
                                        CD44 = colorRampPalette(c("black", "green"))(100))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "Area",
                          colour = list(Area = colorRampPalette(c("black", "red"))(100))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          colour = list(Area = c("black", "red"))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(H3 = c("black", "green"))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType", outline_by = "Area",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType", outline_by = "Area",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"),
                                        Area = c("black", "green"))))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType", outline_by = "Area",
                          colour = list(Area = c("black", "green"))))


  # Error
  expect_error(plotCells(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA",
                          colour = list(SMA = "green")),
               regexp = "Please specify at least two colours when colouring features.",
               fixed = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         colour_by = "SMA",
                         colour = list(test = c("black", "green"))),
               regexp = "'names(colour)' do not match with 'colour_by' and/or 'outline_by'",
               fixed = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         colour_by = "CellType",
                         colour = list(CellType = "green")),
               regexp = "Please specify colours for all 'colour_by' levels.",
               fixed = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         outline_by = "CellType",
                         colour = list(CellType = "green")),
               regexp = "Please specify colours for all 'outline_by' levels.",
               fixed = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         colour_by = "Area",
                         colour = list(Area = "green")),
               regexp = "Please specify at least two colours when colouring continous entries.",
               fixed = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         outline_by = "Area",
                         colour = list(Area = "green")),
               regexp = "Please specify at least two colours when colouring continous entries.",
               fixed = TRUE)
  expect_error(plotCells(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         colour_by = c("H3", "SMA"),
                         colour = list(H3 = c("black", "blue"))),
               regexp = "Please specify colour gradients for all features.",
               fixed = TRUE)
})

test_that("plotCells: SCE can be subsetted.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Subset cells
  set.seed(12345)
  cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]

  expect_silent(plotCells(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType"))
  expect_silent(plotCells(object = cur_sce,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "CellType",
                colour = list(CellType = c(celltype_B = "green",
                                           celltype_A = "blue",
                                           celltype_C = "red"))))
  expect_silent(plotCells(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "H3"))
  expect_silent(plotCells(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "H3",
            outline_by = "CellType"))

  cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
  cur_sce <- cur_sce[,1:10]
  expect_silent(plotCells(object = cur_sce,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "CellType"))
  cur_images <- getImages(pancreasMasks, mcols(pancreasMasks)$ImageNb == unique(cur_sce$ImageNb))
  expect_silent(plotCells(object = cur_sce,
                          mask = cur_images, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "CellType"))
})

test_that("plotCells: Size of images can be changed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Change size of images
  # Decreasing the size
  cur_images <- pancreasMasks
  setImages(cur_images, "A02_mask") <- cur_images[[1]][1:50, 1:10,drop=FALSE]

  expect_silent(plotCells(object = pancreasSCE,
            mask = cur_images, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = cur_images, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          display = "single"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = cur_images[1], img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType"))
  expect_silent(plotCells(object = pancreasSCE,
                          mask = cur_images[1], img_id = "ImageNb",
                          cell_id = "CellNb", outline_by = "CellType"))
})



