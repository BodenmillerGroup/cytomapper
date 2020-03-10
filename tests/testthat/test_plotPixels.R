test_that("plotPixels: Standard input testing works", {
  data("pancreasSCE")
  data("pancreasImages")
  data("pancreasMasks")

  # Works
  # Add test if only image can be displayed
  expect_silent(plotPixels(image = pancreasImages))
  expect_silent(plotPixels(image = pancreasImages,
                          mask = pancreasMasks,
                          img_id = "ImageNb"))

  # Error
  expect_error(plotPixels(image = pancreasImages,
                           mask = pancreasMasks),
               regexp = "'img_id' is missing.",
               fixed = TRUE)
  expect_error(plotPixels(image = "test"),
               regexp = "Please provide the image(s) in form of an 'ImageList' object",
               fix = TRUE)
  expect_error(plotPixels(image = pancreasImages, object = "test"),
               regexp = "'object' is not of type 'SingleCellExperiment'.",
               fix = TRUE)
  expect_error(plotPixels(object = pancreasSCE),
               regexp = 'argument "image" is missing, with no default',
               fix = TRUE)
  expect_error(plotPixels(object = pancreasSCE, image = pancreasImages,
                         img_id = "test"),
               regexp = "'img_id' not in 'mcols(image)'.",
               fix = TRUE)
  expect_error(plotPixels(object = pancreasSCE, image = pancreasImages,
                         img_id = "ImageNb", cell_id = "test"),
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.",
               fix = TRUE)
  expect_error(plotPixels(object = pancreasSCE, image = pancreasImages,
                          mask = "test",
                          img_id = "ImageNb", cell_id = "CellNb"),
               regexp = "Please provide the segmentation mask(s) in form of an 'ImageList' object",
               fix = TRUE)

  mcols(pancreasMasks)$ImageNb <- NULL
  expect_error(plotPixels(image = pancreasImages,
                          mask = pancreasMasks,
                          img_id = "ImageNb"),
               regexp = "'img_id' not in 'mcols(mask)'.",
               fixed = TRUE)
})

test_that("plotPixels: Features can be displayed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "H3"))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "SMA"))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "INS"))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "CD38"))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "CD44"))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = c("H3", "SMA")))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = c("H3", "SMA", "INS")))
  expect_silent(plotPixels(image = pancreasImages,
            colour_by = c("H3", "SMA", "INS", "CD38", "CD44")))

  # Error
  expect_error(plotPixels(image = pancreasImages, colour_by = "test"),
               regexp = "'colour_by' not in 'channelNames(image)' slot.",
               fix = TRUE)
  expect_error(plotPixels(image = pancreasImages, colour_by = c("H3", "test")),
               regexp = "'colour_by' not in 'channelNames(image)' slot.",
               fix = TRUE)

})

test_that("plotPixels: Channels can be enhanced.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "H3", bcg = list(H3 = c(0,1,1))))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "H3", bcg = list(H3 = c(0,2,1))))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "H3", bcg = list(H3 = c(0,1,1.1))))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "H3", bcg = list(H3 = c(100,1,1))))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = c("H3", "SMA"),
                           bcg = list(H3 = c(0,1,1),
                                      SMA = c(0,1,1))))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = c("H3", "SMA"),
                           bcg = list(H3 = c(0,1,1),
                                      SMA = c(0,2,1))))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = c("H3", "SMA"),
                           bcg = list(H3 = c(0,1,1),
                                      SMA = c(0,1,2))))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = c("H3", "SMA"),
                           bcg = list(H3 = c(0,1,1),
                                      SMA = c(10,2,1))))

  # Error
  expect_error(plotPixels(image = pancreasImages,
                           colour_by = c("H3", "SMA"),
                           bcg = list(test = c(0,1,1))),
               regexp = "'bcg': names do not match 'colour_by' argument",
               fixed = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          colour_by = c("H3", "SMA"),
                          bcg = list(H3 = c(0,1))),
               regexp = "'bcg': specify in form of c(0,1,1)",
               fixed = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          colour_by = c("H3", "SMA"),
                          bcg = list(H3 = c("test",1,1))),
               regexp = "'bcg': specify in form of numeric entries",
               fixed = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          colour_by = c("H3", "SMA"),
                          bcg = list(test = c(0,1,1))),
               regexp = "'bcg': names do not match 'colour_by' argument",
               fixed = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          colour_by = c("H3", "SMA"),
                          bcg = c(0,1,1)),
               regexp = "'bcg': please specify a list object",
               fixed = TRUE)

})

test_that("plotPixels: Metadata can be displayed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Area"))
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Pos_X"))
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Pos_Y"))

  # Error
  expect_error(plotPixels(mask = pancreasMasks,
                         colour_by = "H3"),
               regexp = "Please provide a SingleCellExperiment 'object'.",
               fix = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
               mask = pancreasMasks, img_id = "ImageNb",
               cell_id = "CellNb",
               colour_by = c("CellType", "test")),
               regexp = "'colour_by' not in 'rownames(object)' or the 'colData(object)' slot.",
               fix = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb",
                         colour_by = c("CellType", "Area")),
               regexp = "Only one 'colour_by' entry allowed when selecting a 'colData(object)' slot.",
               fix = TRUE)

})

test_that("plotPixels: Cells can be outlined correctly.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            outline_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "SMA",
                          outline_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "SMA",
            outline_by = "Area"))


  # Error
  expect_error(plotPixels(mask = pancreasMasks,
                         outline_by = "CellType"),
               regexp = "Please provide a SingleCellExperiment 'object'.",
               fix = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "SMA",
                          outline_by = "test"),
               regexp = "'outline_by' not in 'colData(object)' slot.",
               fixed = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", colour_by = "SMA",
                         outline_by = c("CellType", "Area")),
               regexp = "Only one 'outline_by' entry allowed.",
               fixed = TRUE)
})

test_that("plotPixels: Exprs values can be correctly set.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "counts",
            colour_by = "H3"))
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "exprs",
            colour_by = "H3"))

  # Error
  expect_error(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "logcounts",
                          colour_by = "H3"),
               regexp = "'exprs_values' not an assay entry in 'object'.",
               fixed = TRUE)

})

test_that("plotPixels: images can be correctly subsetted.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "counts",
            colour_by = "SMA", subset_images = 1))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA", subset_images = 1:3))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA", subset_images = 1:2))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA", subset_images = c(1,3)))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA", subset_images = "A02_mask"))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA",
                          subset_images = c("A02_mask", "F01_mask")))

  # Use mcols entry
  mcols(pancreasMasks)$MaskName <- paste0(names(pancreasMasks), ".tiff")
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "MaskName",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA",
                          subset_images = c("A02_mask.tiff", "F01_mask.tiff")))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "MaskName",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA",
                          subset_images = 1:2))

  # Error
  expect_error(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA",
                          subset_images = 4),
               regexp = "subscript contains out-of-bounds indices",
               fixed = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         colour_by = "SMA",
                         subset_images = "test"),
               regexp = "subscript contains invalid names",
               fixed = TRUE)

})

test_that("plotPixels: colour can be correctly adjusted.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Works
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "SMA",
                          colour = list(SMA = colorRampPalette(c("black", "red"))(100))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "SMA",
                          colour = list(SMA = c("black", "red"))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "CD44"),
                          colour = list(H3 = colorRampPalette(c("black", "red"))(100),
                                        CD44 = colorRampPalette(c("black", "green"))(100))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "Area",
                          colour = list(Area = colorRampPalette(c("black", "red"))(100))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          colour = list(Area = c("black", "red"))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(H3 = c("black", "green"))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType", outline_by = "Area",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType", outline_by = "Area",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"),
                                        Area = c("black", "green"))))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType", outline_by = "Area",
                          colour = list(Area = c("black", "green"))))


  # Error
  expect_error(plotPixels(object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", exprs_values = "counts",
                          colour_by = "SMA",
                          colour = list(SMA = "green")),
               regexp = "Please specify at least two colours when colouring features.",
               fixed = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         colour_by = "SMA",
                         colour = list(test = c("black", "green"))),
               regexp = "'names(colour)' do not match with 'colour_by' and/or 'outline_by'",
               fixed = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         colour_by = "CellType",
                         colour = list(CellType = "green")),
               regexp = "Please specify colours for all 'colour_by' levels.",
               fixed = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         outline_by = "CellType",
                         colour = list(CellType = "green")),
               regexp = "Please specify colours for all 'outline_by' levels.",
               fixed = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         colour_by = "Area",
                         colour = list(Area = "green")),
               regexp = "Please specify at least two colours when colouring continous entries.",
               fixed = TRUE)
  expect_error(plotPixels(object = pancreasSCE,
                         mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", exprs_values = "counts",
                         outline_by = "Area",
                         colour = list(Area = "green")),
               regexp = "Please specify at least two colours when colouring continous entries.",
               fixed = TRUE)
})

test_that("plotPixels: SCE can be subsetted.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Subset cells
  set.seed(12345)
  cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]

  expect_silent(plotPixels(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType"))
  expect_silent(plotPixels(object = cur_sce,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "CellType",
                colour = list(CellType = c(celltype_B = "green",
                                           celltype_A = "blue",
                                           celltype_C = "red"))))
  expect_silent(plotPixels(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "H3"))
  expect_silent(plotPixels(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "H3",
            outline_by = "CellType"))

  cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
  cur_sce <- cur_sce[,1:10]
  expect_silent(plotPixels(object = cur_sce,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "CellType"))
  expect_silent(plotPixels(object = cur_sce,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "CellType",
                          subset_images = unique(cur_sce$ImageNb)))
})

test_that("plotPixels: Size of images can be changed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Change size of images
  # Decreasing the size
  cur_images <- pancreasMasks
  setImages(cur_images, "A02_mask") <- cur_images[[1]][1:50, 1:50,,drop=FALSE]

  expect_silent(plotPixels(object = pancreasSCE,
            mask = cur_images, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
            mask = cur_images, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType",
            subset_images = 1))
  expect_silent(plotPixels(object = pancreasSCE,
                          mask = cur_images, img_id = "ImageNb",
                          cell_id = "CellNb", outline_by = "CellType",
                          subset_images = 1))
})



