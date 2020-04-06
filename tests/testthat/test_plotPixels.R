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
  expect_error(expect_warning(plotPixels(image = pancreasMasks,
                          mask = pancreasMasks,
                          img_id = "ImageNb")))
  expect_error(plotPixels(image = pancreasImages,
                           mask = pancreasMasks),
               regexp = "'img_id' is missing.",
               fixed = TRUE)
  expect_error(plotPixels(image = "test"),
               regexp = "Please provide the image(s) in form of a 'CytoImageList' object",
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
               regexp = "Please provide the segmentation mask(s)\nin form of a 'CytoImageList' object",
               fix = TRUE)

  mcols(pancreasMasks)$ImageNb <- NULL
  expect_error(plotPixels(image = pancreasImages,
                          mask = pancreasMasks,
                          img_id = "ImageNb"),
               regexp = "'img_id' not in 'mcols(mask)'.",
               fixed = TRUE)
})

test_that("plotPixels: Features can be displayed.", {
  data("pancreasImages")

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
  expect_silent(test1 <- plotPixels(image = pancreasImages,
            colour_by = c("H3", "SMA", "INS", "CD38", "CD44"),
            return_plot = TRUE, display = "single"))
  expect_silent(test2 <- plotPixels(image = rev(pancreasImages),
                           colour_by = c("H3", "SMA", "INS", "CD38", "CD44"),
                           return_plot = TRUE, display = "single"))
  expect_identical(test1$plot$A02_imc, test2$plot$A02_imc)
  expect_identical(test1$plot$D01_imc, test2$plot$D01_imc)
  expect_identical(test1$plot$F01_imc, test2$plot$F01_imc)

  # Error
  expect_error(plotPixels(image = pancreasImages, colour_by = "test"),
               regexp = "'colour_by' not in 'channelNames(image)' slot.",
               fix = TRUE)
  expect_error(plotPixels(image = pancreasImages, colour_by = c("H3", "test")),
               regexp = "'colour_by' not in 'channelNames(image)' slot.",
               fix = TRUE)

  channelNames(pancreasImages) <- NULL
  expect_error(plotPixels(image = pancreasImages,
                           colour_by = "H3"),
               regexp = "'channelNames(image)' not set.",
               fixed = TRUE)

})

test_that("plotPixels: Channels can be enhanced.", {
  data("pancreasImages")

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

test_that("plotPixels: Cells can be outlined correctly.", {
  data("pancreasSCE")
  data("pancreasImages")
  data("pancreasImages")

  # Works
  expect_silent(plotPixels(object = pancreasSCE,
            mask = pancreasMasks,
            image = pancreasImages,
            img_id = "ImageNb",
            cell_id = "CellNb",
            outline_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
                           mask = pancreasMasks,
                           image = pancreasImages,
                           img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "SMA",
                          outline_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
                           mask = pancreasMasks,
                           image = pancreasImages,
                           img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "SMA",
            outline_by = "Area"))

  # Error
  expect_error(plotPixels(image = pancreasImages,
                         outline_by = "CellType"),
               regexp = "Outlining cells: provide a SingleCellExperiment 'object' \nand segmentation 'mask' object.",
               fix = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "CellType"),
               regexp = "Outlining cells: provide a SingleCellExperiment 'object' \nand segmentation 'mask' object.",
               fix = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "SMA",
                          outline_by = "test"),
               regexp = "'outline_by' not in 'colData(object)' slot.",
               fixed = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          object = pancreasSCE,
                          mask = pancreasMasks, img_id = "ImageNb",
                         cell_id = "CellNb", colour_by = "SMA",
                         outline_by = c("CellType", "Area")),
               regexp = "Only one 'outline_by' entry allowed.",
               fixed = TRUE)
})

test_that("plotPixels: images can be correctly subsetted.", {
  data("pancreasSCE")
  data("pancreasMasks")
  data("pancreasImages")

  # Works
  expect_silent(plotPixels(image = pancreasImages,
            colour_by = "SMA", subset_images = 1))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "SMA", subset_images = 1:3))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "SMA", subset_images = 1:2))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "SMA", subset_images = c(1,3)))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "SMA",
                           subset_images = "F01_imc"))
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "SMA",
                           subset_images = c("A02_imc", "F01_imc")))
  
  # Setting the image title
  expect_silent(plotPixels(image = pancreasImages,
                           colour_by = "SMA",
                           subset_images = c("A02_imc", "F01_imc"),
                           image_title = list(text = c("test1", "test2"))))

  # Use mcols entry
  mcols(pancreasImages)$ImageName <- paste0(names(pancreasImages), ".tiff")
  expect_silent(plotPixels(image = pancreasImages,
                           img_id = "ImageName",
                           colour_by = "SMA",
                          subset_images = c("A02_imc.tiff", "F01_imc.tiff")))
  expect_silent(plotPixels(image = pancreasImages,
                           img_id = "ImageName",
                          cell_id = "CellNb",
                          colour_by = "SMA",
                          subset_images = 1:2))

  # Image and Mask
  expect_silent(plotPixels(image = pancreasImages,
                           mask = pancreasMasks,
                           object = pancreasSCE,
                           img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "SMA",
                           outline_by = "CellType",
                           subset_images = 1:2))
  
  # Error
  expect_error(plotPixels(image = pancreasImages,
                          subset_images = 4),
               regexp = "subscript contains out-of-bounds indices",
               fixed = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          colour_by = "SMA",
                         subset_images = "test"),
               regexp = "subscript contains invalid names",
               fixed = TRUE)

  mcols(pancreasMasks)$ImageNb <- c("test1", "test2", "test3")
  expect_error(plotPixels(image = pancreasImages,
                          mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "SMA",
                          outline_by = "CellType",
                          subset_images = 1:2),
               regexp = "Mask and image ids must be identical.",
               fixed = TRUE)
  
  expect_error(plotPixels(image = pancreasImages,
                           colour_by = "SMA",
                           subset_images = c("A02_imc", "F01_imc"),
                           image_title = list(text = c("test1", "test2", "test3"))),
                regexp = "Invalid entry to the 'image_title' list object: \nPlease specify one title per image.",
                fixed = TRUE)
})

test_that("plotPixels: colour can be correctly adjusted.", {
  data("pancreasSCE")
  data("pancreasMasks")
  data("pancreasImages")

  # Works
  expect_silent(plotPixels(image = pancreasImages,
                          colour_by = "SMA",
                          colour = list(SMA = colorRampPalette(c("black", "red"))(100))))
  expect_silent(plotPixels(image = pancreasImages,
                          colour_by = "SMA",
                          colour = list(SMA = c("black", "red"))))
  expect_silent(plotPixels(image = pancreasImages,
                          colour_by = c("H3", "CD44"),
                          colour = list(H3 = colorRampPalette(c("black", "red"))(100),
                                        CD44 = colorRampPalette(c("black", "green"))(100))))
  expect_silent(plotPixels(object = pancreasSCE,
                           image = pancreasImages,
                          mask = pancreasMasks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"))))
  expect_silent(plotPixels(object = pancreasSCE,
                           image = pancreasImages,
                           mask = pancreasMasks, img_id = "ImageNb",
                           cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(H3 = c("black", "green"))))
  expect_silent(plotPixels(object = pancreasSCE,
                           image = pancreasImages,
                           mask = pancreasMasks, img_id = "ImageNb",
                           cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"),
                                        H3 = c("black", "green"))))


  # Error
  expect_error(plotPixels(image = pancreasImages,
                          colour_by = "SMA",
                          colour = list(SMA = "green")),
               regexp = "Please specify at least two colours when colouring features.",
               fixed = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                         colour_by = "SMA",
                         colour = list(test = c("black", "green"))),
               regexp = "'names(colour)' do not match with 'colour_by' and/or 'outline_by'",
               fixed = TRUE)
  expect_error(plotPixels(image = pancreasImages,
                          colour_by = c("H3", "SMA"),
                          colour = list(H3 = c("black", "green"))),
               regexp = "Please specify colour gradients for all features.",
               fixed = TRUE)
})

test_that("plotPixels: SCE can be subsetted.", {
  data("pancreasSCE")
  data("pancreasMasks")
  data("pancreasImages")

  # Subset cells
  set.seed(12345)
  cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]

  expect_silent(plotPixels(object = cur_sce,
                           image = pancreasImages,
                           mask = pancreasMasks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"))
  expect_silent(plotPixels(object = cur_sce,
                           image = pancreasImages,
                           mask = pancreasMasks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType",
                colour = list(CellType = c(celltype_B = "green",
                                           celltype_A = "blue",
                                           celltype_C = "red"))))

  cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
  cur_sce <- cur_sce[,1:10]
  expect_silent(plotPixels(object = cur_sce,
                           image = pancreasImages,
                           mask = pancreasMasks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"))
  expect_silent(plotPixels(object = cur_sce,
                           image = pancreasImages,
                           mask = pancreasMasks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType",
                          subset_images = unique(cur_sce$ImageNb)))
})

test_that("plotPixels: Size of images can be changed.", {
  data("pancreasSCE")
  data("pancreasMasks")
  data("pancreasImages")

  # Change size of images
  # Decreasing the size
  cur_masks <- pancreasMasks
  setImages(cur_masks, "A02_mask") <- cur_masks[[1]][1:50, 1:10,drop=FALSE]
  expect_error(plotPixels(object = pancreasSCE,
                           image = pancreasImages,
                           mask = cur_masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"),
               regexp = "Mask and image entries must have the same dimensions.",
               fixed = TRUE)


  cur_images <- pancreasImages
  setImages(cur_images, "A02_imc") <- cur_images[[1]][1:50, 1:10,,drop=FALSE]

  expect_silent(plotPixels(object = pancreasSCE,
                           image = cur_images,
                           mask = cur_masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
                           image = cur_images,
                           mask = cur_masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3",
            subset_images = 1))
  expect_silent(plotPixels(image = cur_images,
                           colour_by = "H3",
                           subset_images = 1,
                           legend = list(colour_by.labels.cex = 0.6,
                                         colour_by.title.cex = 0.5)))
})



