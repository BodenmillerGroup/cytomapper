test_that("plotCells: Standard input testing works", {
  data("pancreasSCE")
  data("pancreasMasks")

  expect_error(plotCells(object = "test"),
               regexp = "'object' is not of type 'SingleCellExperiment'.")
  expect_error(plotCells(object = pancreasSCE),
               regexp = 'argument "img_id" is missing, with no default')
  expect_error(plotCells(object = pancreasSCE, img_id = "test"),
               regexp = 'argument "cell_id" is missing, with no default')
  expect_error(plotCells(object = pancreasSCE,
                         img_id = "test", cell_id = "test"),
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.")
  expect_error(plotCells(object = pancreasSCE,
                         img_id = "test", cell_id = "CellNb"),
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.")
  expect_error(plotCells(object = pancreasSCE,
                         img_id = "ImageNb", cell_id = "test"),
               regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.")
  expect_error(plotCells(object = pancreasSCE,
                         img_id = "ImageNb", cell_id = "CellNb"),
               regexp = 'argument "mask" is missing, with no default')
  expect_error(plotCells(object = pancreasSCE,
                         img_id = "ImageNb", cell_id = "CellNb",
                         mask = "test"),
               regexp = "Please provide the segementation mask(s) in form of an 'ImageList' object")
})

test_that("plotCells: Features can be displayed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb"))
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType"))
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
  expect_silent(plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = c("H3", "SMA", "INS", "CD38", "CD44")))
})

test_that("plotCells: Metadata can be displayed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Colour by
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "H3")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "SMA")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "INS")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CD38")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CD44")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = c("H3", "SMA"))
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = c("H3", "SMA", "INS"))
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = c("H3", "SMA", "INS", "CD38", "CD44"))
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Area")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Pos_X")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb",
            colour_by = "Pos_Y")

  # Outline by
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "SMA",
            outline_by = "CellType")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = c("SMA", "INS"),
            outline_by = "CellType")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", outline_by = "CellType")

  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", outline_by = "CellType")

  # exprs_values
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "counts",
            colour_by = "H3")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "exprs",
            colour_by = "H3")

  # subset_images
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", exprs_values = "counts",
            colour_by = "SMA", subset_images = 3)


  # Change size of images
  # Decreasing the size
  cur_images <- pancreasMasks
  setImages(cur_images, "A02_mask") <- cur_images[[1]][1:50, 1:50,,drop=FALSE]

  plotCells(object = pancreasSCE,
            mask = cur_images, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType")

  # Subset cells
  set.seed(12345)
  cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]

  plotCells(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CellType")
  plotCells(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "H3")
  plotCells(object = cur_sce,
            mask = pancreasMasks, img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "H3",
            outline_by = "CellType")

})



