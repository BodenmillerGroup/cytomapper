test_that("Cell-level information can be correctly displayed.", {
  data("pancreasSCE")
  data("pancreasMasks")

  # Colour by
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "CellType")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "H3")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "H3")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "H3")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "H3")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = c("H3", "SMA"))
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = c("H3", "SMA", "INS"))
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb",
            colour_by = c("H3", "SMA", "INS", "CD38", "CD44"))

  # Fix this!
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb",
            colour_by = "Area")

  # Outline by
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "SMA",
            outline_by = "CellType")
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "SMA",
            outline_by = "CellType")

  # Fix this!
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", outline_by = "CellType")

  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", outline_by = "CellType")

  # exprs_values
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", exprs_values = "counts",
            colour_by = "SMA")

  # subset_images
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", exprs_values = "counts",
            colour_by = "SMA", subset_images = 3)

  # return_images
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", exprs_values = "counts",
            colour_by = "SMA", return_images = TRUE)

  # Change size of images
  cur_images <- pancreasMasks
  setImages(cur_images, "A02_mask") <- cur_images[[1]][1:50, 1:50,,drop=FALSE]

  plotCells(object = pancreasSCE,
            mask = cur_images, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "CellType")

  # Subset cells
  set.seed(12345)
  cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]

  # Fix this!
  plotCells(object = cur_sce,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "CellType")
  plotCells(object = cur_sce,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "H3")
  plotCells(object = cur_sce,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", colour_by = "H3",
            outline_by = "CellType")

})



