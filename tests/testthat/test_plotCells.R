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

  # save_images
  plotCells(object = pancreasSCE,
            mask = pancreasMasks, image_ID = "ImageNb",
            cell_ID = "CellNb", exprs_values = "counts",
            colour_by = "SMA", save_images =  "")


})



