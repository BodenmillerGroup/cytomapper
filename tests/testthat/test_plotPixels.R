test_that("Cell-level information can be correctly displayed.", {
  data("pancreasSCE")
  data("pancreasMasks")
  data("pancreasImages")

  # Colour by
  plotPixels(image = pancreasImages)
  plotPixels(image = pancreasImages, colour_by = "H3")
  plotPixels(image = pancreasImages, colour_by = "SMA")
  plotPixels(image = pancreasImages, colour_by = "INS")
  plotPixels(image = pancreasImages, colour_by = "CD38")
  plotPixels(image = pancreasImages, colour_by = "CD44")

  plotPixels(image = pancreasImages,
             colour_by = c("H3", "SMA"))
  plotPixels(image = pancreasImages,
             colour_by = c("H3", "SMA", "INS"))
  plotPixels(image = pancreasImages,
             colour_by = c("H3", "SMA", "INS", "CD38"))
  plotPixels(image = pancreasImages,
             colour_by = c("H3", "SMA", "INS",
                           "CD38", "CD44"))

  # Outline by
  plotPixels(image = pancreasImages,
            mask = pancreasMasks,
            img_id = "ImageNb")
  # Fix legend!
  plotPixels(image = pancreasImages,
             object = pancreasSCE,
             mask = pancreasMasks,
             img_id = "ImageNb",
             cell_id = "CellNb",
             outline_by = "CellType")
  plotPixels(image = pancreasImages,
             object = pancreasSCE,
             mask = pancreasMasks,
             img_id = "ImageNb",
             cell_id = "CellNb",
             colour_by = "CD44",
             outline_by = "CellType")

  # subset_images
  plotPixels(image = pancreasImages,
             object = pancreasSCE,
             mask = pancreasMasks,
             img_id = "ImageNb",
             cell_id = "CellNb",
             colour_by = "CD44",
             outline_by = "CellType",
             subset_images = 3)


  # Change size of images
  # Decreasing the size
  cur_masks <- pancreasMasks
  setImages(cur_masks, "A02_mask") <- cur_masks[[1]][1:50, 1:50,,drop=FALSE]
  cur_images <- pancreasImages
  setImages(cur_images, "A02_imc") <- cur_images[[1]][1:50, 1:50,,drop=FALSE]

  plotPixels(image = cur_images,
             object = pancreasSCE,
             mask = cur_masks,
             img_id = "ImageNb",
             cell_id = "CellNb",
             colour_by = "CD44",
             outline_by = "CellType")

  # Subset cells
  set.seed(12345)
  cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]

  plotPixels(image = pancreasImages,
             object = cur_sce,
             mask = pancreasMasks,
             img_id = "ImageNb",
             cell_id = "CellNb",
             colour_by = "CD44",
             outline_by = "CellType")

})



