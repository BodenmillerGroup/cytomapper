test_that("plotting-param: scale_bar can be set.", {
  data("pancreasMasks")

  cur_images <- pancreasMasks
  setImages(cur_images, "A02_mask") <- cur_images[[1]][1:50, 1:50,drop=FALSE]

  # Works
  expect_silent(plotCells(cur_images))
  expect_silent(plotCells(cur_images, scale_bar = NULL))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(length = 50)))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(label = "test")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(cex = 2)))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(lwd = 10)))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(colour = "red")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "topleft")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "topright")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "bottomleft")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "bottomright")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(margin = c(1,1))))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "bottomleft",
                            margin = c(1,1))))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "topright",
                                           margin = c(1,1))))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(frame = "all")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(frame = 1)))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(length = 10,
                                           label = "10 um",
                                           cex = 2,
                                           lwd = 5,
                                           colour = "dark red",
                                           position = "bottomright",
                                           margin = c(20,20))))

  # Error
  expect_error(plotCells(pancreasMasks, scale_bar = list(test = "test")),
               regexp = "Invalid entry to the 'scale_bar' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, scale_bar = list(length = NULL)),
               regexp = "Invalid entry to the 'scale_bar' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, scale_bar = list(label = 1)),
               regexp = "Invalid entry to the 'scale_bar' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, scale_bar = list(cex = "test")),
               regexp = "Invalid entry to the 'scale_bar' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, scale_bar = list(lwd = "test")),
               regexp = "Invalid entry to the 'scale_bar' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, scale_bar = list(colour = "test")),
               regexp = "invalid color name 'test'",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, scale_bar = list(position = "test")),
               regexp = "Invalid entry to the 'scale_bar' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, scale_bar = list(margin = "test")),
               regexp = "Invalid entry to the 'scale_bar' list object",
               fixed = TRUE)
})

test_that("plotting-param: image_title can be set.", {
  data("pancreasMasks")

  cur_images <- pancreasMasks
  setImages(cur_images, "A02_mask") <- cur_images[[1]][1:50, 1:50,drop=FALSE]

  # Works
  expect_silent(plotCells(cur_images))
  expect_silent(plotCells(cur_images, image_title = NULL))
  expect_silent(plotCells(cur_images,
          image_title = list(text = c("test1", "test2", "test3"))))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "top")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "bottom")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "topleft")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "bottomleft")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "topright")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "bottomright")))
  expect_silent(plotCells(cur_images,
                          image_title = list(colour = "red")))
  expect_silent(plotCells(cur_images,
                          image_title = list(margin = c(0,0))))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "topright",
                                             margin = c(0,0))))
  expect_silent(plotCells(cur_images,
                          image_title = list(font = 1)))
  expect_silent(plotCells(cur_images,
                          image_title = list(cex = 3)))

  # Error
  expect_error(plotCells(pancreasMasks, image_title = list(text = "test")),
               regexp = "Invalid entry to the 'image_title' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, image_title = list(position = "test")),
               regexp = "Invalid entry to the 'image_title' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, image_title = list(colour = 1)),
               regexp = "Invalid entry to the 'image_title' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, image_title = list(margin = "test")),
               regexp = "Invalid entry to the 'image_title' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, image_title = list(font = "test")),
               regexp = "Invalid entry to the 'image_title' list object",
               fixed = TRUE)
  expect_error(plotCells(pancreasMasks, image_title = list(cex = "test")),
               regexp = "Invalid entry to the 'image_title' list object",
               fixed = TRUE)

})

test_that("plotting-param: missing_colour can be set.", {
  data("pancreasMasks")
  data("pancreasImages")

  # Works
  expect_silent(plotCells(pancreasMasks, missing_colour = "red"))
  expect_silent(plotPixels(image = pancreasImages,
                           mask = pancreasMasks,
                           img_id = "ImageNb",
                           cell_id = "CellNb",
                           missing_colour = "red"))

  # Error
  expect_error(plotCells(pancreasMasks, missing_colour = "test"),
                regexp = "'missing_colour' not a valid colour.",
               fixed = TRUE)
})

test_that("plotting-param: background_colour can be set.", {
  data("pancreasMasks")

  # Works
  expect_silent(plotCells(pancreasMasks, background_colour = "white"))

  # Error
  expect_error(plotCells(pancreasMasks, background_colour = "test"),
               regexp = "'background_colour' not a valid colour.",
               fixed = TRUE)
})

test_that("plotting-param: save_image can be set.", {
  data("pancreasImages")

  cur_path <- tempfile()
  on.exit(unlink(cur_path))

  # Works
  expect_silent(plotPixels(pancreasImages, save_image = NULL))
  expect_silent(plotPixels(pancreasImages,
                          save_image = list(filename = paste0(cur_path, "test.png"),
                                            scale = 10)))
  expect_true(file.exists(paste0(cur_path, "test.png")))
  expect_silent(plotPixels(pancreasImages,
                           save_image = list(filename = paste0(cur_path, "test.jpeg"),
                                             scale = 10)))
  expect_true(file.exists(paste0(cur_path, "test.jpeg")))
  expect_silent(plotPixels(pancreasImages,
                           save_image = list(filename = paste0(cur_path, "test.tiff"),
                                             scale = 10)))
  expect_true(file.exists(paste0(cur_path, "test.tiff")))

  # Error
  expect_error(plotPixels(pancreasImages,
                          save_image = list(filename = "test")),
               regexp = "Invalid entry to the 'save_image' list object")
  expect_error(plotPixels(pancreasImages,
                          save_image = list(filename = 1)),
               regexp = "Invalid entry to the 'save_image' list object")
  expect_error(plotPixels(pancreasImages,
                          save_image = list(filename = "test.pdf")),
               regexp = "Invalid entry to the 'save_image' list object")
  expect_error(plotPixels(pancreasImages,
                          save_image = list(scale = 1)),
               regexp = "Invalid entry to the 'save_image' list object")
  expect_error(plotPixels(pancreasImages,
                          save_image = list(filename = "test.png",
                                            scale = "test")),
               regexp = "Invalid entry to the 'save_image' list object")
})

test_that("plotting-param: return_plot can be set.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")

  # Works
  expect_silent(cur_out <- plotPixels(pancreasImages, return_plot = FALSE))
  expect_null(cur_out)
  expect_silent(cur_out <- plotPixels(pancreasImages, return_plot = TRUE))
  dev.off()
  expect_silent(cur_out$plot)
  expect_silent(cur_out <- plotCells(pancreasMasks, return_plot = TRUE))
  dev.off()
  expect_silent(cur_out$plot)

  # Error
  expect_error(plotPixels(pancreasImages, return_plot = "test"),
               regexp = "Invalid 'return_plot' entry.")

})

test_that("plotting-param: return_images can be set.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")

  # Works
  expect_silent(cur_out <- plotPixels(pancreasImages, return_images = FALSE))
  expect_null(cur_out)
  expect_silent(cur_out <- plotPixels(pancreasImages, return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(pancreasImages))
  expect_identical(names(cur_out$images), names(pancreasImages))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotPixels(pancreasImages, mask = pancreasMasks,
                                      img_id = "ImageNb",
                                      return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(pancreasImages))
  expect_identical(names(cur_out$images), names(pancreasImages))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotPixels(pancreasImages, mask = pancreasMasks,
                                      object = pancreasSCE,
                                      img_id = "ImageNb",
                                      cell_id = "CellNb",
                                      outline_by = "CellType",
                                      return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(pancreasImages))
  expect_identical(names(cur_out$images), names(pancreasImages))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotPixels(pancreasImages, mask = pancreasMasks,
                                      object = pancreasSCE,
                                      img_id = "ImageNb",
                                      cell_id = "CellNb",
                                      colour_by = c("H3", "SMA"),
                                      outline_by = "CellType",
                                      return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(pancreasImages))
  expect_identical(names(cur_out$images), names(pancreasImages))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotCells(pancreasMasks, return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(pancreasMasks))
  expect_identical(names(cur_out$images), names(pancreasMasks))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotCells(pancreasMasks, object = pancreasSCE,
                                     img_id = "ImageNb", cell_id = "CellNb",
                                     colour_by = "CellType",
                                     return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(pancreasMasks))
  expect_identical(names(cur_out$images), names(pancreasMasks))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotCells(pancreasMasks, object = pancreasSCE,
                                     img_id = "ImageNb", cell_id = "CellNb",
                                     colour_by = "CellType", outline_by = "Area",
                                     return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(pancreasMasks))
  expect_identical(names(cur_out$images), names(pancreasMasks))
  expect_silent(plot(cur_out$images[[1]]))

  # Error
  expect_error(plotPixels(pancreasImages, return_images = "test"),
               regexp = "Invalid 'return_images' entry.")

})

test_that("plotting-param: legend can be set.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")

  # Works
  expect_silent(plotPixels(pancreasImages))
  expect_silent(plotPixels(pancreasImages,
                           colour_by = c("H3", "SMA", "INS",
                                         "CD38", "CD44")))
  expect_silent(plotPixels(pancreasImages,
                           colour_by = c("H3", "SMA", "INS",
                                         "CD38", "CD44"),
                           legend = NULL))
  expect_silent(plotPixels(pancreasImages,
                           colour_by = c("H3", "SMA", "INS",
                                         "CD38", "CD44"),
                           legend = list(colour_by.title.font = 4)))
  expect_silent(plotPixels(pancreasImages,
                           colour_by = c("H3", "SMA", "INS",
                                         "CD38", "CD44"),
                           legend = list(colour_by.title.cex = 0.5)))
  expect_silent(plotPixels(pancreasImages,
                           colour_by = c("H3", "SMA", "INS",
                                         "CD38", "CD44"),
                           legend = list(colour_by.labels.cex = 0.5)))


  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "SMA", "INS",
                                        "CD38", "CD44"),
                          legend = NULL))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "SMA", "INS",
                                        "CD38", "CD44")))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          legend = list(colour_by.legend.cex = 0.5)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          legend = list(colour_by.title.cex = 3)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          legend = list(colour_by.title.font = 4)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          legend = list(colour_by.labels.cex = 1)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "Area",
                          legend = list(outline_by.title.font = 4)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "Area",
                          legend = list(outline_by.title.cex = 4)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "Area",
                          legend = list(outline_by.labels.cex = 4)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "CellType",
                          legend = list(outline_by.legend.cex = 2)))

  # Margin
  expect_silent(plotPixels(pancreasImages))
  expect_silent(plotPixels(pancreasImages,
                           colour_by = c("H3", "SMA", "INS",
                                         "CD38", "CD44"),
                           legend = list(margin = 10)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "CellType",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "Area",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          outline_by = "CellType",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          outline_by = "Area",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "SMA", "INS",
                                        "CD38", "CD44"),
                          outline_by = "Area",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = pancreasMasks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "SMA", "INS",
                                        "CD38", "CD44"),
                          outline_by = "CellType",
                          legend = list(margin = 10)))


  # Error
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(test = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(colour_by.title.font = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(colour_by.labels.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(colour_by.legend.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(outline_by.title.font = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(outline_by.title.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(outline_by.labels.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(outline_by.legend.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages,
                          colour_by = "H3",
                          legend = list(margin = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
})

test_that("plotting-param: margin can be set.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")

  # Works
  expect_silent(plotPixels(pancreasImages))
  expect_silent(plotPixels(pancreasImages, margin = 2))
  expect_silent(plotPixels(pancreasImages, margin = 100))

  expect_silent(plotCells(pancreasMasks, margin = 2))
  expect_silent(plotCells(pancreasMasks, margin = 100))

  cur_images <- pancreasImages
  names(cur_images) <- paste(names(cur_images), 2, sep = "_")
  cur_images <- c(pancreasImages, cur_images)
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images, margin = 2))
  expect_silent(plotPixels(cur_images, margin = 100))

  # Error
  expect_error(plotPixels(pancreasImages, margin = "test"),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages, margin = c(1,2)),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages, margin = -1),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)

})

test_that("plotting-param: images can be plotted individually.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")

  # Works

  # Check return plot
  # check scale bar and names
  # check save_image

  expect_silent(plotPixels(pancreasImages))
  expect_silent(plotPixels(pancreasImages, margin = 2))
  expect_silent(plotPixels(pancreasImages, margin = 100))

  expect_silent(plotCells(pancreasMasks, margin = 2))
  expect_silent(plotCells(pancreasMasks, margin = 100))

  cur_images <- pancreasImages
  names(cur_images) <- paste(names(cur_images), 2, sep = "_")
  cur_images <- c(pancreasImages, cur_images)
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images, margin = 2))
  expect_silent(plotPixels(cur_images, margin = 100))

  # Error
  expect_error(plotPixels(pancreasImages, margin = "test"),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages, margin = c(1,2)),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)
  expect_error(plotPixels(pancreasImages, margin = -1),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)

})

