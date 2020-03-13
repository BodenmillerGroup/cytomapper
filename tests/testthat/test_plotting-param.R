test_that("plotting-param: scale_bar can be set.", {
  data("pancreasSCE")
  data("pancreasMasks")

  cur_images <- pancreasMasks
  setImages(cur_images, "A02_mask") <- cur_images[[1]][1:50, 1:50,,drop=FALSE]

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
  data("pancreasSCE")
  data("pancreasMasks")

  cur_images <- pancreasMasks
  setImages(cur_images, "A02_mask") <- cur_images[[1]][1:50, 1:50,,drop=FALSE]

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
  data("pancreasSCE")
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
  data("pancreasSCE")
  data("pancreasMasks")
  data("pancreasImages")

  # Works
  expect_silent(plotCells(pancreasMasks, background_colour = "white"))

  # Error
  expect_error(plotCells(pancreasMasks, background_colour = "test"),
               regexp = "'background_colour' not a valid colour.",
               fixed = TRUE)
})

