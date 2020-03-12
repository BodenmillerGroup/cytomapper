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
                          scale_bar = list(text = "test")))
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

