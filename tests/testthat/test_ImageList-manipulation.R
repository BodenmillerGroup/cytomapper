test_that("ImageList can be scaled.", {
  data("pancreasImages")

  # Works
  expect_silent(cur_images <- scaleImages(pancreasImages, 1))
  expect_identical(cur_images, pancreasImages)
  expect_equal(imageData(cur_images[[1]])[11, 1:2,1],
               c(3.980039, 0.000000), tolerance = 1e-06)

  expect_silent(cur_images <- scaleImages(cur_images, 2))
  expect_equal(imageData(cur_images[[1]])[11, 1:2,1],
               c(7.960078, 0.000000), tolerance = 1e-06)

  expect_silent(plotPixels(cur_images))

  image.list <- list.files(system.file("extdata", package = "SingleCellMapper"),
             pattern = "mask.tiff", full.names = TRUE)
  cur_images <- loadImages(image.list)
  expect_equal(imageData(cur_images[[1]])[11, 1:2],
                   c(0.0008087282, 0.0008087282))

  expect_error(plotCells(cur_images),
               regexp = "Segmentation masks must only contain integer values.",
               fixed = TRUE)

  expect_silent(cur_images <- scaleImages(cur_images, (2^16)-1))
  expect_equal(imageData(cur_images[[1]])[11, 1:2],
               c(53, 53))
  expect_silent(plotCells(cur_images))

  # Error
  expect_error(scaleImages(cur_images, c(1,2)),
               regexp = "'value' must be a single numeric.",
               fixed = TRUE)
  expect_error(scaleImages(cur_images, "test"),
               regexp = "'value' must be a single numeric.",
               fixed = TRUE)

  })

test_that("ImageList can be normalized", {
  data("pancreasImages")

  # Works
  expect_silent(cur_images <- normalize(pancreasImages))
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))
  expect_equal(imageData(cur_images[[1]])[11, 1:10,1],
               c(2.921336e-02, 0.000000e+00, 5.414584e-18,
                 4.185544e-02, 7.561879e-02, 7.183567e-02,
                 2.977572e-02, 1.080541e-02, 7.331755e-03,
                 7.303341e-03), tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[11, 1:10,2],
               c(0.000000, 0.000000, 1.872158e-17,
                 0.000000, 0.000000, 0.000000, 0.000000,
                 3.783517e-02, 4.886636e-02, 0.000000), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               0.364754, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               0.8352143, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,5]),
               1, tolerance = 1e-06)

})
