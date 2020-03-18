test_that("ImageList can be scaled.", {
  data("pancreasImages")

  # Works
  expect_silent(cur_images <- scaleImages(pancreasImages, 1))
  expect_identical(imageData(cur_images[[1]]), imageData(pancreasImages[[1]]))
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
  expect_equal(imageData(cur_images[[2]])[11, 1:10,1],
               c(0.297377274, 0.111573142, 0.080976692,
                 0.107373717, 0.100251775, 0.083805836,
                 0.044586895, 0.022574414, 0.007303341,
                 0.023436422), tolerance = 1e-06)
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

  expect_silent(cur_images <- normalize(pancreasImages, separateChannels = FALSE))
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))
  expect_equal(imageData(cur_images[[1]])[11, 1:10,1],
               c(2.921336e-02, 0.000000e+00, 5.414584e-18,
                 4.185544e-02, 7.561879e-02, 7.183567e-02,
                 2.977572e-02, 1.080541e-02, 7.331755e-03,
                 7.303341e-03), tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[11, 1:10,2],
               c(0.000000, 0.000000, 5.425223e-18,
                 0.000000, 0.000000, 0.000000, 0.000000,
                 1.096405e-02, 1.416071e-02, 0.000000),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[11, 1:10,1],
               c(0.297377274, 0.111573142, 0.080976692,
                 0.107373717, 0.100251775, 0.083805836,
                 0.044586895, 0.022574414, 0.007303341,
                 0.023436422), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               0.2897845, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               0.03007879, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               0.05940799, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,5]),
               0.4116903, tolerance = 1e-06)

  expect_silent(cur_images <- normalize(pancreasImages, separateChannels = TRUE,
                                        separateImages = TRUE))
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
  expect_equal(imageData(cur_images[[2]])[11, 1:10,1],
               c(0.45032775, 0.16895872, 0.12262555, 0.16259939,
                 0.15181441, 0.12690981, 0.06751934, 0.03418515,
                 0.01105968, 0.03549051), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,5]),
               1, tolerance = 1e-06)

  expect_silent(cur_images <- normalize(pancreasImages,
                                        ft = c(0.25, 0.75)))
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))
  expect_equal(imageData(cur_images[[1]])[11, 1:10,1],
               c(0.2646067, 0.2500000, 0.2500000,
                 0.2709277, 0.2878094, 0.2859178,
                 0.2648879, 0.2554027, 0.2536659,
                 0.2536517), tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[11, 1:10,2],
               c(0.2500000, 0.2500000, 0.2500000,
                 0.2500000, 0.2500000, 0.2500000,
                 0.2500000, 0.2689176, 0.2744332,
                 0.2500000), tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[11, 1:10,1],
               c(0.3986886, 0.3057866, 0.2904883, 0.3036869,
                 0.3001259, 0.2919029, 0.2722934, 0.2612872,
                 0.2536517, 0.2617182), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               0.75, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               0.75, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               0.432377, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               0.6676071, tolerance = 1e-06)
  expect_equal(min(imageData(cur_images[[1]])[,,5]),
               0.25, tolerance = 1e-06)
  expect_equal(min(imageData(cur_images[[1]])[,,1]),
               0.25, tolerance = 1e-06)
  expect_equal(min(imageData(cur_images[[1]])[,,2]),
               0.25, tolerance = 1e-06)
  expect_equal(min(imageData(cur_images[[1]])[,,3]),
               0.25, tolerance = 1e-06)
  expect_equal(min(imageData(cur_images[[1]])[,,4]),
               0.25, tolerance = 1e-06)
  expect_equal(min(imageData(cur_images[[1]])[,,5]),
               0.25, tolerance = 1e-06)

  expect_silent(cur_images <- normalize(pancreasImages,
                                        percentileRange = c(0.1, 0.99)))
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))
  expect_equal(imageData(cur_images[[1]])[11, 1:10,1],
               c(6.971395e-02, 0.000000, 0.000000,
                 1.099389e-01, 2.173681e-01, 2.053308e-01,
                 7.150328e-02, 1.114300e-02, 9.040732e-05,
                 0.000000), tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[11, 1:10,2],
               c(0.000000, 0.000000, 9.158603e-17,
                 0.000000, 0.000000, 0.000000, 0.000000,
                 1.850898e-01, 2.390544e-01, 0.000000), tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[11, 1:10,1],
               c(0.92296572, 0.33176870, 0.23441602,
                 0.31840685, 0.29574604, 0.24341788,
                 0.11862990, 0.04858995,
                 0.00000000,0.05133271), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,5]),
               1, tolerance = 1e-06)

  expect_silent(cur_images <- normalize(pancreasImages, percentileRange = NULL,
                                        inputRange = c(0, 20)))
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))
  expect_equal(imageData(cur_images[[1]])[11, 1:10,1],
               c(1.990020e-01, 0.000000, 3.688425e-17,
                 2.851200e-01, 5.151165e-01, 4.893458e-01,
                 2.028327e-01, 7.360668e-02, 4.994404e-02,
                 4.975049e-02), tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[11, 1:10,2],
               c(0.000000, 0.000000, 3.695671e-17,
                 0.000000, 0.000000, 0.000000,
                 0.000000, 7.468728e-02, 9.646304e-02,
                 0.000000), tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[11, 1:10,1],
               c(1.00000000, 0.76003823, 0.55161467,
                 0.73143167, 0.68291688, 0.57088685,
                 0.30372672, 0.15377731, 0.04975049,
                 0.15964932), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               0.2048972, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               0.4046883, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,5]),
               1, tolerance = 1e-06)

  # Test other combinations
  expect_silent(cur_images <- normalize(pancreasImages, separateChannels = FALSE,
                                        percentileRange = NULL,
                                        inputRange = c(0, 20)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))
  expect_silent(cur_images <- normalize(pancreasImages, separateChannels = FALSE,
                                        separateImages = FALSE, percentileRange = NULL,
                                        inputRange = c(0, 20)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))
  expect_silent(cur_images <- normalize(pancreasImages, separateChannels = FALSE,
                                        separateImages = FALSE,
                                        percentileRange = c(0.1, 0.90)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))
  expect_silent(cur_images <- normalize(pancreasImages, separateChannels = FALSE,
                                        separateImages = TRUE,
                                        percentileRange = c(0.1, 0.90)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "SMA")))

  # Error
  expect_error(normalize(pancreasImages, separateChannels = "test"),
               regexp = "'separateChannels' only takes TRUE or FALSE.")
  expect_error(normalize(pancreasImages, separateImages = "test"),
               regexp = "'separateImages' only takes TRUE or FALSE.")
  expect_error(normalize(pancreasImages, percentileRange = "test"),
               regexp = "'percentileRange' takes two numeric values indicating \nthe lower and upper percentile for clipping",
               fixed = TRUE)
  expect_error(normalize(pancreasImages, percentileRange = 1),
               regexp = "'percentileRange' takes two numeric values indicating \nthe lower and upper percentile for clipping",
               fixed = TRUE)
  expect_error(normalize(pancreasImages, percentileRange = c(0.1, 0.2)),
               regexp = "Minimum and maximum value for the indicated percentiles are identical.",
               fixed = TRUE)

})
