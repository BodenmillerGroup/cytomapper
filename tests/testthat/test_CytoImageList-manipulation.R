test_that("CytoImageList can be scaled.", {
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

  image.list <- list.files(system.file("extdata", package = "cytomapper"),
             pattern = "mask.tiff", full.names = TRUE)
  cur_images <- loadImages(image.list)
  expect_equal(imageData(cur_images[[1]])[11, 1:2],
                   c(0.0001983673, 0.0001983673))

  expect_error(plotCells(cur_images),
               regexp = "Segmentation masks must only contain integer values.",
               fixed = TRUE)

  expect_silent(cur_images <- scaleImages(cur_images, (2^16)-1))
  expect_equal(imageData(cur_images[[1]])[11, 1:2],
               c(13, 13))
  expect_silent(plotCells(cur_images))

  # Error
  expect_error(scaleImages(cur_images, c(1,2)),
               regexp = "'value' must be a single numeric.",
               fixed = TRUE)
  expect_error(scaleImages(cur_images, "test"),
               regexp = "'value' must be a single numeric.",
               fixed = TRUE)

  })

test_that("CytoImageList can be normalized", {
  data("pancreasImages")

  # Works
  expect_silent(cur_images <- normalize(pancreasImages))
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99")))

  # Separate images
  # Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99")))

  expect_equal(imageData(cur_images[[1]])[1, 1:10,1],
               imageData(pancreasImages[[1]])[1, 1:10,1]/max(imageData(pancreasImages[[1]])[,,1]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,2],
               imageData(pancreasImages[[1]])[1, 1:10,2]/max(imageData(pancreasImages[[1]])[,,2]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,3],
               imageData(pancreasImages[[1]])[1, 1:10,3]/max(imageData(pancreasImages[[1]])[,,3]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,4],
               imageData(pancreasImages[[1]])[1, 1:10,4]/max(imageData(pancreasImages[[1]])[,,4]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,5],
               imageData(pancreasImages[[1]])[1, 1:10,5]/max(imageData(pancreasImages[[1]])[,,5]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,1],
               imageData(pancreasImages[[2]])[1, 1:10,1]/max(imageData(pancreasImages[[2]])[,,1]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,2],
               imageData(pancreasImages[[2]])[1, 1:10,2]/max(imageData(pancreasImages[[2]])[,,2]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,3],
               imageData(pancreasImages[[2]])[1, 1:10,3]/max(imageData(pancreasImages[[2]])[,,3]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,4],
               imageData(pancreasImages[[2]])[1, 1:10,4]/max(imageData(pancreasImages[[2]])[,,4]),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,5],
               imageData(pancreasImages[[2]])[1, 1:10,5]/max(imageData(pancreasImages[[2]])[,,5]),
               tolerance = 1e-06)

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
  expect_equal(max(imageData(cur_images[[2]])[,,1]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,2]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,3]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,4]),
               1, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,5]),
               1, tolerance = 1e-06)

  # Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = FALSE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(imageData(cur_images[[1]])[1, 1:10,1],
               imageData(pancreasImages[[1]])[1, 1:10,1]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,2],
               imageData(pancreasImages[[1]])[1, 1:10,2]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,3],
               imageData(pancreasImages[[1]])[1, 1:10,3]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,4],
               imageData(pancreasImages[[1]])[1, 1:10,4]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,5],
               imageData(pancreasImages[[1]])[1, 1:10,5]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,1],
               imageData(pancreasImages[[2]])[1, 1:10,1]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,2],
               imageData(pancreasImages[[2]])[1, 1:10,2]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,3],
               imageData(pancreasImages[[2]])[1, 1:10,3]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,4],
               imageData(pancreasImages[[2]])[1, 1:10,4]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,5],
               imageData(pancreasImages[[2]])[1, 1:10,5]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)

  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               max(imageData(pancreasImages[[1]])[,,1])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               max(imageData(pancreasImages[[1]])[,,2])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               max(imageData(pancreasImages[[1]])[,,3])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               max(imageData(pancreasImages[[1]])[,,4])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,5]),
               max(imageData(pancreasImages[[1]])[,,5])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,1]),
               max(imageData(pancreasImages[[2]])[,,1])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,2]),
               max(imageData(pancreasImages[[2]])[,,2])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,3]),
               max(imageData(pancreasImages[[2]])[,,3])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,4]),
               max(imageData(pancreasImages[[2]])[,,4])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,5]),
               max(imageData(pancreasImages[[2]])[,,5])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)

  # Not Separate images
  # Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = FALSE,
                                        separateChannels = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  cur_max <- sapply(pancreasImages, function(x){apply(x, 3, max)})
  cur_max <- as.numeric(apply(cur_max, 1, max))

  expect_equal(imageData(cur_images[[1]])[1, 1:10,1],
               imageData(pancreasImages[[1]])[1, 1:10,1]/cur_max[1],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,2],
               imageData(pancreasImages[[1]])[1, 1:10,2]/cur_max[2],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,3],
               imageData(pancreasImages[[1]])[1, 1:10,3]/cur_max[3],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,4],
               imageData(pancreasImages[[1]])[1, 1:10,4]/cur_max[4],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,5],
               imageData(pancreasImages[[1]])[1, 1:10,5]/cur_max[5],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,1],
               imageData(pancreasImages[[2]])[1, 1:10,1]/cur_max[1],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,2],
               imageData(pancreasImages[[2]])[1, 1:10,2]/cur_max[2],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,3],
               imageData(pancreasImages[[2]])[1, 1:10,3]/cur_max[3],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,4],
               imageData(pancreasImages[[2]])[1, 1:10,4]/cur_max[4],
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,5],
               imageData(pancreasImages[[2]])[1, 1:10,5]/cur_max[5],
               tolerance = 1e-06)

  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               max(imageData(pancreasImages[[1]])[,,1])/cur_max[1], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               max(imageData(pancreasImages[[1]])[,,2])/cur_max[2], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               max(imageData(pancreasImages[[1]])[,,3])/cur_max[3], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               max(imageData(pancreasImages[[1]])[,,4])/cur_max[4], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,5]),
               max(imageData(pancreasImages[[1]])[,,5])/cur_max[5], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,1]),
               max(imageData(pancreasImages[[2]])[,,1])/cur_max[1], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,2]),
               max(imageData(pancreasImages[[2]])[,,2])/cur_max[2], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,3]),
               max(imageData(pancreasImages[[2]])[,,3])/cur_max[3], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,4]),
               max(imageData(pancreasImages[[2]])[,,4])/cur_max[4], tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,5]),
               max(imageData(pancreasImages[[2]])[,,5])/cur_max[5], tolerance = 1e-06)

  # Not Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = FALSE,
                                        separateChannels = FALSE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  cur_max <- sapply(pancreasImages, max)
  cur_max <- as.numeric(max(cur_max))

  expect_equal(imageData(cur_images[[1]])[1, 1:10,1],
               imageData(pancreasImages[[1]])[1, 1:10,1]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,2],
               imageData(pancreasImages[[1]])[1, 1:10,2]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,3],
               imageData(pancreasImages[[1]])[1, 1:10,3]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,4],
               imageData(pancreasImages[[1]])[1, 1:10,4]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[1]])[1, 1:10,5],
               imageData(pancreasImages[[1]])[1, 1:10,5]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,1],
               imageData(pancreasImages[[2]])[1, 1:10,1]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,2],
               imageData(pancreasImages[[2]])[1, 1:10,2]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,3],
               imageData(pancreasImages[[2]])[1, 1:10,3]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,4],
               imageData(pancreasImages[[2]])[1, 1:10,4]/cur_max,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images[[2]])[1, 1:10,5],
               imageData(pancreasImages[[2]])[1, 1:10,5]/cur_max,
               tolerance = 1e-06)

  expect_equal(max(imageData(cur_images[[1]])[,,1]),
               max(imageData(pancreasImages[[1]])[,,1])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,2]),
               max(imageData(pancreasImages[[1]])[,,2])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,3]),
               max(imageData(pancreasImages[[1]])[,,3])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,4]),
               max(imageData(pancreasImages[[1]])[,,4])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[1]])[,,5]),
               max(imageData(pancreasImages[[1]])[,,5])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,1]),
               max(imageData(pancreasImages[[2]])[,,1])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,2]),
               max(imageData(pancreasImages[[2]])[,,2])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,3]),
               max(imageData(pancreasImages[[2]])[,,3])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,4]),
               max(imageData(pancreasImages[[2]])[,,4])/cur_max, tolerance = 1e-06)
  expect_equal(max(imageData(cur_images[[2]])[,,5]),
               max(imageData(pancreasImages[[2]])[,,5])/cur_max, tolerance = 1e-06)


  # Setting the inputRange
  # Separate images
  # Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = TRUE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = TRUE,
                                        separateChannels = TRUE, inputRange = c(0, 0.9)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(imageData(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = FALSE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = TRUE,
                                         separateChannels = FALSE, inputRange = c(0, 0.9)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(imageData(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Not Separate images
  # Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = FALSE,
                                        separateChannels = TRUE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = FALSE,
                                         separateChannels = TRUE, inputRange = c(0, 0.9)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(imageData(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = FALSE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = TRUE,
                                         separateChannels = FALSE, inputRange = c(0, 0.9)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(imageData(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Not Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = FALSE,
                                        separateChannels = FALSE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = FALSE,
                                         separateChannels = FALSE, inputRange = c(0, 0.9)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(imageData(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(imageData(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Setting ft
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = TRUE, ft = c(0, 2)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = FALSE, ft = c(0, 2)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_silent(cur_images <- normalize(pancreasImages, separateImages = FALSE,
                                        separateChannels = TRUE, ft = c(0, 2)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_silent(cur_images <- normalize(pancreasImages, separateImages = FALSE,
                                        separateChannels = FALSE, ft = c(0, 2)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))


  # Error
  expect_error(normalize(pancreasImages, separateChannels = "test"),
               regexp = "'separateChannels' only takes TRUE or FALSE.")
  expect_error(normalize(pancreasImages, separateImages = "test"),
               regexp = "'separateImages' only takes TRUE or FALSE.")

})
