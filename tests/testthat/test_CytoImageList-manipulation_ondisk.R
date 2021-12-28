test_that("On disk: CytoImageList can be scaled.", {
  data("pancreasImages")

  cur_path <- tempdir()
  on.exit(unlink(cur_path))
    
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
  cur_size <- file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]

  # Works - single numeric
  expect_silent(cur_Images_2 <- scaleImages(cur_Images, 1))
  expect_s4_class(cur_Images_2[[1]], "DelayedArray")
  expect_s4_class(cur_Images_2[[2]], "DelayedArray")
  expect_s4_class(cur_Images_2[[3]], "DelayedArray")
  expect_identical(as.array(cur_Images_2[[1]]), as.array(cur_Images[[1]]))

  expect_silent(cur_Images_2 <- scaleImages(cur_Images, 2))
  expect_identical(as.array(cur_Images_2[[1]]), as.array(cur_Images[[1]]) * 2)

  image.list <- list.files(system.file("extdata", package = "cytomapper"),
             pattern = "mask.tiff", full.names = TRUE)
  cur_Images_2 <- loadImages(image.list, on_disk = TRUE, h5FilesPath = cur_path)
  expect_equal(cur_Images_2[[1]][11, 1:2],
                   c(0.01257343, 0.01257343))

  expect_error(plotCells(cur_Images_2),
               regexp = "Segmentation masks must only contain integer values.",
               fixed = TRUE)

  expect_silent(cur_Images_2 <- scaleImages(cur_Images_2, (2^16)-1))
  expect_identical(cur_Images_2[[1]][11, 1:2],
               c(824, 824))
  
  # Works - numeric vector
  expect_silent(cur_images <- scaleImages(cur_Images, c(1, 1, 1)))
  expect_identical(as.array(imageData(cur_images[[1]])), as.array(imageData(cur_Images[[1]])))
  expect_identical(as.array(imageData(cur_images[[2]])), as.array(imageData(cur_Images[[2]])))
  expect_identical(as.array(imageData(cur_images[[3]])), as.array(imageData(cur_Images[[3]])))
  
  expect_silent(cur_images <- scaleImages(cur_Images, c(2, 3, 4)))
  expect_identical(as.array(imageData(cur_images[[1]])),
                   as.array(imageData(cur_Images[[1]])) * 2)
  expect_identical(as.array(imageData(cur_images[[2]])),
                   as.array(imageData(cur_Images[[2]])) * 3)
  expect_identical(as.array(imageData(cur_images[[3]])),
                   as.array(imageData(cur_Images[[3]])) * 4)
  
  expect_silent(plotPixels(cur_images))

  # Error
  expect_error(scaleImages(cur_Images, c(1,2)),
               regexp = "'value' must either be a single numeric or of the same length as 'object'.",
               fixed = TRUE)
  expect_error(scaleImages(cur_Images, "test"),
               regexp = "'value' must be numeric.",
               fixed = TRUE)
  expect_error(scaleImages(cur_Images, c(1, "test")),
               regexp = "'value' must be numeric.",
               fixed = TRUE)
  expect_error(scaleImages(cur_Images, c(1, 2, "test")),
               regexp = "'value' must be numeric.",
               fixed = TRUE)
  expect_error(scaleImages(cur_Images, c(1, 2, 3, 4)),
               regexp = "'value' must either be a single numeric or of the same length as 'object'.",
               fixed = TRUE)
  
  expect_equal(cur_size, file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"])
})

test_that("On disk: CytoImageList can be normalized", {
  data("pancreasImages")
    
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
    
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
  cur_size <- file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]

  # Works
  expect_silent(cur_images <- normalize(cur_Images))
  
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99")))
  
  expect_s4_class(cur_images$E34_imc, "DelayedArray")
  expect_s4_class(cur_images$G01_imc, "DelayedArray")
  expect_s4_class(cur_images$J02_imc, "DelayedArray")
  
  cur_size_2 <- file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]
  expect_gt(cur_size_2, cur_size)
  
  expect_equal(object.size(cur_images), object.size(cur_Images), tolerance = 0.01)
  
  # Check if second layer was created
  expect_true("E34_imc_norm" %in% h5ls(path(cur_images$E34_imc@seed))$name)
  expect_true(".E34_imc_norm_dimnames" %in% h5ls(path(cur_images$E34_imc@seed))$name)
  
  # Test if it can be renormalized again
  expect_silent(cur_images <- normalize(cur_Images))
  expect_s4_class(cur_images$E34_imc, "DelayedArray")
  expect_s4_class(cur_images$G01_imc, "DelayedArray")
  expect_s4_class(cur_images$J02_imc, "DelayedArray")
  
  expect_equal(object.size(cur_images), object.size(cur_Images), tolerance = 0.01)
  
  # Separate images
  # Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = TRUE,
                                        separateChannels = TRUE))
  expect_silent(plotPixels(cur_images,
                          colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(as.array(cur_images[[1]])[1, 1:10,1],
               imageData(pancreasImages[[1]])[1, 1:10,1]/max(imageData(pancreasImages[[1]])[,,1]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,2],
               imageData(pancreasImages[[1]])[1, 1:10,2]/max(imageData(pancreasImages[[1]])[,,2]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,3],
               imageData(pancreasImages[[1]])[1, 1:10,3]/max(imageData(pancreasImages[[1]])[,,3]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,4],
               imageData(pancreasImages[[1]])[1, 1:10,4]/max(imageData(pancreasImages[[1]])[,,4]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,5],
               imageData(pancreasImages[[1]])[1, 1:10,5]/max(imageData(pancreasImages[[1]])[,,5]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,1],
               imageData(pancreasImages[[2]])[1, 1:10,1]/max(imageData(pancreasImages[[2]])[,,1]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,2],
               imageData(pancreasImages[[2]])[1, 1:10,2]/max(imageData(pancreasImages[[2]])[,,2]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,3],
               imageData(pancreasImages[[2]])[1, 1:10,3]/max(imageData(pancreasImages[[2]])[,,3]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,4],
               imageData(pancreasImages[[2]])[1, 1:10,4]/max(imageData(pancreasImages[[2]])[,,4]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,5],
               imageData(pancreasImages[[2]])[1, 1:10,5]/max(imageData(pancreasImages[[2]])[,,5]),
               tolerance = 1e-06)

  expect_equal(max(as.array(cur_images[[1]])[,,1]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,2]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,3]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,4]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,5]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,1]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,2]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,3]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,4]),
               1, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,5]),
               1, tolerance = 1e-06)

  # Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = TRUE,
                                        separateChannels = FALSE))
  expect_silent(plotPixels(cur_images,
                          colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(as.array(cur_images[[1]])[1, 1:10,1],
               imageData(pancreasImages[[1]])[1, 1:10,1]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,2],
               imageData(pancreasImages[[1]])[1, 1:10,2]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,3],
               imageData(pancreasImages[[1]])[1, 1:10,3]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,4],
               imageData(pancreasImages[[1]])[1, 1:10,4]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,5],
               imageData(pancreasImages[[1]])[1, 1:10,5]/max(imageData(pancreasImages[[1]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,1],
               imageData(pancreasImages[[2]])[1, 1:10,1]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,2],
               imageData(pancreasImages[[2]])[1, 1:10,2]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,3],
               imageData(pancreasImages[[2]])[1, 1:10,3]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,4],
               imageData(pancreasImages[[2]])[1, 1:10,4]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,5],
               imageData(pancreasImages[[2]])[1, 1:10,5]/max(imageData(pancreasImages[[2]])),
               tolerance = 1e-06)

  expect_equal(max(as.array(cur_images[[1]])[,,1]),
               max(imageData(pancreasImages[[1]])[,,1])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,2]),
               max(imageData(pancreasImages[[1]])[,,2])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,3]),
               max(imageData(pancreasImages[[1]])[,,3])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,4]),
               max(imageData(pancreasImages[[1]])[,,4])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,5]),
               max(imageData(pancreasImages[[1]])[,,5])/max(imageData(pancreasImages[[1]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,1]),
               max(imageData(pancreasImages[[2]])[,,1])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,2]),
               max(imageData(pancreasImages[[2]])[,,2])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,3]),
               max(imageData(pancreasImages[[2]])[,,3])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,4]),
               max(imageData(pancreasImages[[2]])[,,4])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,5]),
               max(imageData(pancreasImages[[2]])[,,5])/max(imageData(pancreasImages[[2]])), tolerance = 1e-06)

  # Not Separate images
  # Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = FALSE,
                                        separateChannels = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  cur_max <- sapply(pancreasImages, function(x){apply(x, 3, max)})
  cur_max <- as.numeric(apply(cur_max, 1, max))

  expect_equal(as.array(cur_images[[1]])[1, 1:10,1],
               imageData(pancreasImages[[1]])[1, 1:10,1]/cur_max[1],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,2],
               imageData(pancreasImages[[1]])[1, 1:10,2]/cur_max[2],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,3],
               imageData(pancreasImages[[1]])[1, 1:10,3]/cur_max[3],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,4],
               imageData(pancreasImages[[1]])[1, 1:10,4]/cur_max[4],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,5],
               imageData(pancreasImages[[1]])[1, 1:10,5]/cur_max[5],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,1],
               imageData(pancreasImages[[2]])[1, 1:10,1]/cur_max[1],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,2],
               imageData(pancreasImages[[2]])[1, 1:10,2]/cur_max[2],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,3],
               imageData(pancreasImages[[2]])[1, 1:10,3]/cur_max[3],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,4],
               imageData(pancreasImages[[2]])[1, 1:10,4]/cur_max[4],
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,5],
               imageData(pancreasImages[[2]])[1, 1:10,5]/cur_max[5],
               tolerance = 1e-06)

  expect_equal(max(as.array(cur_images[[1]])[,,1]),
               max(imageData(pancreasImages[[1]])[,,1])/cur_max[1], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,2]),
               max(imageData(pancreasImages[[1]])[,,2])/cur_max[2], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,3]),
               max(imageData(pancreasImages[[1]])[,,3])/cur_max[3], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,4]),
               max(imageData(pancreasImages[[1]])[,,4])/cur_max[4], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,5]),
               max(imageData(pancreasImages[[1]])[,,5])/cur_max[5], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,1]),
               max(imageData(pancreasImages[[2]])[,,1])/cur_max[1], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,2]),
               max(imageData(pancreasImages[[2]])[,,2])/cur_max[2], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,3]),
               max(imageData(pancreasImages[[2]])[,,3])/cur_max[3], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,4]),
               max(imageData(pancreasImages[[2]])[,,4])/cur_max[4], tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,5]),
               max(imageData(pancreasImages[[2]])[,,5])/cur_max[5], tolerance = 1e-06)
  
  # Single frame
  cur_img1 <- cur_Images[[1]][,,1]
  cur_img2 <- cur_Images[[2]][,,1]
  cur_img <- CytoImageList(img1 = cur_img1, img2 = cur_img2, on_disk = TRUE, 
                           h5FilesPath = cur_path)
  channelNames(cur_img) <- "H3"
  
  expect_silent(cur_images <- normalize(cur_img, separateImages = FALSE,
                                        separateChannels = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = "H3"))
  
  cur_max <- max(max(cur_img1), max(cur_img2))
  expect_equal(max(as.array(cur_images[[1]])[,,1]),
               max(imageData(pancreasImages[[1]])[,,1])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,1]),
               max(imageData(pancreasImages[[2]])[,,1])/cur_max, tolerance = 1e-06)
  
  # Not Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = FALSE,
                                        separateChannels = FALSE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  cur_max <- sapply(pancreasImages, max)
  cur_max <- as.numeric(max(cur_max))

  expect_equal(as.array(cur_images[[1]])[1, 1:10,1],
               imageData(pancreasImages[[1]])[1, 1:10,1]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,2],
               imageData(pancreasImages[[1]])[1, 1:10,2]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,3],
               imageData(pancreasImages[[1]])[1, 1:10,3]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,4],
               imageData(pancreasImages[[1]])[1, 1:10,4]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,5],
               imageData(pancreasImages[[1]])[1, 1:10,5]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,1],
               imageData(pancreasImages[[2]])[1, 1:10,1]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,2],
               imageData(pancreasImages[[2]])[1, 1:10,2]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,3],
               imageData(pancreasImages[[2]])[1, 1:10,3]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,4],
               imageData(pancreasImages[[2]])[1, 1:10,4]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,5],
               imageData(pancreasImages[[2]])[1, 1:10,5]/cur_max,
               tolerance = 1e-06)

  expect_equal(max(as.array(cur_images[[1]])[,,1]),
               max(imageData(pancreasImages[[1]])[,,1])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,2]),
               max(imageData(pancreasImages[[1]])[,,2])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,3]),
               max(imageData(pancreasImages[[1]])[,,3])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,4]),
               max(imageData(pancreasImages[[1]])[,,4])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[1]])[,,5]),
               max(imageData(pancreasImages[[1]])[,,5])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,1]),
               max(imageData(pancreasImages[[2]])[,,1])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,2]),
               max(imageData(pancreasImages[[2]])[,,2])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,3]),
               max(imageData(pancreasImages[[2]])[,,3])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,4]),
               max(imageData(pancreasImages[[2]])[,,4])/cur_max, tolerance = 1e-06)
  expect_equal(max(as.array(cur_images[[2]])[,,5]),
               max(imageData(pancreasImages[[2]])[,,5])/cur_max, tolerance = 1e-06)


  # Setting the inputRange
  # Separate images
  # Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = TRUE,
                                        separateChannels = TRUE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = TRUE,
                                        separateChannels = TRUE, inputRange = c(0, 0.9)))
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(as.array(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = TRUE,
                                        separateChannels = FALSE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = TRUE,
                                         separateChannels = FALSE, inputRange = c(0, 0.9)))
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = FALSE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(as.array(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Not Separate images
  # Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = FALSE,
                                        separateChannels = TRUE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = FALSE,
                                         separateChannels = TRUE, inputRange = c(0, 0.9)))
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = FALSE,
                                        separateChannels = TRUE))
  expect_silent(plotPixels(cur_images,
                          colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(as.array(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = TRUE,
                                        separateChannels = FALSE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = TRUE,
                                         separateChannels = FALSE, inputRange = c(0, 0.9)))
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = TRUE,
                                        separateChannels = FALSE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(as.array(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Not Separate images
  # Not Separate channels
  expect_silent(cur_images <- normalize(cur_Images, separateImages = FALSE,
                                        separateChannels = FALSE))
  expect_silent(cur_images2 <- normalize(cur_images, separateImages = FALSE,
                                         separateChannels = FALSE, inputRange = c(0, 0.9)))
  expect_silent(cur_images <- normalize(pancreasImages, separateImages = FALSE,
                                        separateChannels = FALSE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images2,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_equal(as.array(cur_images2[[1]])[1, 1:10,1],
               imageData(cur_images[[1]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,2],
               imageData(cur_images[[1]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,3],
               imageData(cur_images[[1]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,4],
               imageData(cur_images[[1]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[1]])[1, 1:10,5],
               imageData(cur_images[[1]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,1],
               imageData(cur_images[[2]])[1, 1:10,1]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,2],
               imageData(cur_images[[2]])[1, 1:10,2]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,3],
               imageData(cur_images[[2]])[1, 1:10,3]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,4],
               imageData(cur_images[[2]])[1, 1:10,4]/0.9,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images2[[2]])[1, 1:10,5],
               imageData(cur_images[[2]])[1, 1:10,5]/0.9,
               tolerance = 1e-06)

  # Setting ft
  expect_silent(cur_images <- normalize(cur_Images, separateImages = TRUE,
                                        separateChannels = TRUE, ft = c(0, 2)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_silent(cur_images <- normalize(cur_Images, separateImages = TRUE,
                                        separateChannels = FALSE, ft = c(0, 2)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_silent(cur_images <- normalize(cur_Images, separateImages = FALSE,
                                        separateChannels = TRUE, ft = c(0, 2)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))

  expect_silent(cur_images <- normalize(cur_Images, separateImages = FALSE,
                                        separateChannels = FALSE, ft = c(0, 2)))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = TRUE))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99"), scale = FALSE))
  
  # Channel-wise normalization
  # Separate images
  expect_silent(cur_images <- normalize(cur_Images, separateImages = TRUE,
                                        inputRange = list(H3 = c(0,50), CD99 = c(0,70))))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("H3", "CD99", "PIN")))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99", "PIN")))
  
  expect_equal(as.array(cur_images[[1]])[1, 1:10,"H3"],
               imageData(pancreasImages[[1]])[1, 1:10,"H3"]/50,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,"CD99"],
               imageData(pancreasImages[[1]])[1, 1:10,"CD99"]/70,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,"PIN"],
               imageData(pancreasImages[[1]])[1, 1:10,"PIN"]/max(imageData(pancreasImages[[1]])[,,"PIN"]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,"H3"],
               imageData(pancreasImages[[2]])[1, 1:10,"H3"]/50,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,"CD99"],
               imageData(pancreasImages[[2]])[1, 1:10,"CD99"]/70,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,"PIN"],
               imageData(pancreasImages[[2]])[1, 1:10,"PIN"]/max(imageData(pancreasImages[[1]])[,,"PIN"]),
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[3]])[1, 1:10,"H3"],
               imageData(pancreasImages[[3]])[1, 1:10,"H3"]/50,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[3]])[1, 1:10,"CD99"],
               imageData(pancreasImages[[3]])[1, 1:10,"CD99"]/70,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[3]])[1, 1:10,"PIN"],
               imageData(pancreasImages[[3]])[1, 1:10,"PIN"]/max(imageData(pancreasImages[[1]])[,,"PIN"]),
               tolerance = 1e-06)
  
  # Not Separate images
  expect_silent(cur_images <- normalize(cur_Images, separateImages = FALSE,
                                        inputRange = list(H3 = c(0,50), CD99 = c(0,70))))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("H3", "CD99", "PIN")))
  expect_silent(plotPixels(cur_images,
                           colour_by = c("H3", "CD99", "PIN")))
  
  expect_equal(as.array(cur_images[[1]])[1, 1:10,"H3"],
               imageData(pancreasImages[[1]])[1, 1:10,"H3"]/50,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[1]])[1, 1:10,"CD99"],
               imageData(pancreasImages[[1]])[1, 1:10,"CD99"]/70,
               tolerance = 1e-06)
  cur_max <- max(c(max(imageData(pancreasImages[[1]])[,,"PIN"]),
                   max(imageData(pancreasImages[[2]])[,,"PIN"]),
                   max(imageData(pancreasImages[[3]])[,,"PIN"])))
  expect_equal(as.array(cur_images[[1]])[1, 1:10,"PIN"],
               imageData(pancreasImages[[1]])[1, 1:10,"PIN"]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,"H3"],
               imageData(pancreasImages[[2]])[1, 1:10,"H3"]/50,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,"CD99"],
               imageData(pancreasImages[[2]])[1, 1:10,"CD99"]/70,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[2]])[1, 1:10,"PIN"],
               imageData(pancreasImages[[2]])[1, 1:10,"PIN"]/cur_max,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[3]])[1, 1:10,"H3"],
               imageData(pancreasImages[[3]])[1, 1:10,"H3"]/50,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[3]])[1, 1:10,"CD99"],
               imageData(pancreasImages[[3]])[1, 1:10,"CD99"]/70,
               tolerance = 1e-06)
  expect_equal(as.array(cur_images[[3]])[1, 1:10,"PIN"],
               imageData(pancreasImages[[3]])[1, 1:10,"PIN"]/cur_max,
               tolerance = 1e-06)

  # Error
  expect_error(normalize(cur_Images, separateChannels = "test"),
               regexp = "'separateChannels' only takes TRUE or FALSE.")
  expect_error(normalize(cur_Images, separateImages = "test"),
               regexp = "'separateImages' only takes TRUE or FALSE.")
  expect_error(normalize(cur_Images, inputRange = "test"),
               regexp = "'inputRange' takes a vector of length 2, a list or NULL.")
  expect_error(normalize(cur_Images, inputRange = 2),
               regexp = "'inputRange' takes a vector of length 2, a list or NULL.")
  cur_images <- cur_Images
  channelNames(cur_images) <- NULL
  expect_error(normalize(cur_images, inputRange = list(H3 = c(0, 100), CDH = c(0, 20))),
               regexp = "Please set the 'channelNames' of the CytoImageList object.")
  expect_error(normalize(pancreasImages, inputRange = list(test = c(0, 100), CDH = c(0, 20))),
               regexp = "The names of 'inputRange' should correspond to the'channelNames' of the CytoImageList object.")

  # Check if overwriting works
  expect_silent(cur_images <- normalize(cur_Images, overwrite = TRUE))
  expect_s4_class(cur_images$E34_imc, "DelayedArray")
  expect_s4_class(cur_images$G01_imc, "DelayedArray")
  expect_s4_class(cur_images$J02_imc, "DelayedArray")
  
  # Check if second layer was created
  expect_true("E34_imc_norm" %in% h5ls(path(cur_images$E34_imc@seed))$name)
  expect_true(".E34_imc_norm_dimnames" %in% h5ls(path(cur_images$E34_imc@seed))$name)
  expect_false("E34_imc" %in% h5ls(path(cur_images$E34_imc@seed))$name)
  expect_false(".E34_imc_dimnames" %in% h5ls(path(cur_images$E34_imc@seed))$name)
})

test_that("On disk: HDF5 handling works", {
    cur_array <- array(data = rnorm(3000), dim = c(100, 100, 3)) 
    cur_array_2 <- array(data = runif(3000), dim = c(100, 100, 3)) 
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_file <- paste0(cur_path, "/test.h5")
    
    cur_hdf5 <- writeHDF5Array(x = cur_array, filepath = cur_file,  name = "test")
    
    expect_equal(h5ls(cur_file)$name, "test")
    
    expect_silent(test <- .add_h5(cur_obj = cur_hdf5, new_obj = cur_array_2, 
                                  overwrite = FALSE, suffix = "_norm"))
    
    expect_equal(h5ls(cur_file)$name, c("test", "test_norm"))
    expect_equal(as.array(test), cur_array_2)
    
    expect_silent(test <- .add_h5(cur_obj = cur_hdf5, new_obj = cur_array_2, 
                                  overwrite = TRUE, suffix = "_norm"))
    expect_equal(h5ls(cur_file)$name, "test_norm")
    expect_equal(as.array(test), cur_array_2)
    
})
