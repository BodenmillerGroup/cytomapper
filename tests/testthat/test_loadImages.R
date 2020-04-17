test_that("loadImages function reads in correct objects.", {
  path <- system.file("extdata", package = "cytomapper")
  single_file <- system.file("extdata/E34_mask.tiff",
                             package = "cytomapper")

  # Single file
  expect_silent(cur_file <- loadImages(single_file))
  expect_error(cur_file <- loadImages("test"))
  expect_s4_class(cur_file, "CytoImageList")

  expect_silent(cur_file <- scaleImages(cur_file, 2^16))
  expect_error(plotCells(cur_file), regexp = "Segmentation masks must only contain integer values.",
               fixed = TRUE)
  
  expect_silent(cur_file <- loadImages(single_file))
  expect_error(cur_file <- loadImages("test"))
  expect_s4_class(cur_file, "CytoImageList")
  
  expect_silent(cur_file <- scaleImages(cur_file, 2^16-1))

  expect_silent(plotCells(cur_file))

  # Pattern
  expect_error(cur_files <- loadImages(path, pattern = ".tiff"))
  expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff"))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  expect_silent(cur_files <- loadImages(path, pattern = "_imc"))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  expect_silent(cur_files <- loadImages(path, pattern = "[A-Z]01_imc"))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 1L)
  expect_error(cur_files <- loadImages(path, pattern = "test"))
  expect_silent(plotPixels(cur_files))

  # Multiple pattern
  expect_silent(cur_files <- loadImages(path, pattern = c("E34_imc", "J02_imc")))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 2L)
  expect_silent(cur_files <- loadImages(path, pattern = c("E34_imc", "E34_imc",
                                                          "J02_imc", "J02_imc")))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 2L)
  expect_silent(cur_files <- loadImages(path, pattern = c("E34_imc", "test")))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 1L)
  expect_error(cur_files <- loadImages(path, pattern = c("test1", "test")))

  # Multiple files
  multi_files <- list.files(system.file("extdata", package = "cytomapper"),
                            pattern = "mask.tiff", full.names = TRUE)
  expect_silent(cur_files <- loadImages(multi_files))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  expect_error(cur_files <- loadImages(c(multi_files, "test")))

  expect_silent(cur_files <- scaleImages(cur_files, (2^16)-1))
  expect_silent(plotCells(cur_files))
})
