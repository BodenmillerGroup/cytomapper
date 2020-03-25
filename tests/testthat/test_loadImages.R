test_that("loadImages function reads in correct objects.", {
  path <- system.file("extdata", package = "cytomapper")
  single_file <- system.file("extdata/A02_mask.tiff",
                             package = "cytomapper")

  # Single file
  expect_silent(cur_file <- loadImages(single_file))
  expect_error(cur_file <- loadImages("test"))
  expect_s4_class(cur_file, "Image")

  expect_silent(cur_file <- EBImage::normalize(cur_file))
  expect_silent(plot(cur_file))

  # Pattern
  expect_error(cur_files <- loadImages(path, pattern = ".tiff"))
  expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff"))
  expect_silent(cur_files <- loadImages(path, pattern = "_imc"))
  expect_silent(cur_files <- loadImages(path, pattern = "[A-Z]01_imc"))
  expect_error(cur_files <- loadImages(path, pattern = "test"))

  # Fix this!
  expect_silent(plotPixels(cur_files))

  # Multiple pattern
  expect_silent(cur_files <- loadImages(path, pattern = c("A02_imc", "F01_imc")))
  expect_silent(cur_files <- loadImages(path, pattern = c("A02_imc", "A02_imc",
                                                          "F01_imc", "F01_imc")))
  expect_silent(cur_files <- loadImages(path, pattern = c("A02_imc", "test")))
  expect_error(cur_files <- loadImages(path, pattern = c("test1", "test")))


  # Multiple files
  multi_files <- list.files(system.file("extdata", package = "cytomapper"),
                            pattern = "mask.tiff", full.names = TRUE)
  expect_silent(cur_files <- loadImages(multi_files))
  expect_error(cur_files <- loadImages(c(multi_files, "test")))

  expect_silent(cur_files <- scaleImages(cur_files, (2^16)-1))
  expect_silent(plotCells(cur_files))
})
