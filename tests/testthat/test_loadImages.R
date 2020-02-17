test_that("loadImages function reads in correct objects.", {
  path <- system.file("extdata", package = "SingleCellMapper")
  single_file <- system.file("extdata/A02_imc.tiff",
                             package = "SingleCellMapper")

  # Single file
  expect_silent(cur_file <- loadImages(single_file))
  expect_error(cur_file <- loadImages("test"))
  expect_s4_class(cur_file, "Image")

  # Pattern
  expect_error(cur_files <- loadImages(path, pattern = ".tiff"))
  expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff"))
  expect_silent(cur_files <- loadImages(path, pattern = "_imc"))
  expect_silent(cur_files <- loadImages(path, pattern = "[A-Z]01_imc"))
  expect_error(cur_files <- loadImages(path, pattern = "test"))

  # Multiple pattern
  expect_silent(cur_files <- loadImages(path, pattern = c("A02_imc", "F01_imc")))
  expect_silent(cur_files <- loadImages(path, pattern = c("A02_imc", "A02_imc",
                                                          "F01_imc", "F01_imc")))
  expect_silent(cur_files <- loadImages(path, pattern = c("A02_imc", "test")))
  expect_error(cur_files <- loadImages(path, pattern = c("test1", "test")))

  # Multiple files
  multi_files <- list.files(system.file("extdata", package = "SingleCellMapper"),
                            pattern = "imc.tiff", full.names = TRUE)
  expect_silent(cur_files <- loadImages(multi_files))
  expect_error(cur_files <- loadImages(c(multi_files, "test")))
})
