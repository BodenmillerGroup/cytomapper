test_that("Images can be loaded into ImageList object.", {
  files <- list.files(system.file("extdata", package = "SingleCellMapper"),
             pattern = "imc.tiff", full.names = TRUE)

  cur_list <- lapply(files, readImage)
  cur_ImageList <- ImageList(cur_list)

  # Check class
  expect_s4_class(cur_ImageList, "ImageList")

  # Check errors
  expect_error(ImageList("test"))

  cur_list[[1]] <- cur_list[[1]][,,-1]
  expect_error(ImageList(cur_list))
})
