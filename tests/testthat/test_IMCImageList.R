test_that("Images can be loaded into IMCImageList object.", {
  files <- list.files(system.file("extdata", package = "IMCMapper"),
             pattern = "imc.tiff", full.names = TRUE)

  # Should work
  cur_list <- lapply(files, readImage)
  expect_silent(cur_ImageList <- IMCImageList(cur_list))
  expect_s4_class(cur_ImageList, "IMCImageList")
  expect_silent(cur_ImageList <- IMCImageList(test1 = cur_list[[1]],
                                           test2 = cur_list[[1]]))
  expect_s4_class(cur_ImageList, "IMCImageList")
  expect_silent(cur_ImageList <- IMCImageList(as(cur_list, "SimpleList")))
  expect_s4_class(cur_ImageList, "IMCImageList")
  expect_silent(cur_ImageList <- IMCImageList(as(cur_list, "List")))
  expect_s4_class(cur_ImageList, "IMCImageList")

  # Should not work
  expect_error(IMCImageList("test"))

  cur_list[[1]] <- cur_list[[1]][,,-1]
  expect_error(IMCImageList(cur_list))
})
