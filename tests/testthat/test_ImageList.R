test_that("Images can be loaded into ImageList object.", {
  files <- list.files(system.file("extdata", package = "SingleCellMapper"),
             pattern = "imc.tiff", full.names = TRUE)

  # Should work
  cur_list <- lapply(files, readImage)
  expect_silent(cur_ImageList <- ImageList(cur_list))
  expect_s4_class(cur_ImageList, "ImageList")
  expect_silent(cur_ImageList <- ImageList(test1 = cur_list[[1]],
                                           test2 = cur_list[[1]]))
  expect_s4_class(cur_ImageList, "ImageList")
  expect_silent(cur_ImageList <- ImageList(as(cur_list, "SimpleList")))
  expect_s4_class(cur_ImageList, "ImageList")
  expect_silent(cur_ImageList <- ImageList(as(cur_list, "List")))
  expect_s4_class(cur_ImageList, "ImageList")

  # Should not work
  expect_error(ImageList("test"))

  cur_list[[1]] <- cur_list[[1]][,,-1]
  expect_error(ImageList(cur_list))
})

test_that("Generic plot function works on images.", {
  data("pancreasImages")
  data("pancreasMasks")

  # Works
  expect_silent(plot(pancreasImages))
  expect_silent(plot(pancreasMasks))
})
