test_that("Images can be loaded into CytoImageList object.", {
  files <- list.files(system.file("extdata", package = "cytomapper"),
             pattern = "imc.tiff", full.names = TRUE)

  # Should work
  cur_list <- lapply(files, readImage)
  expect_silent(cur_ImageList <- CytoImageList(cur_list))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(test1 = cur_list[[1]],
                                           test2 = cur_list[[1]]))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "SimpleList")))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "List")))
  expect_s4_class(cur_ImageList, "CytoImageList")

  # Should not work
  expect_error(CytoImageList("test"))

  cur_list[[1]] <- cur_list[[1]][,,-1]
  expect_error(CytoImageList(cur_list))
})
