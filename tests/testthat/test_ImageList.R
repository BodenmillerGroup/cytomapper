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

  # Check if validity checks are working
  # cur_ImageList[[1]] <- cur_ImageList[[1]][,,1] must fail
})

test_that("Coercion, accessors, looping, subsetting works on ImageList object.", {
  data("pancreasImages")

  # Coercion
  expect_silent(test.list1 <- as.list(pancreasImages))
  expect_silent(test.list2 <- as(pancreasImages, "List"))
  expect_true(!is.null(mcols(test.list2)))
  expect_silent(test.list3 <- as(pancreasImages, "SimpleList"))
  expect_true(!is.null(mcols(test.list3)))

  expect_silent(test.list <- as(test.list1, "ImageList"))
  expect_silent(test.list <- as(test.list2, "ImageList"))
  expect_true(!is.null(mcols(test.list)))
  expect_silent(test.list <- as(test.list3, "ImageList"))
  expect_true(!is.null(mcols(test.list)))

  # Merging
  # Test if c() works

  # Accessors
  ## Getters
  expect_true(is(pancreasImages[1], "ImageList"))
  expect_true(is(pancreasImages[[1]], "Image"))
  getFrames

  mcols

  ## Setters
  cur_Images <- pancreasImages
  expect_silent(cur_Images[1] <- pancreasImages[1])
  expect_identical(cur_Images, pancreasImages)
  expect_silent(cur_Images[[1]] <- pancreasImages[1])
  expect_identical(cur_Images, pancreasImages)
  expect_silent(cur_Images[1] <- pancreasImages[[1]])


})
