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
  expect_identical(mcols(test.list2), mcols(pancreasImages))
  expect_identical(test.list2[[1]], pancreasImages[[1]])

  expect_silent(test.list3 <- as(pancreasImages, "SimpleList"))
  expect_identical(mcols(test.list3), mcols(pancreasImages))
  expect_identical(test.list3[[1]], pancreasImages[[1]])

  expect_silent(test.list <- as(test.list1, "ImageList"))
  expect_true(is.null(mcols(test.list)))

  expect_silent(test.list <- as(test.list2, "ImageList"))
  expect_identical(mcols(test.list2), mcols(test.list))
  expect_identical(test.list2[[1]], test.list[[1]])

  expect_silent(test.list <- as(test.list3, "ImageList"))
  expect_identical(mcols(test.list3), mcols(test.list))
  expect_identical(test.list3[[1]], test.list[[1]])

  # Merging

  ## Should work
  expect_silent(test.list <- c(pancreasImages[c(1,3)], pancreasImages[2]))
  expect_equal(names(test.list), c("A02", "F01", "D01"))
  expect_equal(rownames(mcols(test.list)), c("A02", "F01", "D01"))
  expect_identical(test.list[[3]], pancreasImages[[2]])

  ## Should fail due to duplicated names
  expect_error(test.list <- c(pancreasImages, pancreasImages))

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
  expect_identical(cur_Images, pancreasImages)

  expect_silent(cur_Images[[1]] <- pancreasImages[[1]])
  expect_identical(cur_Images, pancreasImages)

  expect_error(cur_Images[1] <- "test")
  expect_error(cur_Images[[1]] <- "test")

  ### Make sure the metadata and names are stored correctly
  cur_Images <- pancreasImages

  cur_Images[1] <- pancreasImages[3]
  expect_identical(cur_Images[[1]], cur_Images[[3]])
  expect_identical(names(cur_Images), c("F01", "D01", "F01"))
  expect_identical(rownames(mcols(cur_Images)), c("F01", "D01", "F01"))
  expect_equal(mcols(cur_Images)$ImageNb, c(3,2,3))

  cur_Images[[1]] <- pancreasImages[3]
  expect_identical(cur_Images[[1]], cur_Images[[3]])
  expect_identical(names(cur_Images), c("F01", "D01", "F01"))
  expect_identical(rownames(mcols(cur_Images)), c("F01", "D01", "F01"))
  expect_equal(mcols(cur_Images)$ImageNb, c(3,2,3))

  cur_Images[1] <- pancreasImages[[3]]
  expect_identical(cur_Images[[1]], cur_Images[[3]])
  expect_identical(names(cur_Images), c("F01", "D01", "F01"))
  expect_identical(rownames(mcols(cur_Images)), c("F01", "D01", "F01"))
  expect_equal(mcols(cur_Images)$ImageNb, c(3,2,3))

  cur_Images[[1]] <- pancreasImages[[3]]
  expect_identical(cur_Images[[1]], cur_Images[[3]])
  expect_identical(names(cur_Images), c("F01", "D01", "F01"))
  expect_identical(rownames(mcols(cur_Images)), c("F01", "D01", "F01"))
  expect_equal(mcols(cur_Images)$ImageNb, c(3,2,3))

  ## Looping

  ## Vector functions
  expect_equal(length(pancreasImages), 3)

})
