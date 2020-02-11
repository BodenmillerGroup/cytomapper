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

  # Subsetting
  ## Getters
  expect_true(is(pancreasImages[1], "ImageList"))
  expect_true(is(pancreasImages[[1]], "Image"))

  expect_equal(names(pancreasImages), c("A02", "D01", "F01"))
  expect_equal(rownames(mcols(pancreasImages)), c("A02", "D01", "F01"))

  ## Setters
  #### These checks are mainly in place to avoid NA or empty names
  cur_Images <- pancreasImages
  names(cur_Images) <- NULL

  ### Should fail
  expect_error(cur_Images[1] <- as(pancreasImages[[2]], "ImageList"))
  expect_error(cur_Images["test"] <- pancreasImages[1])

  expect_error(names(cur_Images) <- c("test1", "test2"))

  ### Should work
  expect_silent(cur_Images[1] <- pancreasImages[1])

  names(cur_Images) <- c("test1", "test2", "test3")
  expect_equal(names(cur_Images), c("test1", "test2", "test3"))
  expect_equal(rownames(mcols(cur_Images)), c("test1", "test2", "test3"))

  expect_error(cur_Images[1] <- "test")
  expect_error(cur_Images[[1]] <- "test")

  ### Test mcols
  cur_Images1 <- pancreasImages
  cur_Images2 <- pancreasImages
  names(cur_Images2) <- c("test1", "test2", "test3")
  mcols(cur_Images2)$ImageNumber <- mcols(cur_Images2)$ImageNb

  cur_Images3 <- c(cur_Images1, cur_Images2)
  expect_true(is.na(mcols(cur_Images3[1,2])))

  ### Test channel subsetting
  cur_Images <- pancreasImages
  expect_error(cur_Images[1] <- as(cur_Images[[1]][,,1], "ImageList"))

  # Accessors
  ## getImages

  ## setImages

  ## getChannels

  ## setChannels

  ## Looping

  ## Vector functions
  expect_equal(length(pancreasImages), 3)

})

test_that("channelNames can be set.", {
  data("pancreasImages")

  # Subset to one channel per image
  })
