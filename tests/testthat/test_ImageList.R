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
  expect_error(cur_Images["test"] <- pancreasImages[1])
  expect_error(names(cur_Images) <- c("test1", "test2"))
  expect_error(cur_Images[1] <- pancreasImages[[1]])

  ### Should work
  expect_silent(cur_Images[1] <- pancreasImages[1])
  expect_silent(cur_Images[1] <- as(pancreasImages[[2]], "ImageList"))

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
  ### Should work
  expect_s4_class(getImages(pancreasImages, "A02"), "ImageList")
  expect_s4_class(getImages(pancreasImages, "A02")[[1]], "Image")
  expect_s4_class(getImages(pancreasImages, 1), "ImageList")
  expect_s4_class(getImages(pancreasImages, 1)[[1]], "Image")
  expect_s4_class(getImages(pancreasImages, c(TRUE, FALSE, FALSE)), "ImageList")
  expect_s4_class(getImages(pancreasImages, c(TRUE, FALSE, FALSE))[[1]], "Image")
  expect_s4_class(getImages(pancreasImages, c("A02", "D01")), "ImageList")
  expect_s4_class(getImages(pancreasImages, c(1,2)), "ImageList")

  ### Should not work
  expect_error(getImages(pancreasImages, "A"))
  expect_error(getImages(pancreasImages, "A02", "test"))
  expect_error(getImages(pancreasImages, c("A02", "test")))
  expect_error(getImages(pancreasImages, 4))

  ## setImages
  cur_Images1 <- pancreasImages
  cur_Images2 <- pancreasImages
  names(cur_Images2) <- c("test1", "test2", "test3")
  mcols(cur_Images2)$ImageNumber <- mcols(cur_Images2)$ImageNb
  cur_Images3 <- cur_Images2
  names(cur_Images3) <- NULL

  ### Should work
  expect_silent(setImages(cur_Images1, "A02") <- cur_Images2[1])
  expect_equal(names(cur_Images1), c("A02", "D01", "F01"))
  expect_equal(channelNames(cur_Images1), c("H3", "SMA", "INS", "CD38", "CD44"))
  expect_silent(setImages(cur_Images1, "A02") <- cur_Images2[[1]])
  expect_silent(setImages(cur_Images1, "G02") <- cur_Images2[1])
  expect_equal(names(cur_Images1), c("A02", "D01", "F01", "G02"))
  expect_equal(channelNames(cur_Images1), c("H3", "SMA", "INS", "CD38", "CD44"))
  expect_silent(setImages(cur_Images1, 1) <- cur_Images2[2])
  expect_equal(names(cur_Images1), c("test2", "D01", "F01", "G02"))
  expect_equal(channelNames(cur_Images1), c("H3", "SMA", "INS", "CD38", "CD44"))
  expect_silent(setImages(cur_Images1, 1:2) <- cur_Images2[2:3])
  expect_equal(names(cur_Images1), c("test2", "test3", "F01", "G02"))
  expect_equal(channelNames(cur_Images1), c("H3", "SMA", "INS", "CD38", "CD44"))
  expect_silent(setImages(cur_Images3, 1) <- cur_Images3[2])
  expect_equal(channelNames(cur_Images3), c("H3", "SMA", "INS", "CD38", "CD44"))

  ### Should not work
  expect_error(setImages(cur_Images1, 1) <- cur_Images2[[1]])
  expect_error(setImages(cur_Images1, 1:2) <- cur_Images2[2])
  expect_error(setImages(cur_Images1, 1) <- cur_Images3[2])
  expect_error(setImages(cur_Images3, 1) <- pancreasImages[2])
  expect_error(setImages(cur_Images3, "test") <- cur_Images3[2])

  ## getChannels
  ### Should work
  expect_s4_class(getChannels(pancreasImages, 1), "ImageList")
  expect_s4_class(getChannels(pancreasImages, 1:2), "ImageList")
  expect_s4_class(getChannels(pancreasImages, "H3"), "ImageList")

  expect_silent(test <- getChannels(pancreasImages, 1))
  expect_equal(channelNames(test), "H3")
  expect_equal(length(test), 3L)
  expect_equal(names(test), c("A02", "D01", "F01"))

  expect_silent(test <- getChannels(pancreasImages, "H3"))
  expect_equal(channelNames(test), "H3")
  expect_equal(length(test), 3L)
  expect_equal(names(test), c("A02", "D01", "F01"))

  ### Should not work
  expect_error(getChannels(pancreasImages, 10))
  expect_error(getChannels(pancreasImages, "test"))
  expect_error(getChannels(pancreasImages, c("H3", "test")))

  ## setChannels
  cur_Images1 <- pancreasImages
  cur_Images2 <- getChannels(pancreasImages, 2)

  ### Should work
  expect_silent(setChannels(cur_Images1, 1) <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("SMA", "SMA", "INS", "CD38", "CD44"))
  expect_equal(cur_Images1[[1]][,,1], cur_Images1[[1]][,,2])

  expect_silent(setChannels(cur_Images1, "INS") <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("SMA", "SMA", "INS", "CD38", "CD44"))
  expect_equal(cur_Images1[[1]][,,2], cur_Images1[[1]][,,3])

  cur_Images1 <- pancreasImages
  cur_Images2 <- getChannels(pancreasImages, 2:3)

  expect_silent(setChannels(cur_Images1, 1:2) <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("SMA", "INS", "INS", "CD38", "CD44"))
  expect_equal(cur_Images1[[1]][,,2], cur_Images1[[1]][,,3])

  expect_silent(setChannels(cur_Images1, 1:2) <- NULL)
  expect_equal(channelNames(cur_Images1), c("INS", "CD38", "CD44"))

  ### Should not work
  cur_Images1 <- pancreasImages
  cur_Images2 <- getChannels(pancreasImages, 2)
  expect_error(setChannels(cur_Images1, 6) <- cur_Images2)
  expect_error(setChannels(cur_Images1, "test") <- cur_Images2)
  expect_error(setChannels(cur_Images1, 1) <- "test")

  ## Looping
  ### Should work
  expect_silent(cur_list <- lapply(pancreasImages, dim))
  expect_true(is.list(cur_list))
  expect_silent(cur_list <- endoapply(pancreasImages, function(x){
    channelNames(x) <- NULL
    return(x)}))
  expect_true(is(cur_list, "ImageList"))

  ### Should not work
  expect_error(cur_list <- endoapply(pancreasImages, dim))

  ## Vector functions
  expect_equal(length(pancreasImages), 3L)
  expect_null(dim(pancreasImages))
})

test_that("Image and channel names can be extracted and set.", {
  data("pancreasImages")
  cur_Images <- pancreasImages

  # Standard calls - Image
  cur_Image <- pancreasImages[[1]]
  expect_equal(channelNames(cur_Image),
               c("H3", "SMA", "INS", "CD38", "CD44"))

  expect_silent(channelNames(cur_Image) <- c("test1", "test2", "test3", "test4", "test5"))
  expect_equal(channelNames(cur_Image),
               c("test1", "test2", "test3", "test4", "test5"))
  # Standard calls - ImageList
  expect_equal(channelNames(pancreasImages),
               c("H3", "SMA", "INS", "CD38", "CD44"))
  expect_equal(names(pancreasImages),
               c("A02", "D01", "F01"))

  expect_silent(channelNames(cur_Images) <- c("test1", "test2", "test3", "test4", "test5"))
  expect_equal(channelNames(cur_Images),
               c("test1", "test2", "test3", "test4", "test5"))
  expect_silent(names(cur_Images) <- c("test1", "test2", "test3"))
  expect_equal(names(cur_Images),
               c("test1", "test2", "test3"))

  # Should not work
  expect_error(channelNames(cur_Images) <- "test")
  expect_error(names(cur_Images) <- "test")

  # Subset to one channel per image
  cur_Images <- getChannels(pancreasImages, 1)
  expect_equal(channelNames(cur_Images), c("H3"))
  expect_silent(channelNames(cur_Images) <- "test1")
  expect_equal(channelNames(cur_Images), "test1")

  expect_error(channelNames(cur_Images) <- c("test1", "test2"))

  # Check if expansion works
  ## Subset image that third dimension is lost
  cur_Image <- pancreasImages[[1]][,,1]
  cur_Image2 <- cur_Image
  expect_null(channelNames(cur_Image2))
  channelNames(cur_Image2) <- "test"
  expect_equal(channelNames(cur_Image2), "test")
  expect_identical(cur_Image2[,,1], cur_Image)

  cur_Images <- S4Vectors::endoapply(pancreasImages, function(x){
    return(x[,,1])
  })
  cur_Image2 <- cur_Images
  expect_null(channelNames(cur_Image2))
  channelNames(cur_Image2) <- "test"
  expect_equal(channelNames(cur_Image2), "test")
  expect_identical(cur_Image2[[1]][,,1], cur_Images[[1]])

  # Set null
  expect_silent(channelNames(pancreasImages) <- NULL)
  expect_null(channelNames(pancreasImages))
  expect_silent(names(pancreasImages) <- NULL)
  expect_null(names(pancreasImages))
  })

test_that("ImageLists can be merged.", {
  data("pancreasImages")

  # Check mergeChannels function
})
