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
  # Standard calls - CytoImageList
  expect_equal(channelNames(pancreasImages),
               c("H3", "SMA", "INS", "CD38", "CD44"))
  expect_equal(names(pancreasImages),
               c("A02_imc", "D01_imc", "F01_imc"))

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
