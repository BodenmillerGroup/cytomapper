test_that("Coercion works on CytoImageList object.", {
  data("pancreasImages")

  expect_silent(test.list1 <- as.list(pancreasImages))

  expect_silent(test.list2 <- as(pancreasImages, "List"))
  expect_identical(mcols(test.list2), mcols(pancreasImages))
  expect_identical(test.list2[[1]], pancreasImages[[1]])

  expect_silent(test.list3 <- as(pancreasImages, "SimpleList"))
  expect_identical(mcols(test.list3), mcols(pancreasImages))
  expect_identical(test.list3[[1]], pancreasImages[[1]])

  expect_silent(test.list <- as(test.list1, "CytoImageList"))
  expect_true(is.null(mcols(test.list)))

  expect_silent(test.list <- as(test.list2, "CytoImageList"))
  expect_identical(mcols(test.list2), mcols(test.list))
  expect_identical(test.list2[[1]], test.list[[1]])

  expect_silent(test.list <- as(test.list3, "CytoImageList"))
  expect_identical(mcols(test.list3), mcols(test.list))
  expect_identical(test.list3[[1]], test.list[[1]])
})

test_that("Merging works on CytoImageList object.", {
  data("pancreasImages")
  # Merging
  ## Should work
  expect_silent(test.list <- c(pancreasImages[c(1,3)], pancreasImages[2]))
  expect_s4_class(test.list, "CytoImageList")
  expect_equal(names(test.list), c("E34_imc", "J02_imc", "G01_imc"))
  expect_equal(rownames(mcols(test.list)), c("E34_imc", "J02_imc", "G01_imc"))
  expect_identical(test.list[[3]], pancreasImages[[2]])

  ## Should fail due to duplicated names
  expect_error(test.list <- c(pancreasImages, pancreasImages),
               regexp = "Only unique entries allowed in a CytoImageList object.",
               fixed = TRUE)

  # Merge channels
  channels1 <- getChannels(pancreasImages, 1:2)
  channels2 <- getChannels(pancreasImages, 3:4)

  ## Should work
  expect_silent(channels3 <- mergeChannels(channels1, channels2))
  expect_equal(channelNames(channels3), c("H3", "CD99", "PIN", "CD8a"))
  expect_equal(names(channels3), c("E34_imc", "G01_imc", "J02_imc"))
  
  # Check if mcols are correctly set
  mcols(channels1) <- DataFrame(test = c("test1", "test2", "test3"))
  mcols(channels2) <- DataFrame(test2 = c("test5", "test6", "test7"))
  expect_silent(channels3 <- mergeChannels(channels1, channels2))
  expect_equal(mcols(channels3), mcols(channels1))

  ## Should not work
  channels1 <- getChannels(pancreasImages, 1:2)
  channels2 <- getChannels(pancreasImages, 3:4)
  expect_error(mergeChannels(channels1, as(channels2, "SimpleList")))

  channels2 <- channels2[1:2]
  expect_error(mergeChannels(channels1, channels2))
  expect_error(mergeChannels(channels1, channels2[[1]]))
  expect_error(mergeChannels(channels1, as(channels2)))

  expect_error(mergeChannels(channels1, channels1))
})

test_that("General operations work on CytoImageList object.", {
    data("pancreasImages")
  # Subsetting
  ## Getters
  expect_true(is(pancreasImages[1], "CytoImageList"))
  expect_true(is(pancreasImages[[1]], "Image"))

  expect_equal(names(pancreasImages), c("E34_imc", "G01_imc", "J02_imc"))
  expect_equal(rownames(mcols(pancreasImages)), c("E34_imc", "G01_imc", "J02_imc"))

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
  expect_silent(cur_Images[1] <- as(pancreasImages[[2]], "CytoImageList"))

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
  expect_true(all(is.na(mcols(cur_Images3[1:3,3]))))

  ### Test channel subsetting
  cur_Images <- pancreasImages
  expect_error(cur_Images[1] <- as(cur_Images[[1]][,,1], "CytoImageList"))

  ## Looping
  ### Should work
  expect_silent(cur_list <- lapply(pancreasImages, dim))
  expect_true(is.list(cur_list))
  expect_silent(cur_list <- endoapply(pancreasImages, function(x){
    EBImage::gblur(x, sigma = 1)
    return(x)}))
  expect_true(is(cur_list, "CytoImageList"))

  ### Should not work
  expect_error(cur_list <- endoapply(pancreasImages, dim))

  ## Vector functions
  expect_equal(length(pancreasImages), 3L)
  expect_null(dim(pancreasImages))
})

test_that("Custom accessors work on CytoImageList object.", {
  data("pancreasImages")
  # Accessors
  ## getImages
  ### Should work
  expect_s4_class(getImages(pancreasImages, "E34_imc"), "CytoImageList")
  expect_s4_class(getImages(pancreasImages, "E34_imc")[[1]], "Image")
  expect_s4_class(getImages(pancreasImages, 1), "CytoImageList")
  expect_s4_class(getImages(pancreasImages, 1)[[1]], "Image")
  expect_s4_class(getImages(pancreasImages, c(TRUE, FALSE, FALSE)), "CytoImageList")
  expect_s4_class(getImages(pancreasImages, c(TRUE, FALSE, FALSE))[[1]], "Image")
  expect_s4_class(getImages(pancreasImages, c("E34_imc", "G01_imc")), "CytoImageList")
  expect_s4_class(getImages(pancreasImages, c(1,2)), "CytoImageList")

  ### Should not work
  expect_error(getImages(pancreasImages, "A"))
  expect_error(getImages(pancreasImages, "E34_imc", "test"))
  expect_error(getImages(pancreasImages, c("E34_imc", "test")))
  expect_error(getImages(pancreasImages, 4))

  ## setImages
  cur_Images1 <- pancreasImages
  cur_Images2 <- pancreasImages
  names(cur_Images2) <- c("test1", "test2", "test3")
  mcols(cur_Images2)$ImageNumber <- mcols(cur_Images2)$ImageNb
  cur_Images3 <- cur_Images2
  names(cur_Images3) <- NULL

  ### Should work
  expect_silent(setImages(cur_Images1, "E34_imc") <- cur_Images2[1])
  expect_equal(names(cur_Images1), c("E34_imc", "G01_imc", "J02_imc"))
  expect_equal(channelNames(cur_Images1), c("H3", "CD99", "PIN", "CD8a", "CDH"))
  expect_equal(mcols(cur_Images1)$ImageNb, c(1, 2, 3))
  expect_equal(mcols(cur_Images1)$ImageNumber, c(1, NA, NA))
  expect_silent(setImages(cur_Images1, "E34_imc") <- cur_Images2[[1]])
  expect_equal(names(cur_Images1), c("E34_imc", "G01_imc", "J02_imc"))
  expect_equal(channelNames(cur_Images1), c("H3", "CD99", "PIN", "CD8a", "CDH"))
  expect_equal(mcols(cur_Images1)$ImageNb, c(1, 2, 3))
  expect_equal(mcols(cur_Images1)$ImageNumber, c(1, NA, NA))
  expect_silent(setImages(cur_Images1, "G02_imc") <- cur_Images2[1])
  expect_equal(names(cur_Images1), c("E34_imc", "G01_imc", "J02_imc", "G02_imc"))
  expect_equal(channelNames(cur_Images1), c("H3", "CD99", "PIN", "CD8a", "CDH"))
  expect_silent(setImages(cur_Images1, 1) <- cur_Images2[2])
  expect_equal(names(cur_Images1), c("test2", "G01_imc", "J02_imc", "G02_imc"))
  expect_equal(channelNames(cur_Images1), c("H3", "CD99", "PIN", "CD8a", "CDH"))
  expect_silent(setImages(cur_Images1, 1:2) <- cur_Images2[2:3])
  expect_equal(names(cur_Images1), c("test2", "test3", "J02_imc", "G02_imc"))
  expect_equal(channelNames(cur_Images1), c("H3", "CD99", "PIN", "CD8a", "CDH"))
  expect_silent(setImages(cur_Images3, 1) <- cur_Images3[2])
  expect_equal(channelNames(cur_Images3), c("H3", "CD99", "PIN", "CD8a", "CDH"))

  ### Should not work
  expect_error(setImages(cur_Images1, 1) <- cur_Images2[[1]])
  expect_error(setImages(cur_Images1, 1:2) <- cur_Images2[2])
  expect_error(setImages(cur_Images1, 1) <- cur_Images3[2])
  expect_error(setImages(cur_Images3, 1) <- pancreasImages[2])
  expect_error(setImages(cur_Images3, "test") <- cur_Images3[2])

  ### Remove images
  expect_silent(setImages(cur_Images1, 1) <- NULL)
  expect_equal(names(cur_Images1), c("test3", "J02_imc", "G02_imc"))
  expect_silent(setImages(cur_Images1, "test3") <- NULL)
  expect_equal(names(cur_Images1), c("J02_imc", "G02_imc"))

  ## getChannels
  ### Should work
  expect_s4_class(getChannels(pancreasImages, 1), "CytoImageList")
  expect_s4_class(getChannels(pancreasImages, 1:2), "CytoImageList")
  expect_s4_class(getChannels(pancreasImages, "H3"), "CytoImageList")

  expect_silent(test <- getChannels(pancreasImages, 1))
  expect_equal(channelNames(test), "H3")
  expect_equal(length(test), 3L)
  expect_equal(names(test), c("E34_imc", "G01_imc", "J02_imc"))

  expect_silent(test <- getChannels(pancreasImages, "H3"))
  expect_equal(channelNames(test), "H3")
  expect_equal(length(test), 3L)
  expect_equal(names(test), c("E34_imc", "G01_imc", "J02_imc"))

  ### Should not work
  expect_error(getChannels(pancreasImages, 10))
  expect_error(getChannels(pancreasImages, "test"))
  expect_error(getChannels(pancreasImages, c("H3", "test")))

  ## setChannels
  cur_Images1 <- pancreasImages
  cur_Images2 <- getChannels(pancreasImages, 2)
  channelNames(cur_Images2) <- "test"

  ### Should work
  expect_silent(setChannels(cur_Images1, 1) <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test", "CD99", "PIN", "CD8a", "CDH"))
  expect_equal(cur_Images1[[1]][,,1], cur_Images1[[1]][,,2])

  expect_silent(setChannels(cur_Images1, "PIN") <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test", "CD99", "PIN", "CD8a", "CDH"))
  expect_equal(cur_Images1[[1]][,,2], cur_Images1[[1]][,,3])

  cur_Images1 <- pancreasImages
  cur_Images2 <- getChannels(pancreasImages, 2:3)
  channelNames(cur_Images2) <- c("test1", "test2")

  expect_silent(setChannels(cur_Images1, 1:2) <- cur_Images2)
  expect_equal(channelNames(cur_Images1), c("test1", "test2", "PIN", "CD8a", "CDH"))
  expect_equal(cur_Images1[[1]][,,2], cur_Images1[[1]][,,3])

  expect_silent(setChannels(cur_Images1, 1:2) <- NULL)
  expect_equal(channelNames(cur_Images1), c("PIN", "CD8a", "CDH"))
  expect_silent(setChannels(cur_Images1, "PIN") <- NULL)
  expect_equal(channelNames(cur_Images1), c("CD8a", "CDH"))

  ### Should not work
  cur_Images1 <- pancreasImages
  cur_Images2 <- getChannels(pancreasImages, 2)
  expect_error(setChannels(cur_Images1, 6) <- cur_Images2)
  expect_error(setChannels(cur_Images1, "test") <- cur_Images2)
  expect_error(setChannels(cur_Images1, 1) <- "test")
  expect_error(setChannels(cur_Images1, 1) <- cur_Images2)
})

