test_that("On disk: Image and channel names can be extracted and set.", {
  data("pancreasImages")
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
    
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  
  cur_size <- file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]

  # Standard calls - CytoImageList
  expect_equal(channelNames(cur_Images),
               c("H3", "CD99", "PIN", "CD8a", "CDH"))
  expect_equal(names(cur_Images),
               c("E34_imc", "G01_imc", "J02_imc"))

  expect_silent(channelNames(cur_Images) <- c("test1", "test2", "test3", "test4", "test5"))
  expect_equal(channelNames(cur_Images),
               c("test1", "test2", "test3", "test4", "test5"))
  
  expect_equal(cur_size, file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"])
  
  expect_silent(names(cur_Images) <- c("test1", "test2", "test3"))
  expect_equal(names(cur_Images),
               c("test1", "test2", "test3"))
  
  expect_equal(cur_size, file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"])

  # Should not work
  expect_error(channelNames(cur_Images) <- "test")
  expect_error(names(cur_Images) <- "test")

  # Subset to one channel per image
  cur_Images_2 <- getChannels(cur_Images, 1)
  
  expect_equal(cur_size, file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]) 
  
  expect_equal(channelNames(cur_Images_2), c("test1"))
  expect_silent(channelNames(cur_Images_2) <- "test2")
  expect_equal(channelNames(cur_Images_2), "test2")

  expect_error(channelNames(cur_Images_2) <- c("test1", "test2"))
  
  expect_equal(cur_size, file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]) 

  # Check if expansion works
  ## Subset image that third dimension is lost
  cur_Images@listData <- lapply(cur_Images, function(x){
    return(x[,,1])
  })
  cur_Image2 <- cur_Images
  expect_null(channelNames(cur_Image2))
  channelNames(cur_Image2) <- "test"
  expect_equal(channelNames(cur_Image2), "test")
  expect_identical(cur_Image2[[1]][,,1], cur_Images[[1]])
  
  expect_equal(dim(cur_Image2[[1]]), c(100, 100, 1))
  expect_equal(dim(cur_Images[[1]]), c(100, 100))  
  
  expect_equal(cur_size, file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]) 

  # Set null
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  expect_silent(channelNames(cur_Images) <- NULL)
  expect_null(channelNames(cur_Images))
  expect_silent(names(cur_Images) <- NULL)
  expect_null(names(cur_Images))
  
  expect_equal(cur_size, file.info(paste0(cur_path, "/E34_imc.h5"))[,"size"]) 
  })
