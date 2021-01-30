test_that("Images can be loaded into CytoImageList object.", {
  files <- list.files(system.file("extdata", package = "cytomapper"),
             pattern = "imc.tiff", full.names = TRUE)

  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  # Should work
  cur_list <- lapply(files, readImage)  
  names(cur_list) <- c("E34_imc", "G01_imc", "J02_imc")
  expect_silent(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(test1 = cur_list[[1]],
                                           test2 = cur_list[[1]]))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "SimpleList")))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "List")))
  expect_s4_class(cur_ImageList, "CytoImageList")

  # Should not work
  cur_list <- lapply(files, readImage)
  expect_error(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE),
               regexp = paste0("Please specify the names of the images"),
               fixed = TRUE)
  names(cur_list) <- c("E34_imc", "G01_imc", "J02_imc")
  expect_error(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE),
               regexp = paste0("When storing the images on disk, please specify a 'h5FilesPath'. \n",
                               "You can use 'h5FilesPath = getHDF5DumpDir()' to temporarily store the images.\n",
                               "If doing so, .h5 files will be deleted once the R session ends."),
               fixed = TRUE)

  cur_list[[1]] <- cur_list[[1]][,,-1]
  expect_error(CytoImageList(cur_list))
})

test_that("Show function works.", {
    data(pancreasImages)
    
    test <- capture.output(show(pancreasImages))
    expect_length(test, 4)
    expect_equal(test[1], "CytoImageList containing 3 image(s)")
    expect_equal(test[2], "names(3): E34_imc G01_imc J02_imc ")
    expect_equal(test[3], "Each image contains 5 channel(s)")
    expect_equal(test[4], "channelNames(5): H3 CD99 PIN CD8a CDH ")
    
    data(pancreasMasks)
    
    test <- capture.output(show(pancreasMasks))
    expect_length(test, 3)
    expect_equal(test[1], "CytoImageList containing 3 image(s)")
    expect_equal(test[2], "names(3): E34_mask G01_mask J02_mask ")
    expect_equal(test[3], "Each image contains 1 channel")
})
