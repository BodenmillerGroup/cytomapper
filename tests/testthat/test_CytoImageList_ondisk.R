test_that("On disk: Images can be loaded into CytoImageList object.", {
    
  data("pancreasImages")
    
  files <- list.files(system.file("extdata", package = "cytomapper"),
             pattern = "imc.tiff", full.names = TRUE)

  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  # Should work - reading in on disk
  cur_list <- lapply(files, readImage)  
  names(cur_list) <- c("E34_imc", "G01_imc", "J02_imc")
  expect_silent(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  
  expect_s4_class(cur_ImageList$E34_imc, "HDF5Array")
  expect_equal(cur_ImageList$E34_imc@seed@name, "/E34_imc")
  expect_false(cur_ImageList$E34_imc@seed@as_sparse)
  expect_equal(cur_ImageList$E34_imc@seed@dim, c(100, 100, 5))
  expect_equal(cur_ImageList$E34_imc@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_true(expect_true(file.exists(file.path(cur_path, "E34_imc.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "G01_imc.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "J02_imc.h5"))))
  
  expect_equal(names(cur_ImageList), c("E34_imc", "G01_imc", "J02_imc"))
  
  expect_identical(as.array(cur_ImageList$E34_imc)[1:10, 1:10, 1], 
                   as.array(pancreasImages$E34_imc)[1:10, 1:10, 1])
  
  expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
  
  expect_silent(cur_ImageList <- CytoImageList(test1 = cur_list[[1]],
                                           test2 = cur_list[[1]],
                                           on_disk = TRUE, 
                                           h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(length(cur_ImageList), 2L)
  
  expect_s4_class(cur_ImageList$test1, "HDF5Array")
  expect_equal(cur_ImageList$test1@seed@name, "/test1")
  expect_false(cur_ImageList$test1@seed@as_sparse)
  expect_equal(cur_ImageList$test1@seed@dim, c(100, 100, 5))
  expect_equal(cur_ImageList$test1@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_true(file.exists(file.path(cur_path, "test1.h5")))
  expect_true(file.exists(file.path(cur_path, "test2.h5")))
  
  expect_equal(names(cur_ImageList), c("test1", "test2"))
  
  expect_true(file.remove(file.path(cur_path, "test1.h5")))
  expect_true(file.remove(file.path(cur_path, "test2.h5")))
  
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "SimpleList"),
                                               on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(names(cur_ImageList), c("E34_imc", "G01_imc", "J02_imc"))
  expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
  
  
  expect_silent(cur_ImageList <- CytoImageList(as(cur_list, "List"),
                                               on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(names(cur_ImageList), c("E34_imc", "G01_imc", "J02_imc"))
  expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
  
  ## Parallelisation
  cur_list <- lapply(files, readImage)  
  names(cur_list) <- c("E34_imc", "G01_imc", "J02_imc")
  expect_silent(cur_ImageList <- CytoImageList(cur_list, on_disk = TRUE, 
                                               h5FilesPath = cur_path,
                                               BPPARAM = BiocParallel::bpparam()))
  
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
  
  # Should work - memory --> on disk
  expect_silent(cur_ImageList <- CytoImageList(pancreasImages, on_disk = TRUE, 
                                               h5FilesPath = cur_path))
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(mcols(cur_ImageList), mcols(pancreasImages))
  expect_equal(channelNames(cur_ImageList), channelNames(pancreasImages))
  expect_equal(names(cur_ImageList), names(pancreasImages))
  expect_equal(as.array(cur_ImageList$E34_imc), as.array(pancreasImages$E34_imc))
  expect_lt(object.size(cur_ImageList), object.size(pancreasImages))
  
  expect_s4_class(cur_ImageList$E34_imc, "HDF5Array")
  expect_equal(cur_ImageList$E34_imc@seed@name, "/E34_imc")
  expect_false(cur_ImageList$E34_imc@seed@as_sparse)
  expect_equal(cur_ImageList$E34_imc@seed@dim, c(100, 100, 5))
  expect_equal(cur_ImageList$E34_imc@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_true(expect_true(file.exists(file.path(cur_path, "E34_imc.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "G01_imc.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "J02_imc.h5"))))
  
  expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
  
  ## Parallelisation
  expect_silent(cur_ImageList <- CytoImageList(pancreasImages, on_disk = TRUE, 
                                               h5FilesPath = cur_path,
                                               BPPARAM = BiocParallel::bpparam()))
  
  expect_s4_class(cur_ImageList, "CytoImageList")
  expect_equal(mcols(cur_ImageList), mcols(pancreasImages))
  expect_equal(channelNames(cur_ImageList), channelNames(pancreasImages))
  expect_equal(names(cur_ImageList), names(pancreasImages))
  expect_equal(as.array(cur_ImageList$E34_imc), as.array(pancreasImages$E34_imc))
  expect_lt(object.size(cur_ImageList), object.size(pancreasImages))
  
  expect_s4_class(cur_ImageList$E34_imc, "HDF5Array")
  expect_equal(cur_ImageList$E34_imc@seed@name, "/E34_imc")
  expect_false(cur_ImageList$E34_imc@seed@as_sparse)
  expect_equal(cur_ImageList$E34_imc@seed@dim, c(100, 100, 5))
  expect_equal(cur_ImageList$E34_imc@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_true(expect_true(file.exists(file.path(cur_path, "E34_imc.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "G01_imc.h5"))))
  expect_true(expect_true(file.exists(file.path(cur_path, "J02_imc.h5"))))
  
  # Should work - on disk --> memory
  expect_silent(cur_ImageList_memory <- CytoImageList(cur_ImageList, on_disk = FALSE))
  expect_equal(cur_ImageList_memory$E34_imc, pancreasImages$E34_imc)
  expect_equal(mcols(cur_ImageList_memory), mcols(pancreasImages))
  expect_equal(channelNames(cur_ImageList_memory), channelNames(pancreasImages))
  expect_equal(names(cur_ImageList_memory), names(pancreasImages))
  expect_gt(object.size(cur_ImageList_memory), object.size(cur_ImageList))
  
  ## Parallelisation
  expect_silent(cur_ImageList_memory <- CytoImageList(cur_ImageList, on_disk = FALSE,
                                                      BPPARAM = BiocParallel::bpparam()))
  
  expect_equal(cur_ImageList_memory$E34_imc, pancreasImages$E34_imc)
  expect_equal(mcols(cur_ImageList_memory), mcols(pancreasImages))
  expect_equal(channelNames(cur_ImageList_memory), channelNames(pancreasImages))
  expect_equal(names(cur_ImageList_memory), names(pancreasImages))
  expect_gt(object.size(cur_ImageList_memory), object.size(cur_ImageList))
  
  expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))

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
  expect_error(CytoImageList(cur_list, on_disk = TRUE, h5FilesPath = cur_path))
})

test_that("On disk: Show function works.", {
    data(pancreasImages)
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    expect_silent(cur_ImageList <- CytoImageList(pancreasImages, on_disk = TRUE, 
                                                 h5FilesPath = cur_path))
    
    test <- capture.output(show(cur_ImageList))
    expect_length(test, 4)
    expect_equal(test[1], "CytoImageList containing 3 image(s)")
    expect_equal(test[2], "names(3): E34_imc G01_imc J02_imc ")
    expect_equal(test[3], "Each image contains 5 channel(s)")
    expect_equal(test[4], "channelNames(5): H3 CD99 PIN CD8a CDH ")
    
    data(pancreasMasks)
    
    expect_silent(cur_ImageList <- CytoImageList(pancreasMasks, on_disk = TRUE, 
                                                 h5FilesPath = cur_path))
    
    test <- capture.output(show(pancreasMasks))
    expect_length(test, 3)
    expect_equal(test[1], "CytoImageList containing 3 image(s)")
    expect_equal(test[2], "names(3): E34_mask G01_mask J02_mask ")
    expect_equal(test[3], "Each image contains 1 channel")
})
