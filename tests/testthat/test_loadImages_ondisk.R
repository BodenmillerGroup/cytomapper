test_that("On disk: loadImages function reads in correct objects on disk.", {
  path <- system.file("extdata", package = "cytomapper")
  single_file <- system.file("extdata/E34_mask.tiff",
                             package = "cytomapper")
  
  # Error
  expect_error(cur_file <- loadImages(single_file, on_disk = TRUE), 
               regexp = paste0("When storing the images on disk, please specify a 'h5FilesPath'. \n",
                              "You can use 'h5FilesPath = getHDF5DumpDir()' to temporarily store the images.\n",
                              "If doing so, .h5 files will be deleted once the R session ends."),
               fixed = TRUE)

  # Single file
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  expect_silent(cur_file <- loadImages(single_file, on_disk = TRUE, h5FilesPath = cur_path))
  expect_s4_class(cur_file, "CytoImageList")
  
  expect_true(file.exists(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                                         basename(single_file)), ".h5"))))
  expect_s4_class(cur_file$E34_mask, "HDF5Matrix")
  expect_equal(cur_file$E34_mask@seed@name, "/E34_mask")
  expect_false(cur_file$E34_mask@seed@as_sparse)
  expect_equal(cur_file$E34_mask@seed@dim, c(100, 100))
  expect_equal(cur_file$E34_mask@seed@first_val, 0.01257343)
  
  expect_silent(cur_file <- loadImages(single_file, on_disk = TRUE, h5FilesPath = cur_path))
  expect_s4_class(cur_file, "CytoImageList")
  
  expect_true(file.exists(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                                         basename(single_file)), ".h5"))))
  
  ## Parallelisation
  expect_true(file.remove(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                             basename(single_file)), ".h5"))))
  expect_silent(cur_file <- loadImages(single_file, on_disk = TRUE, 
                                       h5FilesPath = cur_path, BPPARAM = BiocParallel::bpparam()))
  expect_s4_class(cur_file, "CytoImageList")
  expect_true(file.exists(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                                         basename(single_file)), ".h5"))))
  
  expect_true(file.remove(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                             basename(single_file)), ".h5"))))

  # Pattern
  expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff",
                                        on_disk = TRUE, h5FilesPath = cur_path))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  
  expect_true(file.exists(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.exists(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.exists(file.path(cur_path, "J02_imc.h5")))
  
  expect_s4_class(cur_files$E34_imc, "HDF5Array")
  expect_equal(cur_files$E34_imc@seed@name, "/E34_imc")
  expect_false(cur_files$E34_imc@seed@as_sparse)
  expect_equal(cur_files$E34_imc@seed@dim, c(100, 100, 5))
  expect_equal(cur_files$E34_imc@seed@first_val, 2.235787, tolerance = 0.00001)
  
  expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff",
                                        on_disk = TRUE, h5FilesPath = cur_path))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  
  expect_true(file.exists(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.exists(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.exists(file.path(cur_path, "J02_imc.h5")))
  
  ## Parallelisation
  expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
  expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff",
                                       on_disk = TRUE, h5FilesPath = cur_path, 
                                       BPPARAM = BiocParallel::bpparam()))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  
  expect_true(file.exists(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.exists(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.exists(file.path(cur_path, "J02_imc.h5")))

  # Multiple pattern
  expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
  expect_silent(cur_files <- loadImages(path, pattern = c("E34_imc", "J02_imc"),
                                        on_disk = TRUE, h5FilesPath = cur_path))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 2L)
  
  expect_true(file.exists(file.path(cur_path, "E34_imc.h5")))
  expect_true(file.exists(file.path(cur_path, "J02_imc.h5")))

  # Multiple files
  multi_files <- list.files(system.file("extdata", package = "cytomapper"),
                            pattern = "mask.tiff", full.names = TRUE)
  expect_silent(cur_files <- loadImages(multi_files, on_disk = TRUE, h5FilesPath = cur_path))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)

  expect_true(file.remove(file.path(cur_path, "E34_mask.h5")))
  expect_true(file.remove(file.path(cur_path, "G01_mask.h5")))
  expect_true(file.remove(file.path(cur_path, "J02_mask.h5")))
})

test_that("On disk: getHDF5DumpDir works.", {
    path <- system.file("extdata", package = "cytomapper")
    single_file <- system.file("extdata/E34_mask.tiff",
                               package = "cytomapper")
    
    # Error
    expect_error(cur_file <- loadImages(single_file, on_disk = TRUE), 
                 regexp = paste0("When storing the images on disk, please specify a 'h5FilesPath'. \n",
                                 "You can use 'h5FilesPath = getHDF5DumpDir()' to temporarily store the images.\n",
                                 "If doing so, .h5 files will be deleted once the R session ends."),
                 fixed = TRUE)
    
    # Single file
    cur_path <- HDF5Array::getHDF5DumpDir()
    on.exit(unlink(cur_path))
    expect_silent(cur_file <- loadImages(single_file, on_disk = TRUE, h5FilesPath = cur_path))
    expect_s4_class(cur_file, "CytoImageList")
    
    expect_true(file.exists(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                                           basename(single_file)), ".h5"))))
    expect_s4_class(cur_file$E34_mask, "HDF5Matrix")
    expect_equal(cur_file$E34_mask@seed@name, "/E34_mask")
    expect_false(cur_file$E34_mask@seed@as_sparse)
    expect_equal(cur_file$E34_mask@seed@dim, c(100, 100))
    expect_equal(cur_file$E34_mask@seed@first_val, 0.01257343)
    
    expect_silent(cur_file <- loadImages(single_file, on_disk = TRUE, h5FilesPath = cur_path))
    expect_s4_class(cur_file, "CytoImageList")
    
    expect_true(file.exists(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                                           basename(single_file)), ".h5"))))
    
    ## Parallelisation
    expect_true(file.remove(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                               basename(single_file)), ".h5"))))
    expect_silent(cur_file <- loadImages(single_file, on_disk = TRUE, 
                                         h5FilesPath = cur_path, BPPARAM = BiocParallel::bpparam()))
    expect_s4_class(cur_file, "CytoImageList")
    expect_true(file.exists(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                                           basename(single_file)), ".h5"))))
    
    expect_true(file.remove(file.path(cur_path, paste0(sub("\\.[^.]*$", "", 
                                               basename(single_file)), ".h5"))))
    
    # Pattern
    expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff",
                                          on_disk = TRUE, h5FilesPath = cur_path))
    expect_s4_class(cur_files, "CytoImageList")
    expect_equal(length(cur_files), 3L)
    
    expect_true(expect_true(file.exists(file.path(cur_path, "E34_imc.h5"))))
    expect_true(expect_true(file.exists(file.path(cur_path, "G01_imc.h5"))))
    expect_true(expect_true(file.exists(file.path(cur_path, "J02_imc.h5"))))
    
    expect_s4_class(cur_files$E34_imc, "HDF5Array")
    expect_equal(cur_files$E34_imc@seed@name, "/E34_imc")
    expect_false(cur_files$E34_imc@seed@as_sparse)
    expect_equal(cur_files$E34_imc@seed@dim, c(100, 100, 5))
    expect_equal(cur_files$E34_imc@seed@first_val, 2.235787, tolerance = 0.00001)
    
    expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff",
                                          on_disk = TRUE, h5FilesPath = cur_path))
    expect_s4_class(cur_files, "CytoImageList")
    expect_equal(length(cur_files), 3L)
    
    expect_true(file.exists(file.path(cur_path, "E34_imc.h5")))
    expect_true(file.exists(file.path(cur_path, "G01_imc.h5")))
    expect_true(file.exists(file.path(cur_path, "J02_imc.h5")))
    
    ## Parallelisation
    expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
    expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
    expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
    expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff",
                                          on_disk = TRUE, h5FilesPath = cur_path, 
                                          BPPARAM = BiocParallel::bpparam()))
    expect_s4_class(cur_files, "CytoImageList")
    expect_equal(length(cur_files), 3L)
    
    expect_true(file.exists(file.path(cur_path, "E34_imc.h5")))
    expect_true(file.exists(file.path(cur_path, "G01_imc.h5")))
    expect_true(file.exists(file.path(cur_path, "J02_imc.h5")))
    
    # Multiple pattern
    expect_true(file.remove(file.path(cur_path, "E34_imc.h5")))
    expect_true(file.remove(file.path(cur_path, "G01_imc.h5")))
    expect_true(file.remove(file.path(cur_path, "J02_imc.h5")))
    expect_silent(cur_files <- loadImages(path, pattern = c("E34_imc", "J02_imc"),
                                          on_disk = TRUE, h5FilesPath = cur_path))
    expect_s4_class(cur_files, "CytoImageList")
    expect_equal(length(cur_files), 2L)
    
    expect_true(file.exists(file.path(cur_path, "E34_imc.h5")))
    expect_true(file.exists(file.path(cur_path, "J02_imc.h5")))
    
    # Multiple files
    multi_files <- list.files(system.file("extdata", package = "cytomapper"),
                              pattern = "mask.tiff", full.names = TRUE)
    expect_silent(cur_files <- loadImages(multi_files, on_disk = TRUE, h5FilesPath = cur_path))
    expect_s4_class(cur_files, "CytoImageList")
    expect_equal(length(cur_files), 3L)
    
    expect_true(file.remove(file.path(cur_path, "E34_mask.h5")))
    expect_true(file.remove(file.path(cur_path, "G01_mask.h5")))
    expect_true(file.remove(file.path(cur_path, "J02_mask.h5")))
})
