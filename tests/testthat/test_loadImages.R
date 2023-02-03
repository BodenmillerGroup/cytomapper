build_single_channels <- function(CIL, tempdir) {

    lapply(seq_along(CIL), function(x){
        dir.create(file.path(tempdir, names(CIL)[x]), showWarnings = FALSE)
        for (i in channelNames(CIL)) {
            EBImage::writeImage(as.array(CIL[[x]][,,i]) / ((2^16) - 1),
                            files = file.path(tempdir, names(CIL)[x], paste0(i, ".tiff")),
                            bits.per.sample = 16L)
        }
    })
}

test_that("loadImages function reads in correct objects.", {
  path <- system.file("extdata", package = "cytomapper")
  single_file <- system.file("extdata/E34_mask.tiff",
                             package = "cytomapper")

  # Single file
  expect_silent(cur_file <- loadImages(single_file))
  expect_error(cur_file <- loadImages("test"))
  expect_s4_class(cur_file, "CytoImageList")

  expect_silent(cur_file <- scaleImages(cur_file, 2^16))
  expect_error(plotCells(cur_file), regexp = "Segmentation masks must only contain integer values.",
               fixed = TRUE)
  
  expect_silent(cur_file <- loadImages(single_file))
  expect_error(cur_file <- loadImages("test"))
  expect_s4_class(cur_file, "CytoImageList")
  
  expect_silent(cur_file <- scaleImages(cur_file, 2^16-1))

  expect_silent(plotCells(cur_file))

  # Pattern
  expect_error(cur_files <- loadImages(path, pattern = ".tiff"))
  expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff"))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  expect_silent(cur_files <- loadImages(path, pattern = "_imc"))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  expect_silent(cur_files <- loadImages(path, pattern = "[A-Z]01_imc"))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 1L)
  expect_error(cur_files <- loadImages(path, pattern = "test"))
  expect_silent(plotPixels(cur_files))

  # Multiple pattern
  expect_silent(cur_files <- loadImages(path, pattern = c("E34_imc", "J02_imc")))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 2L)
  expect_silent(cur_files <- loadImages(path, pattern = c("E34_imc", "E34_imc",
                                                          "J02_imc", "J02_imc")))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 2L)
  expect_silent(cur_files <- loadImages(path, pattern = c("E34_imc", "test")))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 1L)
  expect_error(cur_files <- loadImages(path, pattern = c("test1", "test")))

  # Multiple files
  multi_files <- list.files(system.file("extdata", package = "cytomapper"),
                            pattern = "mask.tiff", full.names = TRUE)
  expect_silent(cur_files <- loadImages(multi_files))
  expect_s4_class(cur_files, "CytoImageList")
  expect_equal(length(cur_files), 3L)
  expect_error(cur_files <- loadImages(c(multi_files, "test")))

  expect_silent(cur_files <- scaleImages(cur_files, (2^16)-1))
  expect_silent(plotCells(cur_files))
  
  # .h5 files
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  expect_silent(cur_files <- loadImages(path, pattern = "_imc.tiff", 
                                       on_disk = TRUE, h5FilesPath = cur_path))
  
  expect_silent(cur_files_2 <- loadImages(cur_path, pattern = "_imc.h5"))
 
  expect_identical(cur_files, cur_files_2) 
  expect_equal(object.size(cur_files), object.size(cur_files_2))
  
  expect_silent(plotPixels(cur_files_2))
  
  expect_silent(cur_file <- loadImages(paste0(cur_path, "/E34_imc.h5")))
  
  expect_silent(plotPixels(cur_file))
  
  # Single-channel
  testdir <- tempdir()
  on.exit(unlink(testdir))
  
  multi_files <- list.files(system.file("extdata", package = "cytomapper"),
                            pattern = "imc.tiff", full.names = TRUE)
  
  cur_files <- loadImages(multi_files)
  channelNames(cur_files) <- c("c1", "c2", "c3", "c4", "c5")
  
  build_single_channels(cur_files, tempdir = testdir)
  
  CIL <- loadImages(testdir, pattern = "_imc$", single_channel = TRUE, as.is = TRUE)
  cur_files <- endoapply(cur_files, floor)
  expect_equal(CIL, cur_files)
  
  CIL <- loadImages(file.path(testdir, "E34_imc"), single_channel = TRUE, as.is = TRUE)
  expect_equal(CIL, cur_files["E34_imc"])
  
  # Test name
  expect_silent(cur_file <- loadImages(paste0(cur_path, "/E34_imc.h5"), name = "E34_imc"))
  
  expect_silent(plotPixels(cur_file))
  
  expect_error(cur_files_2 <- loadImages(cur_path, pattern = "_imc.h5", name = "E34_imc"))
  
  expect_silent(cur_files_2 <- loadImages(cur_path, pattern = "_imc.h5", name = c("E34_imc", "G01_imc", "J02_imc")))
  
  cur_files_2 <- normalize(cur_files_2)
  
  expect_silent(cur_files_3 <- loadImages(cur_path, pattern = "_imc.h5", 
                                          name = c("E34_imc_norm", "G01_imc_norm", "J02_imc_norm")))
  
  expect_identical(cur_files_2, cur_files_3)
  
  expect_silent(plotPixels(cur_files_3))
  
  
  # Error
  expect_error(cur_files_2 <- loadImages(cur_path, pattern = "_imc.h5", name = 1),
               regexp = "Argument 'name' must be of type character.",
               fixed = TRUE)
  
  expect_error(cur_files_2 <- loadImages(cur_path, pattern = "_imc.h5", name = c("E34_imc_norm", "G01_imc_norm")),
               regexp = "Length of 'name' must either be 1 or the same length as the number of files read in.",
               fixed = TRUE)
  expect_error(cur_files <- loadImages(testdir, pattern = "_imc$", single_channel = 1),
               regexp = "'single_channel' needs to be a single logical.",
               fixed = TRUE)
  expect_error(cur_files <- loadImages(testdir, pattern = "_imc$", single_channel = 1),
               regexp = "'single_channel' needs to be a single logical.",
               fixed = TRUE)
  expect_error(cur_files <- loadImages(testdir, pattern = "_imc$", single_channel = c(2,3)),
               regexp = "'single_channel' needs to be a single logical.",
               fixed = TRUE)
  expect_error(cur_files <- loadImages(list.files(testdir), pattern = "_imc$", single_channel = TRUE),
               regexp = "Setting 'single_channel' requires 'x' to be a single path.",
               fixed = TRUE)
  expect_error(cur_files <- loadImages(testdir, single_channel = TRUE),
               regexp = "The files are of type other than 'jpeg', 'tiff', 'png' or different file types are mixed.",
               fixed = TRUE)
 })
