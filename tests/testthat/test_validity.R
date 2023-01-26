test_that("loadImage validity check is correct.", {
    # Test file
    cur_dir <- system.file("scripts", package = "cytomapper")
    cur_file <- system.file("scripts/1_LoadPancreasData.Rmd",
                               package = "cytomapper")
    
    # Error
    expect_error(.valid.loadImage.input(x = 1),
                 regexp = "Please provide a string input indicating a single file\n, a path or a vector of files.",
                 fixed = TRUE)
    expect_error(.valid.loadImage.input(x = "test.png", single_channel = FALSE),
                 regexp = "The provided file or path does not exist.\nMake sure the file or path is accessible\nfrom the current location.",
                 fixed = TRUE)
    expect_error(.valid.loadImage.input(x = cur_dir, pattern = NULL, single_channel = FALSE),
                 regexp = "The provided path contains file-types other than\n'jpeg', 'tiff', 'png' or 'h5'.\nPlease provide a correct regular expression\nin the 'pattern' argument to select correct images.",
                 fixed = TRUE)
    expect_error(.valid.loadImage.input(x = cur_dir, pattern = 1, single_channel = FALSE),
                 regexp = "Please provide a single character,\ncharacter vector or factor as pattern input.",
                 fixed = TRUE)
    expect_error(.valid.loadImage.input(x = cur_dir, pattern = 1, single_channel = FALSE),
                 regexp = "Please provide a single character,\ncharacter vector or factor as pattern input.",
                 fixed = TRUE)
    expect_error(.valid.loadImage.input(x = cur_dir, pattern = "test", single_channel = FALSE),
                 regexp = "The pattern does not match any\nof the files in the provided directory.",
                 fixed = TRUE)
    expect_error(.valid.loadImage.input(x = cur_dir, pattern = "PancreasData", single_channel = FALSE),
                 regexp = "The provided path contains file-types other than\n'jpeg', 'tiff', 'png' or 'h5'.\nPlease provide a correct regular expression\nin the 'pattern' argument to select correct images.",
                 fixed = TRUE)
    expect_error(.valid.loadImage.input(x = cur_file, single_channel = FALSE),
                 regexp = "The provided file is not of type 'jpeg', 'tiff', 'png' or 'h5'.\nOther image types are not supported.",
                 fixed = TRUE)
    
    cur_files <- list.files(system.file("scripts",
                            package = "cytomapper"))
    expect_error(.valid.loadImage.input(x = cur_files, single_channel = FALSE),
                 regexp = "One or multiple files do not exist.\nPlease correct the input.",
                 fixed = TRUE)
    
    cur_files <- list.files(system.file("scripts",
                                        package = "cytomapper"),
                            full.names = TRUE)
    expect_error(.valid.loadImage.input(x = cur_files, single_channel = FALSE),
                 regexp = "The files are of type other than 'jpeg', 'tiff', 'png' or 'h5'.\nPlease only provide files of the supported file-type.",
                 fixed = TRUE)
    
    
    # Works
    cur_dir <- system.file("extdata", package = "cytomapper")
    cur_file <- system.file("extdata/E34_imc.tiff", package = "cytomapper")
    expect_silent(cur_out <- .valid.loadImage.input(x = cur_file, name = NULL, single_channel = FALSE))  
    expect_true(file.exists(cur_out))
    expect_equal(basename(cur_out), "E34_imc.tiff")
    expect_message(cur_out <- .valid.loadImage.input(x = cur_dir, pattern = NULL, name = NULL, single_channel = FALSE), regexp = "All files in the provided location will be read in.")  
    expect_true(all(file.exists(cur_out)))
    expect_equal(basename(cur_out), c("E34_imc.tiff", "E34_mask.tiff", "G01_imc.tiff", "G01_mask.tiff", "J02_imc.tiff", "J02_mask.tiff"))
    expect_silent(cur_out <- .valid.loadImage.input(x = cur_dir, pattern = ".tiff", name = NULL, single_channel = FALSE))  
    expect_true(all(file.exists(cur_out)))
    expect_equal(basename(cur_out), c("E34_imc.tiff", "E34_mask.tiff", "G01_imc.tiff", "G01_mask.tiff", "J02_imc.tiff", "J02_mask.tiff"))
    expect_silent(cur_out <- .valid.loadImage.input(x = cur_dir, pattern = "_imc", name = NULL, single_channel = FALSE))  
    expect_true(all(file.exists(cur_out)))
    expect_equal(basename(cur_out), c("E34_imc.tiff", "G01_imc.tiff", "J02_imc.tiff"))
    expect_silent(cur_out <- .valid.loadImage.input(x = cur_dir, pattern = c("G01_imc", "E34_imc"), name = NULL, single_channel = FALSE))  
    expect_true(all(file.exists(cur_out)))
    expect_equal(basename(cur_out), c("E34_imc.tiff", "G01_imc.tiff"))
    expect_silent(cur_out <- .valid.loadImage.input(x = cur_dir, pattern = factor(c("G01_imc", "E34_imc")), name = NULL, single_channel = FALSE))  
    expect_true(all(file.exists(cur_out)))
    expect_equal(basename(cur_out), c("E34_imc.tiff", "G01_imc.tiff"))
    expect_silent(cur_out <- .valid.loadImage.input(x = cur_dir, pattern = factor(c("G01_imc", "G01_imc", "E34_imc", "E34_imc")), name = NULL, single_channel = FALSE))  
    expect_true(all(file.exists(cur_out)))
    expect_equal(basename(cur_out), c("E34_imc.tiff", "G01_imc.tiff"))
    cur_files <- list.files(system.file("extdata",
                                        package = "cytomapper"), full.names = TRUE)
    expect_silent(cur_out <- .valid.loadImage.input(x = cur_files, name = NULL, single_channel = FALSE))  
    expect_true(all(file.exists(cur_out)))
    expect_equal(basename(cur_out), c("E34_imc.tiff", "E34_mask.tiff", "G01_imc.tiff", "G01_mask.tiff", "J02_imc.tiff", "J02_mask.tiff"))
})

test_that("Image setting validity check is correct.", {
    data("pancreasImages")
    
    # Error
    expect_error(.valid.Image.setting(value = "test"), 
                 regexp = "Invalid replacement operation: \nOnly 'Image' or 'CytoImageList' objects allowed.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = pancreasImages[1]), 
                 regexp = "Invalid replacement operation: \nCannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = pancreasImages[[1]]), 
                 regexp = "Invalid replacement operation: \nCannot set Image object to named CytoImageList.",
                 fixed = TRUE)
    cur_value <- pancreasImages
    names(cur_value) <- NULL
    expect_error(.valid.Image.setting(x = pancreasImages, i = 1, value = cur_value[1]), 
                 regexp = "Invalid replacement operation: \nCannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = "test", value = pancreasImages[1]), 
                 regexp = "Invalid replacement operation: \n'i' is of type character. \n This setting is only allowed for named CytoImageList objects.",
                 fixed = TRUE)
})

test_that("Channel setting validity check is correct.", {
    # Error
    expect_error(.valid.Channel.setting(value = "test"), 
                 regexp = "Invalid replacement operation: \nOnly 'CytoImageList' objects allowed.\nTo alter Image objects, see ?Image.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    expect_error(.valid.Channel.setting(x = cur_x, value = pancreasImages[1]), 
                 regexp = "Invalid replacement operation: \nReplacement needs to have same length as 'x'.",
                 fixed = TRUE)
    expect_error(.valid.Channel.setting(x = cur_x, i = 1:3, value = pancreasImages), 
                 regexp = "Invalid replacement operation: \nNumber of replacement channels is not the same as \nnumber of channels to replace.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    names(cur_x) <- c("test1", "test2", "test3")
    expect_error(.valid.Channel.setting(x = cur_x, i = 1:3, value = getChannels(pancreasImages, 1:3)), 
                 regexp = "Invalid replacement operation: \nNames of 'x' and 'value' do not match.",
                 fixed = TRUE)
    cur_x <- pancreasImages
    channelNames(cur_x) <- NULL
    expect_error(.valid.Channel.setting(x = cur_x, i = "test", value = getChannels(pancreasImages, 1)), 
                 regexp = "Invalid replacement operation: \nTrying to set a named channel in an unnamed CytoImageList.",
                 fixed = TRUE)
})

test_that("SingleCellExperiment validity check is correct.", {
    data("pancreasSCE")
    # Error
    expect_error(.valid.sce(object = "test"), 
                 regexp = "'object' is not of type 'SingleCellExperiment'.", 
                 fixed = TRUE)
    expect_error(.valid.sce(object = pancreasSCE, img_id = NULL, cell_id = "test"), 
                 regexp = "Please provide an 'img_id' and 'cell_id' argument", 
                 fixed = TRUE)
    expect_error(.valid.sce(object = pancreasSCE, img_id = "test", cell_id = NULL), 
                 regexp = "Please provide an 'img_id' and 'cell_id' argument", 
                 fixed = TRUE)
    expect_error(.valid.sce(object = pancreasSCE, img_id = "test", cell_id = "CellNb"), 
                 regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.", 
                 fixed = TRUE)
    expect_error(.valid.sce(object = pancreasSCE, img_id = "ImageNb", cell_id = "test"), 
                 regexp = "'img_id' and/or 'cell_id' not in 'colData(object)'.", 
                 fixed = TRUE)
    cur_sce <- pancreasSCE
    set.seed(1234)
    cur_sce$CellNb <- rnorm(ncol(cur_sce))   
    expect_error(.valid.sce(object = cur_sce, img_id = "ImageNb", cell_id = "CellNb"), 
                 regexp = "Cell ids should only contain numeric integer values.", 
                 fixed = TRUE)
    expect_error(.valid.sce(object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb", exprs_values = "test"), 
                 regexp = "'exprs_values' not an assay entry in 'object'.", 
                 fixed = TRUE)
})

test_that("Mask validity check is correct.", {
    data("pancreasMasks")
    data("pancreasImages")
    
    # Error
    expect_error(.valid.mask(mask = "test"),
                 regexp = "Please provide the segmentation mask(s)\nin form of a 'CytoImageList' object",
                 fixed = TRUE)
    expect_error(.valid.mask(mask = pancreasImages),
                 regexp = "Segmentation masks must only contain one channel.",
                 fixed = TRUE)
    expect_error(.valid.mask(mask = getChannels(pancreasImages, 1)),
                 regexp = "Segmentation masks must only contain integer values.",
                 fixed = TRUE)
    expect_error(.valid.mask(mask = pancreasMasks, img_id = "test"),
                 regexp = "'img_id' not in 'mcols(mask)'.",
                 fixed = TRUE)
    cur_mask <- pancreasMasks
    mcols(cur_mask)$test <- rep(1, 3)
    expect_error(.valid.mask(mask = cur_mask, img_id = "test"),
                 regexp = "Entries to in the 'mcols(mask)[,img_id]' slot are not unique.",
                 fixed = TRUE)
})

test_that("Image validity check is correct.", {
    data("pancreasImages")
  
    # Error
    expect_error(.valid.image(image = "test"),
                 regexp = "Please provide the image(s) in form of a 'CytoImageList' object",
                 fixed = TRUE)
    expect_error(.valid.image(image = pancreasImages, img_id = "test"),
                 regexp = "'img_id' not in 'mcols(image)'.",
                 fixed = TRUE)
    cur_images <- pancreasImages
    mcols(cur_images)$test <- rep(1, 3)
    expect_error(.valid.image(image = cur_images, img_id = "test"),
                 regexp = "Entries to in the 'mcols(image)[,img_id]' slot are not unique.",
                 fixed = TRUE)
})

test_that("Match objects for plotCells validity check is correct.", {
    data("pancreasMasks")
    data("pancreasSCE")
    
    # Error
    cur_mask <- pancreasMasks
    mcols(cur_mask)$ImageNb <- 7:9
    expect_error(.valid.matchObjects.plotCells(object = pancreasSCE, mask = cur_mask, img_id = "ImageNb"),
                 regexp = "None of the images appear in 'object'.\nPlease make sure to set the image ids correctly.",
                 fixed = TRUE)
})

test_that("Match objects for plotPixels validity check is correct.", {
    data("pancreasImages")
    data("pancreasMasks")
    data("pancreasSCE")
    
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = pancreasMasks, image = pancreasImages, img_id = NULL),
                 regexp = "'img_id' is missing.",
                 fixed = TRUE)
    cur_mask <- pancreasMasks
    mcols(cur_mask)$ImageNb <- 7:9
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = cur_mask, image = pancreasImages, img_id = "ImageNb"),
                 regexp = "Mask and image ids must be identical.",
                 fixed = TRUE)
    cur_mask <- pancreasMasks
    cur_mask[[1]] <- cur_mask[[1]][1:50, 1:50]
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = cur_mask, image = pancreasImages, img_id = "ImageNb"),
                 regexp = "Mask and image entries must have the same dimensions.",
                 fixed = TRUE) 
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = NULL, image = pancreasImages, img_id = NULL),
                 regexp = "'img_id' is missing.",
                 fixed = TRUE) 
    cur_images <- pancreasImages
    mcols(cur_images)$ImageNb <- 7:9
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = NULL, image = cur_images, img_id = "ImageNb"),
                 regexp = "Image ids in 'mcols(image)' and 'colData(object)' do not match",
                 fixed = TRUE)   
})

test_that("colour_by validity check is correct.", {
    data("pancreasSCE")
    data("pancreasImages")
    
    # Error
    expect_error(.valid.colour_by(colour_by = "test", object = NULL, image = NULL, call.arg = "plotCells"),
                 regexp = "Please provide a SingleCellExperiment 'object'.",
                 fixed = TRUE)
    cur_sce <- pancreasSCE
    colData(cur_sce) <- NULL
    expect_error(.valid.colour_by(colour_by = "CellType", object = cur_sce, image = NULL, call.arg = "plotCells"),
                 regexp = "'colour_by' not in 'rownames(object)' or the 'colData(object)' slot.",
                 fixed = TRUE)
    expect_error(.valid.colour_by(colour_by = c("CellType", "H3"), object = pancreasSCE, image = NULL, call.arg = "plotCells"),
                 regexp = "'colour_by' entries found in 'rownames(object)'\nand 'colData(object)' slot.\nPlease select either rownames or colData entries.",
                 fixed = TRUE)
    expect_error(.valid.colour_by(colour_by = c("CellType", "test"), object = pancreasSCE, image = NULL, call.arg = "plotCells"),
                 regexp = "'colour_by' not in 'rownames(object)' or the 'colData(object)' slot.",
                 fixed = TRUE)
    expect_error(.valid.colour_by(colour_by = c("CellType", "Area"), object = pancreasSCE, image = NULL, call.arg = "plotCells"),
                 regexp = "Only one 'colour_by' entry allowed when selecting a 'colData(object)' slot.",
                 fixed = TRUE)
    cur_sce <- rbind(pancreasSCE, pancreasSCE)
    rownames(cur_sce) <- paste0("test", 1:10)
    expect_error(.valid.colour_by(colour_by = paste0("test", 1:7), object = cur_sce, image = NULL, call.arg = "plotCells"),
                 regexp = "Only six 'colour_by' entries allowed when selecting marker expression.",
                 fixed = TRUE)
    cur_images <- pancreasImages
    channelNames(cur_images) <- NULL
    expect_error(.valid.colour_by(colour_by = NULL, object = NULL, image = cur_images, call.arg = "plotPixels"),
                 regexp = "'channelNames(image)' not set.",
                 fixed = TRUE)
    expect_error(.valid.colour_by(colour_by = "test", object = NULL, image = pancreasImages, call.arg = "plotPixels"),
                 regexp = "'colour_by' not in 'channelNames(image)' slot.",
                 fixed = TRUE)
})

test_that("outline_by validity check is correct.", {
    data("pancreasImages")
    data("pancreasMasks")
    data("pancreasSCE")
    
    # Error
    expect_error(.valid.outline_by(outline_by = "CellType", image = pancreasImages, object = pancreasSCE, mask = NULL), 
                 regexp = "Outlining cells: provide a SingleCellExperiment 'object' \nand segmentation 'mask' object.",
                 fixed = TRUE)
    expect_error(.valid.outline_by(outline_by = "CellType", image = pancreasImages, object = NULL, mask = pancreasSCE), 
                 regexp = "Outlining cells: provide a SingleCellExperiment 'object' \nand segmentation 'mask' object.",
                 fixed = TRUE)
    expect_error(.valid.outline_by(outline_by = "CellType", image = NULL, object = NULL, mask = pancreasSCE), 
                 regexp = "Please provide a SingleCellExperiment 'object'.",
                 fixed = TRUE)
    expect_error(.valid.outline_by(outline_by = c("CellType", "Area"), image = NULL, object = pancreasSCE, mask = pancreasSCE), 
                 regexp = "Only one 'outline_by' entry allowed.",
                 fixed = TRUE)
    cur_sce <- pancreasSCE
    colData(cur_sce) <- NULL
    expect_error(.valid.outline_by(outline_by = "CellType", image = NULL, object = cur_sce, mask = pancreasSCE), 
                 regexp = "outline_by' not in the 'colData(object)' slot.",
                 fixed = TRUE)
    expect_error(.valid.outline_by(outline_by = "test", image = NULL, object = pancreasSCE, mask = pancreasSCE), 
                 regexp = "'outline_by' not in 'colData(object)' slot.",
                 fixed = TRUE)
})

test_that("colour validity check is correct.", {
    data("pancreasSCE")
    data("pancreasImages")
    
    # Error
    expect_error(.valid.colour(colour = "test"), 
                 regexp = "'colour' is a list of entries in which each name specifies\nan entry of 'colour_by' and/or 'outline_by'", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list("test")), 
                 regexp = "'colour': please specify the entries that should be coloured.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(CellType = NULL), colour_by = "CellType", outline_by = NULL), 
                 regexp = "Empty entries not allowed in 'colour'", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(H3 = "test"), colour_by = c("H3", "CD99"), 
                               outline_by = NULL, image = pancreasImages), 
                 regexp = "Please specify colour gradients for all features.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(H3 = "test"), colour_by = c("H3", "CD99"), 
                               outline_by = NULL, image = NULL, object = pancreasSCE), 
                 regexp = "Please specify colour gradients for all features.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(H3 = c("black", "green"),
                                             CD99 = "red"), colour_by = c("H3", "CD99"), 
                               outline_by = NULL, image = NULL, object = pancreasSCE), 
                 regexp = "Please specify at least two colours when colouring features.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(H3 = c("black", "green"),
                                             CD99 = "red"), colour_by = c("H3", "CD99"), 
                               outline_by = NULL, image = pancreasImages, object = NULL), 
                 regexp = "Please specify at least two colours when colouring features.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(Area = "red"), colour_by = "Area", 
                               outline_by = NULL, image = NULL, object = pancreasSCE), 
                 regexp = "Please specify at least two colours when colouring continous entries.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(CellType = "red"), colour_by = "CellType", 
                               outline_by = NULL, image = NULL, object = pancreasSCE), 
                 regexp = "Please specify colours for all 'colour_by' levels.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(CellType = c(celltype_A = "red")), colour_by = "CellType", 
                               outline_by = NULL, image = NULL, object = pancreasSCE), 
                 regexp = "Please specify colours for all 'colour_by' levels.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(Area = "red"), colour_by = NULL, 
                               outline_by = "Area", image = NULL, object = pancreasSCE), 
                 regexp = "Please specify at least two colours when colouring continous entries.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(CellType = "red"), colour_by = NULL, 
                               outline_by = "CellType", image = NULL, object = pancreasSCE), 
                 regexp = "Please specify colours for all 'outline_by' levels.", 
                 fixed = TRUE)
    expect_error(.valid.colour(colour = list(CellType = c(celltype_A = "red")), colour_by = NULL, 
                               outline_by = "CellType", image = NULL, object = pancreasSCE), 
                 regexp = "Please specify colours for all 'outline_by' levels.", 
                 fixed = TRUE)

    
})

test_that("bcg input validity check is correct.", {
    # Error
    expect_error(.valid.bcg(bcg = "test"), 
                 regexp = "'bcg': please specify a list object",
                 fixed = TRUE)
    expect_error(.valid.bcg(bcg = list("test")), 
                 regexp = "'bcg': please indicate which channels to modify",
                 fixed = TRUE)
    expect_error(.valid.bcg(bcg = list(test = "test"), colour_by = NULL), 
                 regexp = "'colour_by': please indicate which channels to modify",
                 fixed = TRUE)
    expect_error(.valid.bcg(bcg = list(H3 = "test", CD99 = "test"), colour_by = c("test1", "test2")), 
                 regexp = "'bcg': names do not match 'colour_by' argument",
                 fixed = TRUE)
    expect_error(.valid.bcg(bcg = list(H3 = 1, CD99 = c(1,2,3)), colour_by = c("H3", "CD99")), 
                 regexp = "'bcg': specify in form of c(0,1,1)",
                 fixed = TRUE)
    expect_error(.valid.bcg(bcg = list(H3 = c("test", "test", "test"), CD99 = c(1,2,3)), colour_by = c("H3", "CD99")), 
                 regexp = "'bcg': specify in form of numeric entries",
                 fixed = TRUE)
})


