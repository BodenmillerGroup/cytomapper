test_that("On disk: Image setting validity check is correct.", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Error
    expect_error(.valid.Image.setting(value = "test"), 
                 regexp = "Invalid replacement operation: \nOnly 'Image' or 'CytoImageList' objects allowed.",
                 fixed = TRUE)
    cur_x <- cur_Images
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = cur_Images[1]), 
                 regexp = "Invalid replacement operation: \nCannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- cur_Images
    expect_error(.valid.Image.setting(x = cur_x, i = 1, value = cur_Images[[1]]), 
                 regexp = "Invalid replacement operation: \nCannot set Image object to named CytoImageList.",
                 fixed = TRUE)
    cur_value <- cur_Images
    names(cur_value) <- NULL
    expect_error(.valid.Image.setting(x = cur_Images, i = 1, value = cur_value[1]), 
                 regexp = "Invalid replacement operation: \nCannot merge named and unnamed CytoImageList object.",
                 fixed = TRUE)
    cur_x <- cur_Images
    names(cur_x) <- NULL
    expect_error(.valid.Image.setting(x = cur_x, i = "test", value = cur_Images[1]), 
                 regexp = "Invalid replacement operation: \n'i' is of type character. \n This setting is only allowed for named CytoImageList objects.",
                 fixed = TRUE)
})

test_that("On disk: Mask validity check is correct.", {
    data("pancreasMasks")
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Error
    expect_error(.valid.mask(mask = "test"),
                 regexp = "Please provide the segmentation mask(s)\nin form of a 'CytoImageList' object",
                 fixed = TRUE)
    expect_error(.valid.mask(mask = cur_Images),
                 regexp = "Segmentation masks must only contain integer values.",
                 fixed = TRUE)
    expect_error(.valid.mask(mask = getChannels(cur_Images, 1)),
                 regexp = "Segmentation masks must only contain integer values.",
                 fixed = TRUE)
    expect_error(.valid.mask(mask = cur_Masks, img_id = "test"),
                 regexp = "'img_id' not in 'mcols(mask)'.",
                 fixed = TRUE)
    cur_mask <- cur_Masks
    mcols(cur_mask)$test <- rep(1, 3)
    expect_error(.valid.mask(mask = cur_mask, img_id = "test"),
                 regexp = "Entries to in the 'mcols(mask)[,img_id]' slot are not unique.",
                 fixed = TRUE)
})

test_that("On disk: Image validity check is correct.", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  
    # Error
    expect_error(.valid.image(image = "test"),
                 regexp = "Please provide the image(s) in form of a 'CytoImageList' object",
                 fixed = TRUE)
    expect_error(.valid.image(image = cur_Images, img_id = "test"),
                 regexp = "'img_id' not in 'mcols(image)'.",
                 fixed = TRUE)
    cur_images <- cur_Images
    mcols(cur_images)$test <- rep(1, 3)
    expect_error(.valid.image(image = cur_images, img_id = "test"),
                 regexp = "Entries to in the 'mcols(image)[,img_id]' slot are not unique.",
                 fixed = TRUE)
})

test_that("On disk: Match objects for plotCells validity check is correct.", {
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Error
    cur_mask <- cur_Masks
    mcols(cur_mask)$ImageNb <- 7:9
    expect_error(.valid.matchObjects.plotCells(object = pancreasSCE, mask = cur_mask, img_id = "ImageNb"),
                 regexp = "None of the images appear in 'object'.\nPlease make sure to set the image ids correctly.",
                 fixed = TRUE)
})

test_that("On disk: Match objects for plotPixels validity check is correct.", {
    data("pancreasImages")
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = cur_Masks, image = cur_Images, img_id = NULL),
                 regexp = "'img_id' is missing.",
                 fixed = TRUE)
    cur_mask <- cur_Masks
    mcols(cur_mask)$ImageNb <- 7:9
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = cur_mask, image = cur_Images, img_id = "ImageNb"),
                 regexp = "Mask and image ids must be identical.",
                 fixed = TRUE)
    cur_mask <- cur_Masks
    cur_mask[[1]] <- cur_mask[[1]][1:50, 1:50]
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = cur_mask, image = cur_Images, img_id = "ImageNb"),
                 regexp = "Mask and image entries must have the same dimensions.",
                 fixed = TRUE) 
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = NULL, image = cur_Images, img_id = NULL),
                 regexp = "'img_id' is missing.",
                 fixed = TRUE) 
    cur_images <- cur_Images
    mcols(cur_images)$ImageNb <- 7:9
    expect_error(.valid.matchObjects.plotPixels(object = pancreasSCE, mask = NULL, image = cur_images, img_id = "ImageNb"),
                 regexp = "Image ids in 'mcols(image)' and 'colData(object)' do not match",
                 fixed = TRUE)   
})

test_that("On disk: colour_by validity check is correct.", {
    data("pancreasSCE")
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
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
    cur_images <- cur_Images
    channelNames(cur_images) <- NULL
    expect_error(.valid.colour_by(colour_by = NULL, object = NULL, image = cur_images, call.arg = "plotPixels"),
                 regexp = "'channelNames(image)' not set.",
                 fixed = TRUE)
    expect_error(.valid.colour_by(colour_by = "test", object = NULL, image = cur_Images, call.arg = "plotPixels"),
                 regexp = "'colour_by' not in 'channelNames(image)' slot.",
                 fixed = TRUE)
})

test_that("On disk: outline_by validity check is correct.", {
    data("pancreasImages")
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Error
    expect_error(.valid.outline_by(outline_by = "CellType", image = cur_Images, object = pancreasSCE, mask = NULL), 
                 regexp = "Outlining cells: provide a SingleCellExperiment 'object' \nand segmentation 'mask' object.",
                 fixed = TRUE)
    expect_error(.valid.outline_by(outline_by = "CellType", image = cur_Images, object = NULL, mask = pancreasSCE), 
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

test_that("On disk: colour validity check is correct.", {
    data("pancreasSCE")
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
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
                               outline_by = NULL, image = cur_Images), 
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
                               outline_by = NULL, image = cur_Images, object = NULL), 
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


