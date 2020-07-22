test_that("cytomapperShiny: Standard input testing works", {
    data("pancreasSCE")
    data("pancreasMasks")
    data("pancreasImages")
    
    # Fail
    expect_error(cytomapperShiny(object = "test"), 
                 regexp = "'object' is not of type 'SingleCellExperiment'.",
                 fixed = TRUE)
    expect_error(cytomapperShiny(object = pancreasSCE), 
                 regexp = "Please provide an 'img_id' argument.")
    expect_error(cytomapperShiny(object = pancreasSCE, img_id = c("test", 2), cell_id = "CellNb"), 
                 regexp = "Invalid argument for 'img_id'.",
                 fixed = TRUE)
    expect_error(cytomapperShiny(object = pancreasSCE, img_id = 2, cell_id = "CellNb"), 
                 regexp = "Invalid argument for 'img_id'.",
                 fixed = TRUE)
    expect_error(cytomapperShiny(object = pancreasSCE, img_id = "test", cell_id = "CellNb"), 
                 regexp = "'img_id' not in 'colData(object)'.",
                 fixed = TRUE)
    expect_error(cytomapperShiny(object = pancreasSCE, img_id = "ImageNb"), 
                 regexp = "Please provide a 'cell_id' argument.",
                 fixed = TRUE)
    expect_error(cytomapperShiny(object = pancreasSCE, cell_id = c("test", 2), img_id = "ImageNb"), 
                 regexp = "Invalid argument for 'cell_id'.",
                 fixed = TRUE)
    expect_error(cytomapperShiny(object = pancreasSCE, cell_id = "test", img_id = "ImageNb"), 
                 regexp = "'cell_id' not in 'colData(object)'.",
                 fixed = TRUE)
    
    cur_obj <- pancreasSCE
    rownames(cur_obj) <- NULL
    expect_error(cytomapperShiny(object = cur_obj, img_id = "ImageNb", cell_id = "CellNb"), 
                 regexp = "Please specify the rownames of the 'object'.",
                 fixed = TRUE)
    rownames(cur_obj) <- paste0("test", 1:5)
    expect_error(cytomapperShiny(object = cur_obj, image = pancreasImages, img_id = "ImageNb", cell_id = "CellNb"), 
                 regexp = "The 'channelNames' of the images need to match the rownames of the object.",
                 fixed = TRUE)
    cur_images <- pancreasImages
    channelNames(cur_images) <- NULL
    expect_error(cytomapperShiny(object = pancreasSCE, image = cur_images, img_id = "ImageNb", cell_id = "CellNb"), 
                 regexp = "Please specify the 'channelNames' of the 'image' object.",
                 fixed = TRUE)
    cur_obj <- pancreasSCE
    metadata(cur_obj) <- data.frame()
    expect_warning(cytomapperShiny(object = cur_obj, mask = pancreasMasks, image = pancreasImages, img_id = "ImageNb", cell_id = "CellNb"), 
                   regexp = "metadata('object') will be stored as 'list' in the metadata slot of the output object.",
                   fixed = TRUE)
    
    cur_images <- pancreasImages
    cur_masks <- pancreasMasks
    cur_images[1] <- NULL
    cur_masks[1] <- NULL
    expect_error(cytomapperShiny(object = pancreasSCE, image = cur_images, mask = cur_masks, img_id = "ImageNb", cell_id = "CellNb"), 
                 regexp = "Please provide a unique image/mask for every sample stored in 'object'.",
                 fixed = TRUE)
    
    # Pass
})
