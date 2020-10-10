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
    expect_error(cytomapperShiny(object = pancreasSCE, image = pancreasImages, 
                                 cell_id = "CellNb", img_id = "ImageNb"), 
                 regexp = "Please provide a mask object.",
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

test_that("server-utils: auxiliary functions work", {
    # Test colouring function
    expect_equal(.create_colours(1), "#70C389")
    expect_equal(.create_colours(2), "#39BEB4")
    expect_equal(.create_colours(3), "#3F85A7") 
    expect_equal(.create_colours(4), "#494579") 
    expect_equal(.create_colours(5), "#5B1C55")
    expect_equal(.create_colours(6), "#971B4B") 
    expect_equal(.create_colours(7), "#C81F43") 
    expect_equal(.create_colours(8), "#F26738") 
    expect_equal(.create_colours(9), "#F79C1D") 
    expect_equal(.create_colours(10), "#F7CD0F")
    expect_equal(.create_colours(11), "#EBE24A") 
    expect_equal(.create_colours(12), "#B4D55A")
    expect_true(is.na(.create_colours(13)))
    
    # Brush range
    cur_brush <- NULL
    expect_equal(.brushRange(cur_brush), NULL)
    
    cur_brush <- list(xmin = 1.2, xmax = 1.7,
                      ymin = 1.0, ymax = 2.0)
    expect_equal(.brushRange(cur_brush), "xmin = 1.2 xmax = 1.7 ymin = 1 ymax = 2")
    
    # General help
    cur_out <-.general_help()
    expect_length(cur_out, 16)
    expect_equal(unlist(cur_out[[1]]$children), "Using the Shiny application")
    expect_equal(unlist(cur_out[[3]]$children), "1. Select the number of plots")
    expect_equal(unlist(cur_out[[5]]$children), "2. Select the sample")
    expect_equal(unlist(cur_out[[7]]$children), "3. Select the markers")
    expect_equal(unlist(cur_out[[9]]$children), "4. Gate cells")
    expect_equal(unlist(cur_out[[11]]$children), "5. Observe the selected cells")
    expect_equal(unlist(cur_out[[13]]$children), "6. Change samples")
    expect_equal(unlist(cur_out[[15]]$children), "7. Save the selected cells")
})
