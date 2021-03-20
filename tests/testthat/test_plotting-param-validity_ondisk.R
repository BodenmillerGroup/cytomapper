test_that("On disk: image title parameters are correctly set.", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Error
    expect_error(.valid.imagetitle(imagetitle = "test", image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object",
                 fixed = TRUE)
    
    expect_error(.valid.imagetitle(imagetitle = list(test = "test"), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object",
                 fixed = TRUE)
    
    expect_error(.valid.imagetitle(imagetitle = list(text = "test"), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \nPlease specify one title per image.",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(position = "test"), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \nposition not correctly specified",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(font = "test"), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'font' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(font = 1:2), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'font' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(cex = "test"), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(cex = 1:2), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(cex = "test"), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(cex = c(1,2)), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(margin = "test"), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'margin' should contain two numeric elements corresponding to x and y margin",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(margin = 1), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'margin' should contain two numeric elements corresponding to x and y margin",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(colour = 1), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'colour' should be a single character entry",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(colour = c("test1", "test2")), image = cur_Images),
                 regexp = "Invalid entry to the 'image_title' list object: \n'colour' should be a single character entry",
                 fixed = TRUE)
    
    # Works
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(text = paste0("test", 1:3)), image = cur_Images))
    expect_equal(cur_out$text, paste0("test", 1:3))
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "top"), image = cur_Images))
    expect_equal(cur_out$position, "top")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "bottom"), image = cur_Images))
    expect_equal(cur_out$position, "bottom")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "topleft"), image = cur_Images))
    expect_equal(cur_out$position, "topleft")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "bottomleft"), image = cur_Images))
    expect_equal(cur_out$position, "bottomleft")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "topright"), image = cur_Images))
    expect_equal(cur_out$position, "topright")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "bottomright"), image = cur_Images))
    expect_equal(cur_out$position, "bottomright")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(margin = c(1,2)), image = cur_Images))
    expect_equal(cur_out$margin, c(1,2)) 
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(cex = 2), image = cur_Images))
    expect_equal(cur_out$cex, 2) 
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(font = 2), image = cur_Images))
    expect_equal(cur_out$font, 2) 
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(colour = "test"), image = cur_Images))
    expect_equal(cur_out$colour, "test") 
    
    # Default output
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = NULL, image = cur_Images))
    expect_null(cur_out)
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "topleft"), image = cur_Images))
    expect_equal(cur_out, list(position = "topleft",
                               font = 2,
                               margin = c(10, 10),
                               colour = "white"))
})


test_that("On disk: all parameters are correctly set.", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Error
    expect_error(.plottingParam(dotArgs = list(test = "test"), image = cur_Images),
                 regexp = "Entries 'test' are not supported",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(return_plot = "test"), image = cur_Images),
                 regexp = "Invalid 'return_plot' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(return_images = "test"), image = cur_Images),
                 regexp = "Invalid 'return_images' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(margin = "test"), image = cur_Images),
                 regexp = "Invalid 'margin' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(margin = 1:2), image = cur_Images),
                 regexp = "Invalid 'margin' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(margin = -1), image = cur_Images),
                 regexp = "Invalid 'margin' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(display = "test"), image = cur_Images),
                 regexp = "Invalid 'display' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(scale = "test"), image = cur_Images),
                 regexp = "Invalid 'scale' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(interpolate = "test"), image = cur_Images),
                 regexp = "Invalid 'interpolate' entry.",
                 fixed = TRUE)
    
    # Works 
    expect_silent(cur_out <- .plottingParam(dotArgs = list(return_plot = TRUE), image = cur_Images))
    expect_equal(cur_out$return_plot, TRUE)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(return_images = TRUE), image = cur_Images))
    expect_equal(cur_out$return_images, TRUE)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(display = "single"), image = cur_Images))
    expect_equal(cur_out$display, "single")
    expect_silent(cur_out <- .plottingParam(dotArgs = list(display = "all"), image = cur_Images))
    expect_equal(cur_out$display, "all")
    expect_silent(cur_out <- .plottingParam(dotArgs = list(scale = TRUE), image = cur_Images))
    expect_equal(cur_out$scale, TRUE)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(interpolate = TRUE), image = cur_Images))
    expect_equal(cur_out$interpolate, TRUE)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(margin = 1), image = cur_Images))
    expect_equal(cur_out$margin, 1)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(margin = 0), image = cur_Images))
    expect_equal(cur_out$margin, 0)
    
    # Defaults
    expect_silent(cur_out <- .plottingParam(dotArgs = list(), image = cur_Images))
    expect_equal(cur_out, list(scale_bar = list(length = NULL, label = NULL, cex = NULL, lwidth = NULL, colour = "white", 
                                                position = "bottomright", margin = c(10, 10), frame = "all"),
                               image_title = list(text = NULL, position = "top", colour = "white", margin = c(10, 10),
                                                  font = 2, cex = NULL),
                               legend = list(colour_by.title.font = 1, colour_by.title.cex = NULL,
                                             colour_by.labels.cex = NULL, colour_by.legend.cex = NULL,
                                             outline_by.title.font = 1, outline_by.title.cex = NULL,
                                             outline_by.labels.cex = NULL, outline_by.legend.cex = NULL,
                                             margin = 2),
                               return_plot = FALSE,
                               return_images = FALSE,
                               margin = 0,
                               display = "all",
                               scale = TRUE,
                               interpolate = TRUE,
                               missing_colour = "gray",
                               background_colour = "#000000",
                               thick = FALSE))
})




