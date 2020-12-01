test_that("scale bar parameters are correctly set.", {
    # Error
    expect_error(.valid.scalebar(scalebar = "test"),
                regexp = "Invalid entry to the 'scale_bar' list object",
                fixed = TRUE)
    
    expect_error(.valid.scalebar(scalebar = list(test = "test")),
                 regexp = "Invalid entry to the 'scale_bar' list object",
                 fixed = TRUE)
    
    expect_error(.valid.scalebar(scalebar = list(position = "test")),
                 regexp = "Invalid entry to the 'scale_bar' list object: \nposition not correctly specified",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(margin = "test")),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'margin' should contain two numeric elements corresponding to x and y margin",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(margin = 1)),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'margin' should contain two numeric elements corresponding to x and y margin",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(length = "test")),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'length' should be numeric and of length 1",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(length = 1:2)),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'length' should be numeric and of length 1",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(cex = "test")),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(cex = c(1,2))),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(lwidth = "test")),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'lwidth' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(lwidth = 1:2)),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'lwidth' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(label = 1:2)),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'label' should be a single entry",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(colour = 1)),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'colour' should be a single character entry",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(colour = c("test1", "test2"))),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'colour' should be a single character entry",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(frame = "frame")),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'frame' should be a single integer or set to 'all'",
                 fixed = TRUE)
    expect_error(.valid.scalebar(scalebar = list(frame = 1:2)),
                 regexp = "Invalid entry to the 'scale_bar' list object: \n'frame' should be a single integer or set to 'all'",
                 fixed = TRUE)
    
    # Works
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(position = "topleft")))
    expect_equal(cur_out$position, "topleft")
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(position = "topright")))
    expect_equal(cur_out$position, "topright")
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(position = "bottomleft")))
    expect_equal(cur_out$position, "bottomleft")
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(position = "bottomright")))
    expect_equal(cur_out$position, "bottomright")
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(margin = c(1,2))))
    expect_equal(cur_out$margin, c(1,2)) 
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(length = 10)))
    expect_equal(cur_out$length, 10) 
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(cex = 2)))
    expect_equal(cur_out$cex, 2) 
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(lwidth = 2)))
    expect_equal(cur_out$lwidth, 2) 
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(label = "test")))
    expect_equal(cur_out$label, "test") 
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(colour = "test")))
    expect_equal(cur_out$colour, "test") 
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(frame = 1)))
    expect_equal(cur_out$frame, 1) 
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(frame = "all")))
    expect_equal(cur_out$frame, "all") 
    
    # Default output
    expect_silent(cur_out <- .valid.scalebar(scalebar = NULL))
    expect_null(cur_out)
    expect_silent(cur_out <- .valid.scalebar(scalebar = list(position = "topleft")))
    expect_equal(cur_out, list(position = "topleft",
                               margin = c(10, 10),
                               colour = "white",
                               frame = "all"))
})

test_that("image title parameters are correctly set.", {
    data("pancreasImages")
    # Error
    expect_error(.valid.imagetitle(imagetitle = "test", image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object",
                 fixed = TRUE)
    
    expect_error(.valid.imagetitle(imagetitle = list(test = "test"), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object",
                 fixed = TRUE)
    
    expect_error(.valid.imagetitle(imagetitle = list(text = "test"), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \nPlease specify one title per image.",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(position = "test"), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \nposition not correctly specified",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(font = "test"), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'font' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(font = 1:2), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'font' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(cex = "test"), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(cex = 1:2), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(cex = "test"), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(cex = c(1,2)), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(margin = "test"), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'margin' should contain two numeric elements corresponding to x and y margin",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(margin = 1), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'margin' should contain two numeric elements corresponding to x and y margin",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(colour = 1), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'colour' should be a single character entry",
                 fixed = TRUE)
    expect_error(.valid.imagetitle(imagetitle = list(colour = c("test1", "test2")), image = pancreasImages),
                 regexp = "Invalid entry to the 'image_title' list object: \n'colour' should be a single character entry",
                 fixed = TRUE)
    
    # Works
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(text = paste0("test", 1:3)), image = pancreasImages))
    expect_equal(cur_out$text, paste0("test", 1:3))
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "top"), image = pancreasImages))
    expect_equal(cur_out$position, "top")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "bottom"), image = pancreasImages))
    expect_equal(cur_out$position, "bottom")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "topleft"), image = pancreasImages))
    expect_equal(cur_out$position, "topleft")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "bottomleft"), image = pancreasImages))
    expect_equal(cur_out$position, "bottomleft")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "topright"), image = pancreasImages))
    expect_equal(cur_out$position, "topright")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "bottomright"), image = pancreasImages))
    expect_equal(cur_out$position, "bottomright")
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(margin = c(1,2)), image = pancreasImages))
    expect_equal(cur_out$margin, c(1,2)) 
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(cex = 2), image = pancreasImages))
    expect_equal(cur_out$cex, 2) 
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(font = 2), image = pancreasImages))
    expect_equal(cur_out$font, 2) 
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(colour = "test"), image = pancreasImages))
    expect_equal(cur_out$colour, "test") 
    
    # Default output
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = NULL, image = pancreasImages))
    expect_null(cur_out)
    expect_silent(cur_out <- .valid.imagetitle(imagetitle = list(position = "topleft"), image = pancreasImages))
    expect_equal(cur_out, list(position = "topleft",
                               font = 2,
                               margin = c(10, 10),
                               colour = "white"))
})

test_that("legend parameters are correctly set.", {
    # Error
    expect_error(.valid.legendparam(legendparam = "test"),
                 regexp = "Invalid entry to the 'legend' list object",
                 fixed = TRUE)
    
    expect_error(.valid.legendparam(legendparam = list(test = "test")),
                 regexp = "Invalid entry to the 'legend' list object",
                 fixed = TRUE)
    
    expect_error(.valid.legendparam(legendparam = list(colour_by.title.font = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'colour_by.title.font' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(colour_by.title.font = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'colour_by.title.font' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(colour_by.title.cex = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'colour_by.title.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(colour_by.title.cex = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'colour_by.title.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(colour_by.labels.cex = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'colour_by.labels.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(colour_by.labels.cex = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'colour_by.labels.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(colour_by.legend.cex = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'colour_by.legend.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(colour_by.legend.cex = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'colour_by.legend.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(outline_by.title.font = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'outline_by.title.font' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(outline_by.title.font = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'outline_by.title.font' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(outline_by.title.cex = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'outline_by.title.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(outline_by.title.cex = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'outline_by.title.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(outline_by.labels.cex = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'outline_by.labels.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(outline_by.labels.cex = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'outline_by.labels.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(outline_by.legend.cex = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'outline_by.legend.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(outline_by.legend.cex = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'outline_by.legend.cex' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(margin = "test")),
                 regexp = "Invalid entry to the 'legend' list object: \n'margin' should be a single number",
                 fixed = TRUE)
    expect_error(.valid.legendparam(legendparam = list(margin = 1:2)),
                 regexp = "Invalid entry to the 'legend' list object: \n'margin' should be a single number",
                 fixed = TRUE)
    
    
    # Works
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(colour_by.title.font = 1)))
    expect_equal(cur_out$colour_by.title.font, 1)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(colour_by.title.cex = 1)))
    expect_equal(cur_out$colour_by.title.cex, 1)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(colour_by.labels.cex = 1)))
    expect_equal(cur_out$colour_by.labels.cex, 1)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(colour_by.legend.cex = 1)))
    expect_equal(cur_out$colour_by.legend.cex, 1)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(outline_by.title.font = 1)))
    expect_equal(cur_out$outline_by.title.font, 1)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(outline_by.title.cex = 1)))
    expect_equal(cur_out$outline_by.title.cex, 1)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(outline_by.labels.cex = 1)))
    expect_equal(cur_out$outline_by.labels.cex, 1)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(outline_by.legend.cex = 1)))
    expect_equal(cur_out$outline_by.legend.cex, 1)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(margin = 1)))
    expect_equal(cur_out$margin, 1)
    
    # Default output
    expect_silent(cur_out <- .valid.legendparam(legendparam = NULL))
    expect_null(cur_out)
    expect_silent(cur_out <- .valid.legendparam(legendparam = list(colour_by.title.font = 1)))
    expect_equal(cur_out, list(colour_by.title.font = 1,
                               outline_by.title.font = 1,
                               margin = 2))
})

test_that("missing colour parameters are correctly set.", {
    expect_silent(cur_out <- .valid.missingcolour(missingcolour = NULL))
    expect_equal(cur_out, "gray")

    expect_error(.valid.missingcolour(missingcolour = "test"),
                 regexp = "'missing_colour' not a valid colour.",
                 fixed = TRUE)
    expect_error(.valid.missingcolour(missingcolour = c("red", "green")),
                 regexp = "'missing_colour' not a valid colour.",
                 fixed = TRUE)
})

test_that("background colour parameters are correctly set.", {
    expect_silent(cur_out <- .valid.backgroundcolour(backgroundcolour = NULL))
    expect_equal(cur_out, "#000000")
    
    expect_error(.valid.backgroundcolour(backgroundcolour = "test"),
                 regexp = "'background_colour' not a valid colour.",
                 fixed = TRUE)
    expect_error(.valid.backgroundcolour(backgroundcolour = c("red", "green")),
                 regexp = "'background_colour' not a valid colour.",
                 fixed = TRUE)
})

test_that("save plot parameters are correctly set.", {
    # Error
    expect_error(.valid.saveplot(saveplot = "test"),
                 regexp = "Invalid entry to the 'save_plot' list object",
                 fixed = TRUE)
    
    expect_error(.valid.saveplot(saveplot = list(test = "test")),
                 regexp = "Invalid entry to the 'save_plot' list object",
                 fixed = TRUE)
    
    expect_error(.valid.saveplot(saveplot = list(filename = "test")),
                 regexp = "Invalid entry to the 'save_plot' list object: \nPlease provide a file extension indicating the format to save the image.",
                 fixed = TRUE)
    expect_error(.valid.saveplot(saveplot = list(filename = 1)),
                 regexp = "Invalid entry to the 'save_plot' list object: \nInvalid entry of 'filename'",
                 fixed = TRUE)
    expect_error(.valid.saveplot(saveplot = list(filename = c("test.png", "test2.png"))),
                 regexp = "Invalid entry to the 'save_plot' list object: \nInvalid entry of 'filename'",
                 fixed = TRUE)
    expect_error(.valid.saveplot(saveplot = list(filename = "test.test")),
                 regexp = "Invalid entry to the 'save_plot' list object: \n'filename' only supports 'tiff', 'png' and 'jpeg' file types.",
                 fixed = TRUE)
    expect_error(.valid.saveplot(saveplot = list(scale = 1)),
                 regexp = "Invalid entry to the 'save_plot' list object: \n'filename' not provided.",
                 fixed = TRUE)
    expect_error(.valid.saveplot(saveplot = list(filename = "test.png", scale = 1:2)),
                 regexp = "Invalid entry to the 'save_plot' list object: \nInvalid entry of 'scale'",
                 fixed = TRUE)
    expect_error(.valid.saveplot(saveplot = list(filename = "test.png", scale = "test")),
                 regexp = "Invalid entry to the 'save_plot' list object: \nInvalid entry of 'scale'",
                 fixed = TRUE)

    # Works
    expect_silent(cur_out <- .valid.saveplot(saveplot = list(filename = "test.png")))
    expect_equal(cur_out$filename, "test.png")
    expect_silent(cur_out <- .valid.saveplot(saveplot = list(filename = "test.png", scale = 3)))
    expect_equal(cur_out$scale, 3)
    
    # Default
    expect_silent(cur_out <- .valid.saveplot(saveplot = list(filename = "test.png")))
    expect_equal(cur_out, list(filename = "test.png", scale = 1))
})

test_that("all parameters are correctly set.", {
    data("pancreasImages")
    
    # Error
    expect_error(.plottingParam(dotArgs = list(test = "test"), image = pancreasImages),
                 regexp = "Entries 'test' are not supported",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(return_plot = "test"), image = pancreasImages),
                 regexp = "Invalid 'return_plot' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(return_images = "test"), image = pancreasImages),
                 regexp = "Invalid 'return_images' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(margin = "test"), image = pancreasImages),
                 regexp = "Invalid 'margin' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(margin = 1:2), image = pancreasImages),
                 regexp = "Invalid 'margin' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(margin = -1), image = pancreasImages),
                 regexp = "Invalid 'margin' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(display = "test"), image = pancreasImages),
                 regexp = "Invalid 'display' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(scale = "test"), image = pancreasImages),
                 regexp = "Invalid 'scale' entry.",
                 fixed = TRUE)
    expect_error(.plottingParam(dotArgs = list(interpolate = "test"), image = pancreasImages),
                 regexp = "Invalid 'interpolate' entry.",
                 fixed = TRUE)
    
    # Works 
    expect_silent(cur_out <- .plottingParam(dotArgs = list(return_plot = TRUE), image = pancreasImages))
    expect_equal(cur_out$return_plot, TRUE)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(return_images = TRUE), image = pancreasImages))
    expect_equal(cur_out$return_images, TRUE)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(display = "single"), image = pancreasImages))
    expect_equal(cur_out$display, "single")
    expect_silent(cur_out <- .plottingParam(dotArgs = list(display = "all"), image = pancreasImages))
    expect_equal(cur_out$display, "all")
    expect_silent(cur_out <- .plottingParam(dotArgs = list(scale = TRUE), image = pancreasImages))
    expect_equal(cur_out$scale, TRUE)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(interpolate = TRUE), image = pancreasImages))
    expect_equal(cur_out$interpolate, TRUE)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(margin = 1), image = pancreasImages))
    expect_equal(cur_out$margin, 1)
    expect_silent(cur_out <- .plottingParam(dotArgs = list(margin = 0), image = pancreasImages))
    expect_equal(cur_out$margin, 0)
    
    # Defaults
    expect_silent(cur_out <- .plottingParam(dotArgs = list(), image = pancreasImages))
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




