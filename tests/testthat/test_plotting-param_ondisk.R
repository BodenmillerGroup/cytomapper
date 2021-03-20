test_that("On disk: plotting-param: scale_bar can be set.", {
  data("pancreasMasks")
    
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
    
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  cur_images <- cur_Masks
  setImages(cur_images, "E34_mask") <- cur_images[[1]][1:50, 1:50,drop=FALSE]

  # Works
  expect_silent(plotCells(cur_images))
  expect_silent(plotCells(cur_images, scale_bar = NULL))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(length = 50)))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(label = "test")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(cex = 2)))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(lwidth = 5)))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(colour = "red")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "topleft")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "topright")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "bottomleft")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "bottomright")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(margin = c(1,1))))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "bottomleft",
                            margin = c(1,1))))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(position = "topright",
                                           margin = c(1,1))))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(frame = "all")))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(frame = 1)))
  expect_silent(plotCells(cur_images,
                          scale_bar = list(length = 10,
                                           label = "10 um",
                                           cex = 2,
                                           lwidth = 5,
                                           colour = "dark red",
                                           position = "bottomright",
                                           margin = c(20,20))))
})

test_that("On disk: plotting-param: image_title can be set.", {
  data("pancreasMasks")
    
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
    
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    
  cur_images <- cur_Masks
  setImages(cur_images, "E34_mask") <- cur_images[[1]][1:50, 1:50,drop=FALSE]

  # Works
  expect_silent(plotCells(cur_images))
  expect_silent(plotCells(cur_images, image_title = NULL))
  expect_silent(plotCells(cur_images,
          image_title = list(text = c("test1", "test2", "test3"))))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "top")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "bottom")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "topleft")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "bottomleft")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "topright")))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "bottomright")))
  expect_silent(plotCells(cur_images,
                          image_title = list(colour = "red")))
  expect_silent(plotCells(cur_images,
                          image_title = list(margin = c(0,0))))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "topright",
                                             margin = c(0,0))))
  expect_silent(plotCells(cur_images,
                          image_title = list(position = "bottomleft",
                                             margin = c(0,0))))
  expect_silent(plotCells(cur_images,
                          image_title = list(font = 1)))
  expect_silent(plotCells(cur_images,
                          image_title = list(cex = 3)))
})

test_that("On disk: plotting-param: missing_colour can be set.", {
  data("pancreasMasks")
  data("pancreasImages")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(plotCells(cur_Masks, missing_colour = "red"))
  expect_silent(plotPixels(image = cur_Images,
                           mask = cur_Masks,
                           img_id = "ImageNb",
                           cell_id = "CellNb",
                           missing_colour = "red"))

  # Error
  expect_error(plotCells(cur_Masks, missing_colour = "test"),
                regexp = "'missing_colour' not a valid colour.",
               fixed = TRUE)
})

test_that("On disk: plotting-param: background_colour can be set.", {
  data("pancreasMasks")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(plotCells(cur_Masks, background_colour = "white"))
  expect_silent(plotCells(cur_Masks, background_colour = "red"))

  # Error
  expect_error(plotCells(cur_Masks, background_colour = "test"),
               regexp = "'background_colour' not a valid colour.",
               fixed = TRUE)
})

test_that("On disk: plotting-param: save_plot can be set.", {
  data("pancreasImages")
    
  skip_on_ci()
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  

  # Works
  expect_silent(plotPixels(cur_Images, save_plot = NULL))

  # Test if par is correctly returned
  cur_par1 <- par()
  expect_silent(plotPixels(cur_Images,
                           save_plot = list(filename = paste0(cur_path, "/test.png"),
                                            scale = 10)))
  cur_par2 <- par()
  expect_identical(cur_par1, cur_par2)
  expect_true(file.exists(paste0(cur_path, "/test.png")))
  expect_silent(plotPixels(cur_Images,
                           save_plot = list(filename = paste0(cur_path, "/test.jpeg"),
                                             scale = 10)))
  expect_true(file.exists(paste0(cur_path, "/test.jpeg")))
  expect_silent(plotPixels(cur_Images,
                           save_plot = list(filename = paste0(cur_path, "/test.tiff"),
                                             scale = 10)))
  expect_true(file.exists(paste0(cur_path, "/test.tiff")))

  # Test if displaying single images works
  expect_silent(plotPixels(cur_Images,
                           save_plot = list(filename = paste0(cur_path, "/test.png"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_legend.png")))
  expect_true(file.exists(paste0(cur_path, "/test_1.png")))
  expect_true(file.exists(paste0(cur_path, "/test_2.png")))
  expect_true(file.exists(paste0(cur_path, "/test_3.png")))

  expect_silent(plotPixels(cur_Images,
                           save_plot = list(filename = paste0(cur_path, "/test.jpeg"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_legend.jpeg")))
  expect_true(file.exists(paste0(cur_path, "/test_1.jpeg")))
  expect_true(file.exists(paste0(cur_path, "/test_2.jpeg")))
  expect_true(file.exists(paste0(cur_path, "/test_3.jpeg")))

  expect_silent(plotPixels(cur_Images,
                           save_plot = list(filename = paste0(cur_path, "/test.tiff"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_legend.png")))
  expect_true(file.exists(paste0(cur_path, "/test_1.png")))
  expect_true(file.exists(paste0(cur_path, "/test_2.png")))
  expect_true(file.exists(paste0(cur_path, "/test_3.png")))

  # Test if subsetting works
  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_all.png"),
                                            scale = 1),
                           display = "all"))
  expect_true(file.exists(paste0(cur_path, "/test_all.png")))

  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_all.jpeg"),
                                            scale = 1),
                           display = "all"))
  expect_true(file.exists(paste0(cur_path, "/test_all.jpeg")))

  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_all.tiff"),
                                            scale = 1),
                           display = "all"))
  expect_true(file.exists(paste0(cur_path, "/test_all.tiff")))

  # Test if subsetting and displaying single images works
  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_single.png"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_single_legend.png")))
  expect_true(file.exists(paste0(cur_path, "/test_single_1.png")))
  expect_true(file.exists(paste0(cur_path, "/test_single_2.png")))

  expect_silent(plotPixels(cur_Images[1],
                           save_plot = list(filename = paste0(cur_path, "/test_one.png"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_one_legend.png")))
  expect_true(file.exists(paste0(cur_path, "/test_one_1.png")))

  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_single.jpeg"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_single_legend.jpeg")))
  expect_true(file.exists(paste0(cur_path, "/test_single_1.jpeg")))
  expect_true(file.exists(paste0(cur_path, "/test_single_2.jpeg")))

  expect_silent(plotPixels(cur_Images[1],
                           save_plot = list(filename = paste0(cur_path, "/test_one.jpeg"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_one_legend.jpeg")))
  expect_true(file.exists(paste0(cur_path, "/test_one_1.jpeg")))

  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_single.tiff"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_single_legend.tiff")))
  expect_true(file.exists(paste0(cur_path, "/test_single_1.tiff")))
  expect_true(file.exists(paste0(cur_path, "/test_single_2.tiff")))

  expect_silent(plotPixels(cur_Images[1],
                           save_plot = list(filename = paste0(cur_path, "/test_one.tiff"),
                                            scale = 1),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_one_legend.tiff")))
  expect_true(file.exists(paste0(cur_path, "/test_one_1.tiff")))

  # Remove legend
  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_nl.png"),
                                            scale = 10),
                           legend = NULL,
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_nl_1.png")))
  expect_true(file.exists(paste0(cur_path, "/test_nl_2.png")))

  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_nl.jpeg"),
                                            scale = 10),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_nl_1.jpeg")))
  expect_true(file.exists(paste0(cur_path, "/test_nl_2.jpeg")))

  expect_silent(plotPixels(cur_Images[c(1,3)],
                           save_plot = list(filename = paste0(cur_path, "/test_nl.tiff"),
                                            scale = 10),
                           display = "single"))
  expect_true(file.exists(paste0(cur_path, "/test_nl_1.tiff")))
  expect_true(file.exists(paste0(cur_path, "/test_nl_2.tiff")))

  # Error
  expect_error(plotPixels(cur_Images,
                          save_plot = list(filename = "test")),
               regexp = "Invalid entry to the 'save_plot' list object")
  expect_error(plotPixels(cur_Images,
                          save_plot = list(filename = 1)),
               regexp = "Invalid entry to the 'save_plot' list object")
  expect_error(plotPixels(cur_Images,
                          save_plot = list(filename = "test.pdf")),
               regexp = "Invalid entry to the 'save_plot' list object")
  expect_error(plotPixels(cur_Images,
                          save_plot = list(scale = 1)),
               regexp = "Invalid entry to the 'save_plot' list object")
  expect_error(plotPixels(cur_Images,
                          save_plot = list(filename = "test.png",
                                            scale = "test")),
               regexp = "Invalid entry to the 'save_plot' list object")
})

test_that("On disk: plotting-param: return_plot can be set.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(cur_out <- plotPixels(cur_Images, return_plot = FALSE))
  expect_null(cur_out)
  expect_silent(cur_out <- plotPixels(cur_Images, return_plot = TRUE))
  dev.off()
  expect_silent(cur_out$plot)
  expect_silent(cur_out <- plotCells(cur_Masks, return_plot = TRUE))
  dev.off()
  expect_silent(cur_out$plot)

  # Error
  expect_error(plotPixels(cur_Images, return_plot = "test"),
               regexp = "Invalid 'return_plot' entry.")

})

test_that("On disk: plotting-param: return_images can be set.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(cur_out <- plotPixels(cur_Images, return_images = FALSE))
  expect_null(cur_out)
  expect_silent(cur_out <- plotPixels(cur_Images, return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(cur_Images))
  expect_identical(names(cur_out$images), names(cur_Images))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotPixels(cur_Images, mask = cur_Masks,
                                      img_id = "ImageNb",
                                      return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(cur_Images))
  expect_identical(names(cur_out$images), names(cur_Images))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotPixels(cur_Images, mask = cur_Masks,
                                      object = pancreasSCE,
                                      img_id = "ImageNb",
                                      cell_id = "CellNb",
                                      outline_by = "CellType",
                                      return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(cur_Images))
  expect_identical(names(cur_out$images), names(cur_Images))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotPixels(cur_Images, mask = cur_Masks,
                                      object = pancreasSCE,
                                      img_id = "ImageNb",
                                      cell_id = "CellNb",
                                      colour_by = c("H3", "CD99"),
                                      outline_by = "CellType",
                                      return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(cur_Images))
  expect_identical(names(cur_out$images), names(cur_Images))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotCells(cur_Masks, return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(cur_Masks))
  expect_identical(names(cur_out$images), names(cur_Masks))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotCells(cur_Masks, object = pancreasSCE,
                                     img_id = "ImageNb", cell_id = "CellNb",
                                     colour_by = "CellType",
                                     return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(cur_Masks))
  expect_identical(names(cur_out$images), names(cur_Masks))
  expect_silent(plot(cur_out$images[[1]]))

  expect_silent(cur_out <- plotCells(cur_Masks, object = pancreasSCE,
                                     img_id = "ImageNb", cell_id = "CellNb",
                                     colour_by = "CellType", outline_by = "Area",
                                     return_images = TRUE))
  expect_identical(mcols(cur_out$images), mcols(cur_Masks))
  expect_identical(names(cur_out$images), names(cur_Masks))
  expect_silent(plot(cur_out$images[[1]]))

  # Error
  expect_error(plotPixels(cur_Images, return_images = "test"),
               regexp = "Invalid 'return_images' entry.")

})

test_that("On disk: plotting-param: legend can be set.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(plotPixels(cur_Images))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("H3", "CD99", "PIN",
                                         "CD8a", "CDH")))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("H3", "CD99", "PIN",
                                         "CD8a", "CDH"),
                           legend = NULL))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("H3", "CD99", "PIN",
                                         "CD8a", "CDH"),
                           legend = list(colour_by.title.font = 4)))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("H3", "CD99", "PIN",
                                         "CD8a", "CDH"),
                           legend = list(colour_by.title.cex = 0.5)))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("H3", "CD99", "PIN",
                                         "CD8a", "CDH"),
                           legend = list(colour_by.labels.cex = 0.5)))


  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "CD99", "PIN",
                                        "CD8a", "CDH"),
                          legend = NULL))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "CD99", "PIN",
                                        "CD8a", "CDH")))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          legend = list(colour_by.legend.cex = 0.5)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          legend = list(colour_by.title.cex = 3)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          legend = list(colour_by.title.font = 4)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          legend = list(colour_by.labels.cex = 1)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "Area",
                          legend = list(outline_by.title.font = 4)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "Area",
                          legend = list(outline_by.title.cex = 4)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "Area",
                          legend = list(outline_by.labels.cex = 4)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "CellType",
                          legend = list(outline_by.legend.cex = 2)))

  # Margin
  expect_silent(plotPixels(cur_Images))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("H3", "CD99", "PIN",
                                         "CD8a", "CDH"),
                           legend = list(margin = 10)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "CellType",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          outline_by = "Area",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "Area",
                          outline_by = "CellType",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "CellType",
                          outline_by = "Area",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "CD99", "PIN",
                                        "CD8a", "CDH"),
                          outline_by = "Area",
                          legend = list(margin = 10)))
  expect_silent(plotCells(mask = cur_Masks,
                          object = pancreasSCE,
                          img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = c("H3", "CD99", "PIN",
                                        "CD8a", "CDH"),
                          outline_by = "CellType",
                          legend = list(margin = 10)))


  # Error
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(test = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(colour_by.title.font = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(colour_by.labels.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(colour_by.legend.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(outline_by.title.font = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(outline_by.title.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(outline_by.labels.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(outline_by.legend.cex = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images,
                          colour_by = "H3",
                          legend = list(margin = "test")),
               regexp = "Invalid entry to the 'legend' list object",
               fixed = TRUE)
})

test_that("On disk: plotting-param: margin can be set.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(plotPixels(cur_Images))
  expect_silent(plotPixels(cur_Images, margin = 2))
  expect_silent(plotPixels(cur_Images, margin = 100))

  expect_silent(plotCells(cur_Masks, margin = 2))
  expect_silent(plotCells(cur_Masks, margin = 100))

  cur_images <- cur_Images
  names(cur_images) <- paste(names(cur_images), 2, sep = "_")
  cur_images <- c(cur_Images, cur_images)
  expect_silent(plotPixels(cur_images))
  expect_silent(plotPixels(cur_images, margin = 2))
  expect_silent(plotPixels(cur_images, margin = 100))

  # Error
  expect_error(plotPixels(cur_Images, margin = "test"),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images, margin = c(1,2)),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)
  expect_error(plotPixels(cur_Images, margin = -1),
               regexp = "Invalid 'margin' entry.",
               fixed = TRUE)

})

test_that("On disk: plotting-param: images can be plotted individually.", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99")))
  expect_silent(plotPixels(cur_Images, display = "all"))
  expect_silent(plotPixels(cur_Images, display = "single"))
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           display = "single"))
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           scale_bar = list(frame = 3),
                           display = "single"))
  # scale_bar
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           scale_bar = list(position = "topright",
                                            frame = 3,
                                            margin = c(20,20)),
                           display = "single"))

  # image_title
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           image_title = list(text = c(1,2,3),
                                              cex = 3),
                           display = "single"))

  # image_title
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           image_title = list(text = c(1,2,3),
                                              cex = 3),
                           display = "single"))

  # subsetting
  expect_silent(plotPixels(cur_Images["E34_imc"], colour_by = c("H3", "CD99", "CDH"),
                           scale_bar = list(frame = 3),
                           display = "single"))

  expect_silent(plotCells(cur_Masks["E34_mask"],
                           display = "single"))
  
  skip_on_ci()

  # save_plot
  cur_path <- tempdir()
  on.exit(unlink(cur_path))

  dev.off()

  cur_par1 <- par()
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           save_plot = list(filename = paste0(cur_path, "/test.png"),
                                             scale = 2),
                           display = "all"))
  cur_par2 <- par()
  expect_equal(cur_par1, cur_par2)

  dev.off()

  cur_par1 <- par()
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           save_plot = list(filename = paste0(cur_path, "/test.png"),
                                             scale = 2),
                           display = "single"))
  cur_par2 <- par()
  expect_equal(cur_par1, cur_par2)

  # Subsetting the images
  expect_silent(plotPixels(cur_Images["E34_imc"],
                          display = "single",
                          save_plot = list(filename = paste0(cur_path, "/test.png"),
                                           scale = 2)))
  # return_plot
  expect_silent(cur_out <- plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           return_plot = TRUE,
                           display = "single"))
  expect_equal(length(cur_out$plot), 4L)
  expect_equal(names(cur_out$plot), c("legend", names(cur_Images)))
  expect_silent(cur_out <- plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                                      return_plot = TRUE,
                                      display = "single",
                                      legend = NULL))
  expect_equal(length(cur_out$plot), 3L)
  expect_equal(names(cur_out$plot), names(cur_Images))

  expect_silent(cur_out <- plotCells(cur_Masks, object = pancreasSCE,
                                     img_id = "ImageNb", cell_id = "CellNb",
                                     colour_by = c("H3", "CD99", "CDH"),
                                     return_plot = TRUE,
                                     display = "single"))
  expect_equal(length(cur_out$plot), 4L)
  expect_equal(names(cur_out$plot), c("legend", mcols(cur_Masks)$ImageNb))
  expect_silent(cur_out <- plotCells(cur_Masks, object = pancreasSCE,
                                     img_id = "ImageNb", cell_id = "CellNb",
                                     colour_by = c("H3", "CD99", "CDH"),
                                     return_plot = TRUE,
                                     display = "single",
                                     legend = NULL))
  expect_equal(length(cur_out$plot), 3L)
  expect_equal(names(cur_out$plot), as.character(mcols(cur_Masks)$ImageNb))

  # return_images
  expect_silent(cur_out <- plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                                      return_images = TRUE,
                                      display = "single"))
  expect_equal(length(cur_out$images), 3L)

  # margin
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           margin = 10, display = "single"))

  # legend
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           legend = NULL, display = "single"))
  expect_silent(plotPixels(cur_Images, colour_by = c("H3", "CD99", "CDH"),
                           legend = list(colour_by.labels.cex = 1,
                                         colour_by.title.cex = 2),
                           display = "single"))

  # Error
  expect_error(plotPixels(cur_Images,
                          colour_by = c("H3", "CD99", "CDH"),
                          display = "test"),
               regexp = "Invalid 'display' entry.",
               fixed = TRUE)
})


test_that("On disk: plotting-param: scale can be correctly set", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(plotPixels(cur_Images, colour_by = c("CD99", "CDH", "PIN")))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("CD99", "CDH", "PIN"),
                           scale = FALSE))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("CD99", "CDH", "PIN"),
                           scale = FALSE,
                           bcg = list(CD99 = c(0,2,1),
                                      CDH = c(0,1,1),
                                      PIN = c(0,10,1))))
  expect_silent(plotPixels(cur_Images,
                           colour_by = c("CD99", "CDH", "PIN"),
                           scale = FALSE,
                           display = "single",
                           bcg = list(CD99 = c(0,2,1),
                                      CDH = c(0,1,1),
                                      PIN = c(0,10,1))))

  expect_silent(plotCells(cur_Masks, object = pancreasSCE,
                          img_id = "ImageNb", cell_id = "CellNb",
                          colour_by = c("CD99", "CDH", "PIN")))
  expect_silent(plotCells(cur_Masks, object = pancreasSCE,
                          img_id = "ImageNb", cell_id = "CellNb",
                          colour_by = c("CD99", "CDH", "PIN"),
                          scale = FALSE))
  expect_silent(plotCells(cur_Masks, object = pancreasSCE,
                          img_id = "ImageNb", cell_id = "CellNb",
                          colour_by = c("H3", "CDH", "PIN"),
                          scale = FALSE))
  expect_silent(plotCells(cur_Masks, object = pancreasSCE,
                          img_id = "ImageNb", cell_id = "CellNb",
                          colour_by = c("CD99", "CDH", "PIN"),
                          scale = FALSE,
                          exprs_values = "exprs"))
  expect_silent(plotCells(cur_Masks, object = pancreasSCE,
                          img_id = "ImageNb", cell_id = "CellNb",
                          colour_by = c("CD99", "CDH", "PIN"),
                          scale = TRUE,
                          exprs_values = "exprs"))

  # Error
  expect_error(plotPixels(cur_Images,
                          colour_by = c("CD99", "CDH", "PIN"),
                          scale = "test"),
               regexp = "Invalid 'scale' entry.",
               fixed = TRUE)
})

test_that("On disk: plotting-param: images can be interpolated", {
  data("pancreasImages")
  data("pancreasMasks")
  data("pancreasSCE")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

  # Works
  expect_silent(plotCells(cur_Masks, interpolate = TRUE))
  expect_silent(plotCells(cur_Masks, interpolate = FALSE))

  expect_silent(plotPixels(cur_Images, interpolate = TRUE))
  expect_silent(plotPixels(cur_Images, interpolate = FALSE))

  expect_silent(plotPixels(cur_Images, colour_by = c("CD99", "CDH"),
                           interpolate = TRUE))
  expect_silent(plotPixels(cur_Images, colour_by = c("CD99", "CDH"),
                           interpolate = FALSE))

  # Error
  expect_error(plotPixels(cur_Images,
                          colour_by = c("CD99", "CDH", "PIN"),
                          interpolate = "test"),
               regexp = "Invalid 'interpolate' entry.",
               fixed = TRUE)
})

test_that("On disk: plotting-param: border thickness can be adjusted", {
    data("pancreasImages")
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    
    # Works
    expect_silent(plotCells(object = pancreasSCE,
                            mask = cur_Masks, img_id = "ImageNb",
                            cell_id = "CellNb",
                            outline_by = "CellType",
                            thick = TRUE))
    expect_silent(plotCells(object = pancreasSCE,
                            mask = cur_Masks, img_id = "ImageNb",
                            cell_id = "CellNb", colour_by = "CD99",
                            outline_by = "CellType",
                            thick = TRUE))
    expect_silent(plotCells(object = pancreasSCE,
                            mask = cur_Masks, img_id = "ImageNb",
                            cell_id = "CellNb", colour_by = "CD99",
                            outline_by = "Area", 
                            thick = TRUE))
    expect_silent(plotPixels(object = pancreasSCE,
                             mask = cur_Masks,
                             image = cur_Images,
                             img_id = "ImageNb",
                             cell_id = "CellNb",
                             outline_by = "CellType",
                             thick = TRUE))
    expect_silent(plotPixels(object = pancreasSCE,
                             mask = cur_Masks,
                             image = cur_Images,
                             img_id = "ImageNb",
                             cell_id = "CellNb", colour_by = "CD99",
                             outline_by = "CellType",
                             thick = TRUE))
    expect_silent(plotPixels(object = pancreasSCE,
                             mask = cur_Masks,
                             image = cur_Images,
                             img_id = "ImageNb",
                             cell_id = "CellNb", colour_by = "CD99",
                             outline_by = "Area",
                             thick = TRUE))
    expect_silent(plotPixels(object = pancreasSCE,
                             mask = cur_Masks,
                             image = cur_Images,
                             img_id = "ImageNb",
                             cell_id = "CellNb", colour_by = "CD99",
                             thick = TRUE))
    
    # Error
    expect_error(plotCells(object = pancreasSCE,
                           mask = cur_Masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           outline_by = "CellType",
                           thick = "test"),
                 regexp = "Invalid 'thick' entry.",
                 fixed = TRUE)
})

