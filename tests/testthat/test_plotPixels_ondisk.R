test_that("On disk: plotPixels: Standard input testing works", {
  data("pancreasSCE")
  data("pancreasImages")
  data("pancreasMasks")
  
  cur_path <- tempdir()
  on.exit(unlink(cur_path))
  
  cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
  cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
 
  # Works
  # Add test if only image can be displayed
  expect_silent(plotPixels(image = cur_Images))
  expect_silent(plotPixels(image = cur_Images,
                          mask = pancreasMasks,
                          img_id = "ImageNb"))
  expect_silent(plotPixels(image = pancreasImages,
                           mask = cur_Masks,
                           img_id = "ImageNb"))
  expect_silent(plotPixels(image = cur_Images,
                           mask = cur_Masks,
                           img_id = "ImageNb"))
})

test_that("On disk: plotPixels: Features can be displayed.", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    

  # Works
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "H3"))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "CD99"))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "PIN"))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "CD8a"))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "CDH"))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = c("H3", "CD99")))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = c("H3", "CD99", "PIN")))
  expect_silent(test1 <- plotPixels(image = cur_Images,
            colour_by = c("H3", "CD99", "PIN", "CD8a", "CDH"),
            return_plot = TRUE, display = "single"))
  expect_silent(test2 <- plotPixels(image = rev(cur_Images),
                           colour_by = c("H3", "CD99", "PIN", "CD8a", "CDH"),
                           return_plot = TRUE, display = "single"))
  expect_identical(test1$plot$E34_imc, test2$plot$E34_imc)
  expect_identical(test1$plot$G01_imc, test2$plot$G01_imc)
  expect_identical(test1$plot$J02_imc, test2$plot$J02_imc)
})

test_that("On disk: plotPixels: Channels can be enhanced.", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
  # Works
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "H3", bcg = list(H3 = c(0,1,1))))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "H3", bcg = list(H3 = c(0,2,1))))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "H3", bcg = list(H3 = c(0,1,1.1))))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = "H3", bcg = list(H3 = c(100,1,1))))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = c("H3", "CD99"),
                           bcg = list(H3 = c(0,1,1),
                                      CD99 = c(0,1,1))))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = c("H3", "CD99"),
                           bcg = list(H3 = c(0,1,1),
                                      CD99 = c(0,2,1))))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = c("H3", "CD99"),
                           bcg = list(H3 = c(0,1,1),
                                      CD99 = c(0,1,2))))
  expect_silent(plotPixels(image = cur_Images,
                           colour_by = c("H3", "CD99"),
                           bcg = list(H3 = c(0,1,1),
                                      CD99 = c(10,2,1))))
})

test_that("On disk: plotPixels: Cells can be outlined correctly.", {
    data("pancreasSCE")
    data("pancreasImages")
    data("pancreasMasks")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
  # Works
  expect_silent(plotPixels(object = pancreasSCE,
            mask = cur_Masks,
            image = cur_Images,
            img_id = "ImageNb",
            cell_id = "CellNb",
            outline_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
                           mask = cur_Masks,
                           image = cur_Images,
                           img_id = "ImageNb",
                          cell_id = "CellNb", colour_by = "CD99",
                          outline_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
                           mask = cur_Masks,
                           image = cur_Images,
                           img_id = "ImageNb",
            cell_id = "CellNb", colour_by = "CD99",
            outline_by = "Area"))
})

test_that("On disk: plotPixels: images can be correctly subsetted.", {
    data("pancreasSCE")
    data("pancreasImages")
    data("pancreasMasks")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
  # Works
  expect_silent(plotPixels(image = cur_Images[1],
            colour_by = "CD99"))
  expect_silent(plotPixels(image = cur_Images[1:3],
                           colour_by = "CD99"))
  expect_silent(plotPixels(image = cur_Images[1:2],
                           colour_by = "CD99"))
  expect_silent(plotPixels(image = cur_Images[c(1,3)],
                           colour_by = "CD99"))
  expect_silent(plotPixels(image = cur_Images["J02_imc"],
                           colour_by = "CD99"))
  expect_silent(plotPixels(image = cur_Images[c("E34_imc", "J02_imc")],
                           colour_by = "CD99"))

  # Setting the image title
  expect_silent(plotPixels(image = cur_Images[c("E34_imc", "J02_imc")],
                           colour_by = "CD99",
                           image_title = list(text = c("test1", "test2"))))

  # Use mcols entry
  mcols(cur_Images)$ImageName <- paste0(names(cur_Images), ".tiff")
  cur_images <- getImages(cur_Images, mcols(cur_Images)$ImageName %in% c("E34_imc.tiff", "J02_imc.tiff"))
  expect_silent(plotPixels(image = cur_images,
                           img_id = "ImageName",
                           colour_by = "CD99"))
  expect_silent(plotPixels(image = cur_Images[1:2],
                           img_id = "ImageName",
                          cell_id = "CellNb",
                          colour_by = "CD99"))

  # Image and Mask
  expect_silent(plotPixels(image = cur_Images[1:2],
                           mask = cur_Masks[1:2],
                           object = pancreasSCE,
                           img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "CD99",
                           outline_by = "CellType"))
})

test_that("On disk: plotPixels: colour can be correctly adjusted.", {
    data("pancreasSCE")
    data("pancreasImages")
    data("pancreasMasks")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
  # Works
  expect_silent(plotPixels(image = cur_Images,
                          colour_by = "CD99",
                          colour = list(CD99 = colorRampPalette(c("black", "red"))(100))))
  expect_silent(plotPixels(image = cur_Images,
                          colour_by = "CD99",
                          colour = list(CD99 = c("black", "red"))))
  expect_silent(plotPixels(image = cur_Images,
                          colour_by = c("H3", "CDH"),
                          colour = list(H3 = colorRampPalette(c("black", "red"))(100),
                                        CDH = colorRampPalette(c("black", "green"))(100))))
  expect_silent(plotPixels(object = pancreasSCE,
                           image = cur_Images,
                          mask = cur_Masks, img_id = "ImageNb",
                          cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"))))
  expect_silent(plotPixels(object = pancreasSCE,
                           image = cur_Images,
                           mask = cur_Masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(H3 = c("black", "green"))))
  expect_silent(plotPixels(object = pancreasSCE,
                           image = cur_Images,
                           mask = cur_Masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                          colour_by = "H3", outline_by = "CellType",
                          colour = list(CellType = c(celltype_B = "green",
                                                     celltype_A = "blue",
                                                     celltype_C = "red"),
                                        H3 = c("black", "green"))))
})

test_that("On disk: plotPixels: SCE can be subsetted.", {
    data("pancreasSCE")
    data("pancreasImages")
    data("pancreasMasks")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
  # Subset cells
  set.seed(12345)
  cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]

  expect_silent(plotPixels(object = cur_sce,
                           image = cur_Images,
                           mask = cur_Masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"))
  expect_silent(plotPixels(object = cur_sce,
                           image = cur_Images,
                           mask = cur_Masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "Pattern"))
  expect_silent(plotPixels(object = cur_sce,
                           image = cur_Images,
                           mask = cur_Masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType",
                colour = list(CellType = c(celltype_B = "green",
                                           celltype_A = "blue",
                                           celltype_C = "red"))))

  cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
  cur_sce <- cur_sce[,1:10]
  expect_silent(plotPixels(object = cur_sce,
                           image = cur_Images,
                           mask = cur_Masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"))
  cur_images <- getImages(cur_Images, mcols(cur_Images)$ImageNb == unique(cur_sce$ImageNb))
  cur_masks <- getImages(cur_Masks, mcols(cur_Masks)$ImageNb == unique(cur_sce$ImageNb))
  expect_silent(plotPixels(object = cur_sce,
                           image = cur_images,
                           mask = cur_masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"))
})

test_that("On disk: plotPixels: Size of images can be changed.", {
    data("pancreasSCE")
    data("pancreasImages")
    data("pancreasMasks")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
  # Change size of images
  # Decreasing the size
  cur_masks <- cur_Masks
  setImages(cur_masks, "E34_mask") <- cur_masks[[1]][1:50, 1:10,drop=FALSE]
  expect_error(plotPixels(object = pancreasSCE,
                           image = cur_Images,
                           mask = cur_masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"),
               regexp = "Mask and image entries must have the same dimensions.",
               fixed = TRUE)


  cur_images <- cur_Images
  setImages(cur_images, "E34_imc") <- cur_images[[1]][1:50, 1:10,,drop=FALSE]

  expect_silent(plotPixels(object = pancreasSCE,
                           image = cur_images,
                           mask = cur_masks, img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3", outline_by = "CellType"))
  expect_silent(plotPixels(object = pancreasSCE,
                           image = cur_images[1],
                           mask = cur_masks[1], img_id = "ImageNb",
                           cell_id = "CellNb",
                           colour_by = "H3"))
  expect_silent(plotPixels(image = cur_images[1],
                           colour_by = "H3",
                           legend = list(colour_by.labels.cex = 0.6,
                                         colour_by.title.cex = 0.5)))
})



