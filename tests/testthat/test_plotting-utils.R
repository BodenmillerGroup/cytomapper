test_that("masks can be coloured by metadata.", {
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_col <- c(celltype_A = "red", celltype_B = "blue", celltype_C = "green")
    
    expect_silent(cur_out <- .colourMaskByMeta(object = pancreasSCE, mask = pancreasMasks, 
                      cell_id = "CellNb", img_id = "ImageNb", 
                      colour_by = "CellType", cur_colour = cur_col, 
                      missing_colour = "grey", background_colour = "black"))
    
    # Test if cells are correctly coloured
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "blue"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "green"))
    
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "blue"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "green"))
    
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "blue"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "green"))

    cur_col <- c("black", "green")
    
    expect_silent(cur_out <- .colourMaskByMeta(object = pancreasSCE, mask = pancreasMasks, 
                                               cell_id = "CellNb", img_id = "ImageNb", 
                                               colour_by = "Area", cur_colour = cur_col, 
                                               missing_colour = "grey", background_colour = "black"))
    
    cur_col_ramp <- colorRampPalette(cur_col)(101)
    
    # First image
    cur_scaling <- .minMaxScaling(colData(pancreasSCE[,pancreasSCE$ImageNb == 1])[,"Area"],
                                  min_x = min(colData(pancreasSCE)[,"Area"]),
                                  max_x = max(colData(pancreasSCE)[,"Area"]))
    expect_equal(max(cur_scaling), 
                 (max(colData(pancreasSCE[,pancreasSCE$ImageNb == 1])[,"Area"]) - min(colData(pancreasSCE)[,"Area"]))/(max(colData(pancreasSCE)[,"Area"]) - min(colData(pancreasSCE)[,"Area"])))
    
    col_ind <- cur_col_ramp[round(100*cur_scaling) + 1]

    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1]
    cur_col_ind <- col_ind[match(pancreasMasks[[1]][pancreasMasks[[1]] %in% cur_ids], 
                                 cur_ids)]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == cur_col_ind))
    
    # Second image
    cur_scaling <- .minMaxScaling(colData(pancreasSCE[,pancreasSCE$ImageNb == 2])[,"Area"],
                                  min_x = min(colData(pancreasSCE)[,"Area"]),
                                  max_x = max(colData(pancreasSCE)[,"Area"]))
    expect_equal(max(cur_scaling), 
                 (max(colData(pancreasSCE[,pancreasSCE$ImageNb == 2])[,"Area"]) - min(colData(pancreasSCE)[,"Area"]))/(max(colData(pancreasSCE)[,"Area"]) - min(colData(pancreasSCE)[,"Area"])))
    
    col_ind <- cur_col_ramp[round(100*cur_scaling) + 1]
    
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2]
    cur_col_ind <- col_ind[match(pancreasMasks[[2]][pancreasMasks[[2]] %in% cur_ids], 
                                 cur_ids)]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == cur_col_ind))
    
    # Third image
    cur_scaling <- .minMaxScaling(colData(pancreasSCE[,pancreasSCE$ImageNb == 3])[,"Area"],
                                  min_x = min(colData(pancreasSCE)[,"Area"]),
                                  max_x = max(colData(pancreasSCE)[,"Area"]))
    expect_equal(max(cur_scaling), 
                 (max(colData(pancreasSCE[,pancreasSCE$ImageNb == 3])[,"Area"]) - min(colData(pancreasSCE)[,"Area"]))/(max(colData(pancreasSCE)[,"Area"]) - min(colData(pancreasSCE)[,"Area"])))
    
    col_ind <- cur_col_ramp[round(100*cur_scaling) + 1]
    
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3]
    cur_col_ind <- col_ind[match(pancreasMasks[[3]][pancreasMasks[[3]] %in% cur_ids], 
                                 cur_ids)]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == cur_col_ind))

    # Check missing colour and background colour
    cur_col <- c(celltype_A = "red", celltype_B = "blue", celltype_C = "green")
    
    expect_silent(cur_out <- .colourMaskByMeta(object = pancreasSCE, mask = pancreasMasks, 
                                               cell_id = "CellNb", img_id = "ImageNb", 
                                               colour_by = "CellType", cur_colour = cur_col, 
                                               missing_colour = "grey", background_colour = "black"))
    
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] == 0L] == "black"))
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] == 0L] == "black"))
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] == 0L] == "black"))
    
    # Subset SCE
    set.seed(1234)
    cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]
    expect_silent(cur_out <- .colourMaskByMeta(object = cur_sce, mask = pancreasMasks, 
                                               cell_id = "CellNb", img_id = "ImageNb", 
                                               colour_by = "CellType", cur_colour = cur_col, 
                                               missing_colour = "grey", background_colour = "black"))
    
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] == 0L] == "black"))
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] == 0L] == "black"))
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] == 0L] == "black"))
    
    # Test if cells are correctly coloured
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1 & cur_sce$CellType == "celltype_A"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "red"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1 & cur_sce$CellType == "celltype_B"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1]  
    expect_true(all(cur_out[[1]][!(pancreasMasks[[1]] %in% cur_ids) & as.vector(pancreasMasks[[1]] != 0L)] == "grey"))
    
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_A"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "red"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_B"]
    expect_true(all(cur_out[[2]][cur_out[[2]] %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2]  
    expect_true(all(cur_out[[2]][!(pancreasMasks[[2]] %in% cur_ids) & as.vector(pancreasMasks[[2]] != 0L)] == "grey"))
    
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_A"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "red"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_B"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3]  
    expect_true(all(cur_out[[3]][!(pancreasMasks[[3]] %in% cur_ids) & as.vector(pancreasMasks[[3]] != 0L)] == "grey"))
})

test_that("masks can be coloured by features.", {
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_col <- list(H3 = c("black", "red"), SMA = c("black", "green"), CD44 = c("black", "blue"))
    plottingParam <- list(scale = TRUE)
    
    expect_silent(cur_out <- .colourMaskByFeature(object = pancreasSCE[,pancreasSCE$ImageNb == 1], mask = pancreasMasks[1], 
                                                  cell_id = "CellNb", img_id = "ImageNb",
                                                  colour_by = c("H3", "SMA", "CD44"), 
                                                  exprs_values = "counts", cur_colour = cur_col,
                                                  missing_colour = "grey", background_colour = "black", 
                                                  plottingParam = plottingParam))
    
    # Check if overlayed image is the same as EBImage output
    cur_img <- combine(pancreasMasks[[1]], pancreasMasks[[1]], pancreasMasks[[1]])
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
    cur_img_H3 <- pancreasMasks[[1]]
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img[,,1] <- cur_img_H3
    cur_img_SMA <- pancreasMasks[[1]]
    cur_img_SMA[cur_img_SMA == 0L] <- NA
    cur_ind <- match(cur_img_SMA, cur_sce$CellNb)
    cur_img_SMA[!is.na(cur_ind)] <- assay(cur_sce, "counts")["SMA",cur_ind[!is.na(cur_ind)]]
    cur_img[,,2] <- cur_img_SMA
    cur_img_CD44 <- pancreasMasks[[1]]
    cur_img_CD44[cur_img_CD44 == 0L] <- NA
    cur_ind <- match(cur_img_CD44, cur_sce$CellNb)
    cur_img_CD44[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD44",cur_ind[!is.na(cur_ind)]]
    cur_img[,,3] <- cur_img_CD44
    
    cur_img <- EBImage::normalize(cur_img, separate = TRUE)
    cur_img[is.na(cur_img)] <- 0
    
    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    # Now with global scaling
    expect_silent(cur_out <- .colourMaskByFeature(object = pancreasSCE, mask = pancreasMasks, 
                                                  cell_id = "CellNb", img_id = "ImageNb",
                                                  colour_by = c("H3", "SMA", "CD44"), 
                                                  exprs_values = "counts", cur_colour = cur_col,
                                                  missing_colour = "grey", background_colour = "black", 
                                                  plottingParam = plottingParam))
    
    # Check if overlayed image is the same as EBImage output
    cur_img <- combine(pancreasMasks[[1]], pancreasMasks[[1]], pancreasMasks[[1]])
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
    cur_img_H3 <- pancreasMasks[[1]]
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE, 
                                     inputRange = c(min(assay(pancreasSCE, "counts")["H3",]),
                                                    max(assay(pancreasSCE, "counts")["H3",])))
    cur_img[,,1] <- cur_img_H3
    cur_img_SMA <- pancreasMasks[[1]]
    cur_img_SMA[cur_img_SMA == 0L] <- NA
    cur_ind <- match(cur_img_SMA, cur_sce$CellNb)
    cur_img_SMA[!is.na(cur_ind)] <- assay(cur_sce, "counts")["SMA",cur_ind[!is.na(cur_ind)]]
    cur_img_SMA <- EBImage::normalize(cur_img_SMA, separate = TRUE, 
                                     inputRange = c(min(assay(pancreasSCE, "counts")["SMA",]),
                                                    max(assay(pancreasSCE, "counts")["SMA",])))
    cur_img[,,2] <- cur_img_SMA
    cur_img_CD44 <- pancreasMasks[[1]]
    cur_img_CD44[cur_img_CD44 == 0L] <- NA
    cur_ind <- match(cur_img_CD44, cur_sce$CellNb)
    cur_img_CD44[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD44",cur_ind[!is.na(cur_ind)]]
    cur_img_CD44 <- EBImage::normalize(cur_img_CD44, separate = TRUE, 
                                      inputRange = c(min(assay(pancreasSCE, "counts")["CD44",]),
                                                     max(assay(pancreasSCE, "counts")["CD44",])))
    cur_img[,,3] <- cur_img_CD44
    
    cur_img[is.na(cur_img)] <- 0
    
    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- combine(pancreasMasks[[2]], pancreasMasks[[2]], pancreasMasks[[2]])
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 2]
    cur_img_H3 <- pancreasMasks[[2]]
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE, 
                                     inputRange = c(min(assay(pancreasSCE, "counts")["H3",]),
                                                    max(assay(pancreasSCE, "counts")["H3",])))
    cur_img[,,1] <- cur_img_H3
    cur_img_SMA <- pancreasMasks[[2]]
    cur_img_SMA[cur_img_SMA == 0L] <- NA
    cur_ind <- match(cur_img_SMA, cur_sce$CellNb)
    cur_img_SMA[!is.na(cur_ind)] <- assay(cur_sce, "counts")["SMA",cur_ind[!is.na(cur_ind)]]
    cur_img_SMA <- EBImage::normalize(cur_img_SMA, separate = TRUE, 
                                      inputRange = c(min(assay(pancreasSCE, "counts")["SMA",]),
                                                     max(assay(pancreasSCE, "counts")["SMA",])))
    cur_img[,,2] <- cur_img_SMA
    cur_img_CD44 <- pancreasMasks[[2]]
    cur_img_CD44[cur_img_CD44 == 0L] <- NA
    cur_ind <- match(cur_img_CD44, cur_sce$CellNb)
    cur_img_CD44[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD44",cur_ind[!is.na(cur_ind)]]
    cur_img_CD44 <- EBImage::normalize(cur_img_CD44, separate = TRUE, 
                                       inputRange = c(min(assay(pancreasSCE, "counts")["CD44",]),
                                                      max(assay(pancreasSCE, "counts")["CD44",])))
    cur_img[,,3] <- cur_img_CD44
    
    cur_img[is.na(cur_img)] <- 0
    
    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- combine(pancreasMasks[[3]], pancreasMasks[[3]], pancreasMasks[[2]])
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 3]
    cur_img_H3 <- pancreasMasks[[3]]
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE, 
                                     inputRange = c(min(assay(pancreasSCE, "counts")["H3",]),
                                                    max(assay(pancreasSCE, "counts")["H3",])))
    cur_img[,,1] <- cur_img_H3
    cur_img_SMA <- pancreasMasks[[3]]
    cur_img_SMA[cur_img_SMA == 0L] <- NA
    cur_ind <- match(cur_img_SMA, cur_sce$CellNb)
    cur_img_SMA[!is.na(cur_ind)] <- assay(cur_sce, "counts")["SMA",cur_ind[!is.na(cur_ind)]]
    cur_img_SMA <- EBImage::normalize(cur_img_SMA, separate = TRUE, 
                                      inputRange = c(min(assay(pancreasSCE, "counts")["SMA",]),
                                                     max(assay(pancreasSCE, "counts")["SMA",])))
    cur_img[,,2] <- cur_img_SMA
    cur_img_CD44 <- pancreasMasks[[3]]
    cur_img_CD44[cur_img_CD44 == 0L] <- NA
    cur_ind <- match(cur_img_CD44, cur_sce$CellNb)
    cur_img_CD44[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD44",cur_ind[!is.na(cur_ind)]]
    cur_img_CD44 <- EBImage::normalize(cur_img_CD44, separate = TRUE, 
                                       inputRange = c(min(assay(pancreasSCE, "counts")["CD44",]),
                                                      max(assay(pancreasSCE, "counts")["CD44",])))
    cur_img[,,3] <- cur_img_CD44
    
    cur_img[is.na(cur_img)] <- 0
    
    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    # Set scaling to FALSE
    plottingParam <- list(scale = FALSE)
    colour_by <- c("H3", "SMA", "CD44")
    expect_silent(cur_out <- .colourMaskByFeature(object = pancreasSCE, mask = pancreasMasks, 
                                                  cell_id = "CellNb", img_id = "ImageNb",
                                                  colour_by = colour_by, 
                                                  exprs_values = "counts", cur_colour = cur_col,
                                                  missing_colour = "grey", background_colour = "black", 
                                                  plottingParam = plottingParam))
    
    # Check if overlayed image is the same as EBImage output
    cur_img <- combine(pancreasMasks[[1]], pancreasMasks[[1]], pancreasMasks[[1]])
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
    cur_img_H3 <- pancreasMasks[[1]]
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE, 
                                     inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                    max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,1] <- cur_img_H3
    cur_img_SMA <- pancreasMasks[[1]]
    cur_img_SMA[cur_img_SMA == 0L] <- NA
    cur_ind <- match(cur_img_SMA, cur_sce$CellNb)
    cur_img_SMA[!is.na(cur_ind)] <- assay(cur_sce, "counts")["SMA",cur_ind[!is.na(cur_ind)]]
    cur_img_SMA <- EBImage::normalize(cur_img_SMA, separate = TRUE, 
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_SMA
    cur_img_CD44 <- pancreasMasks[[1]]
    cur_img_CD44[cur_img_CD44 == 0L] <- NA
    cur_ind <- match(cur_img_CD44, cur_sce$CellNb)
    cur_img_CD44[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD44",cur_ind[!is.na(cur_ind)]]
    cur_img_CD44 <- EBImage::normalize(cur_img_CD44, separate = TRUE, 
                                       inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                      max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,3] <- cur_img_CD44
    
    cur_img[is.na(cur_img)] <- 0
    
    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- combine(pancreasMasks[[2]], pancreasMasks[[2]], pancreasMasks[[2]])
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 2]
    cur_img_H3 <- pancreasMasks[[2]]
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE, 
                                     inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                    max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,1] <- cur_img_H3
    cur_img_SMA <- pancreasMasks[[2]]
    cur_img_SMA[cur_img_SMA == 0L] <- NA
    cur_ind <- match(cur_img_SMA, cur_sce$CellNb)
    cur_img_SMA[!is.na(cur_ind)] <- assay(cur_sce, "counts")["SMA",cur_ind[!is.na(cur_ind)]]
    cur_img_SMA <- EBImage::normalize(cur_img_SMA, separate = TRUE, 
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_SMA
    cur_img_CD44 <- pancreasMasks[[2]]
    cur_img_CD44[cur_img_CD44 == 0L] <- NA
    cur_ind <- match(cur_img_CD44, cur_sce$CellNb)
    cur_img_CD44[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD44",cur_ind[!is.na(cur_ind)]]
    cur_img_CD44 <- EBImage::normalize(cur_img_CD44, separate = TRUE, 
                                       inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                      max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,3] <- cur_img_CD44
    
    cur_img[is.na(cur_img)] <- 0
    
    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- combine(pancreasMasks[[3]], pancreasMasks[[3]], pancreasMasks[[2]])
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 3]
    cur_img_H3 <- pancreasMasks[[3]]
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE, 
                                     inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                    max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,1] <- cur_img_H3
    cur_img_SMA <- pancreasMasks[[3]]
    cur_img_SMA[cur_img_SMA == 0L] <- NA
    cur_ind <- match(cur_img_SMA, cur_sce$CellNb)
    cur_img_SMA[!is.na(cur_ind)] <- assay(cur_sce, "counts")["SMA",cur_ind[!is.na(cur_ind)]]
    cur_img_SMA <- EBImage::normalize(cur_img_SMA, separate = TRUE, 
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_SMA
    cur_img_CD44 <- pancreasMasks[[3]]
    cur_img_CD44[cur_img_CD44 == 0L] <- NA
    cur_ind <- match(cur_img_CD44, cur_sce$CellNb)
    cur_img_CD44[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD44",cur_ind[!is.na(cur_ind)]]
    cur_img_CD44 <- EBImage::normalize(cur_img_CD44, separate = TRUE, 
                                       inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                      max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,3] <- cur_img_CD44
    
    cur_img[is.na(cur_img)] <- 0
    
    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
})

test_that("images can be coloured by features.", {
    data("pancreasImages")
    
    cur_col <- list(H3 = c("black", "red"), SMA = c("black", "green"), CD44 = c("black", "blue"))
    colour_by <- c("H3", "SMA", "CD44")
    plottingParam <- list(scale = TRUE)
    
    expect_silent(cur_out <- .colourImageByFeature(image = pancreasImages[1], colour_by = colour_by, 
                                                   bcg = list(H3 = c(0,1,1), SMA = c(0,1,1), CD44 = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    
    # Compare to EBImage
    cur_img <- getChannels(pancreasImages[1], c("H3", "SMA", "CD44"))
    cur_img <- EBImage::normalize(cur_img[[1]])
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    expect_silent(cur_out <- .colourImageByFeature(image = pancreasImages, colour_by = colour_by, 
                                                   bcg = list(H3 = c(0,1,1), SMA = c(0,1,1), CD44 = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    
    # Compare to EBImage
    H3_dist <- unlist(lapply(getChannels(pancreasImages, "H3"), as.numeric))
    SMA_dist <- unlist(lapply(getChannels(pancreasImages, "SMA"), as.numeric))
    CD44_dist <- unlist(lapply(getChannels(pancreasImages, "CD44"), as.numeric))
    cur_img <- getChannels(pancreasImages[1], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(SMA_dist), max(SMA_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CD44_dist), max(CD44_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(pancreasImages[2], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(SMA_dist), max(SMA_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CD44_dist), max(CD44_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- getChannels(pancreasImages[3], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(SMA_dist), max(SMA_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CD44_dist), max(CD44_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    # Set scale to FALSE
    plottingParam <- list(scale = FALSE)
    
    expect_silent(cur_out <- .colourImageByFeature(image = pancreasImages, colour_by = colour_by, 
                                                   bcg = list(H3 = c(0,1,1), SMA = c(0,1,1), CD44 = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    
    all_dist <- unlist(lapply(getChannels(pancreasImages, colour_by), as.numeric))
    cur_img <- getChannels(pancreasImages[1], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- getChannels(pancreasImages[2], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- getChannels(pancreasImages[3], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    # Set bcg
    plottingParam <- list(scale = TRUE)
    
    expect_silent(cur_out <- .colourImageByFeature(image = pancreasImages, colour_by = colour_by, 
                                                   bcg = list(H3 = c(0,0.5,1), SMA = c(0,1,2), CD44 = c(10,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    
    # Compare to EBImage
    H3_dist <- unlist(lapply(getChannels(pancreasImages, "H3"), as.numeric))
    SMA_dist <- unlist(lapply(getChannels(pancreasImages, "SMA"), as.numeric))
    CD44_dist <- unlist(lapply(getChannels(pancreasImages, "CD44"), as.numeric))
    cur_img <- getChannels(pancreasImages[1], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(SMA_dist), max(SMA_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CD44_dist), max(CD44_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- getChannels(pancreasImages[2], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(SMA_dist), max(SMA_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CD44_dist), max(CD44_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
    
    cur_img <- getChannels(pancreasImages[3], c("H3", "SMA", "CD44"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(SMA_dist), max(SMA_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CD44_dist), max(CD44_dist)))
    
    dimnames(cur_img) <- list(NULL, NULL, NULL)
    
    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
})

test_that("images can be outlined by metadata.", {
  # .outlineMaskByMeta
})

test_that("colours can be selected.", {
  # .selectColours
})

test_that("min/max scaling works.", {
  # .minMaxScaling
})

test_that("colour mixing works.", {
  # .mixColours
})

test_that("images can be displayed.", {
  # .displayImages
})

test_that("legend can be plotted.", {
  # .plotLegend
})

test_that("scale bar can be plotted.", {
  # .plotScaleBar
})

test_that("title can be set", {
    # .plotScaleBar
})


