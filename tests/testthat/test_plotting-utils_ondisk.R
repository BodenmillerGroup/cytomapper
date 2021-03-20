test_that("On disk: masks can be coloured by metadata.", {
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

    cur_col <- c(celltype_A = "red", celltype_B = "green", celltype_C = "blue")

    expect_silent(cur_out <- .colourMaskByMeta(object = pancreasSCE, mask = cur_Masks,
                      cell_id = "CellNb", img_id = "ImageNb",
                      colour_by = "CellType", cur_colour = cur_col,
                      missing_colour = "grey", background_colour = "black"))
    
    expect_true(is.list(cur_out))
    expect_true(is.null(cur_out$cur_limit))
    cur_out <- cur_out$imgs
    
    # Test if cells are correctly coloured
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) %in% cur_ids] == "green"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) %in% cur_ids] == "blue"))

    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) %in% cur_ids] == "green"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) %in% cur_ids] == "blue"))

    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) %in% cur_ids] == "green"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) %in% cur_ids] == "blue"))

    cur_col <- c("black", "green")

    expect_silent(cur_out <- .colourMaskByMeta(object = pancreasSCE, mask = cur_Masks,
                                               cell_id = "CellNb", img_id = "ImageNb",
                                               colour_by = "Area", cur_colour = cur_col,
                                               missing_colour = "grey", background_colour = "black"))

    cur_col_ramp <- colorRampPalette(cur_col)(101)
    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit, list(Area = c(2, 200)))
    cur_out <- cur_out$imgs
    
    # First image
    cur_scaling <- .minMaxScaling(colData(pancreasSCE[,pancreasSCE$ImageNb == 1])[,"Area"],
                                  min_x = min(colData(pancreasSCE)[,"Area"]),
                                  max_x = max(colData(pancreasSCE)[,"Area"]))
    expect_equal(max(cur_scaling),
                 (max(colData(pancreasSCE[,pancreasSCE$ImageNb == 1])[,"Area"]) - min(colData(pancreasSCE)[,"Area"]))/(max(colData(pancreasSCE)[,"Area"]) - min(colData(pancreasSCE)[,"Area"])))

    col_ind <- cur_col_ramp[round(100*cur_scaling) + 1]

    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1]
    cur_col_ind <- col_ind[match(as.array(cur_Masks[[1]])[as.array(cur_Masks[[1]]) %in% cur_ids],
                                 cur_ids)]
    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) %in% cur_ids] == cur_col_ind))

    # Second image
    cur_scaling <- .minMaxScaling(colData(pancreasSCE[,pancreasSCE$ImageNb == 2])[,"Area"],
                                  min_x = min(colData(pancreasSCE)[,"Area"]),
                                  max_x = max(colData(pancreasSCE)[,"Area"]))
    expect_equal(max(cur_scaling),
                 (max(colData(pancreasSCE[,pancreasSCE$ImageNb == 2])[,"Area"]) - min(colData(pancreasSCE)[,"Area"]))/(max(colData(pancreasSCE)[,"Area"]) - min(colData(pancreasSCE)[,"Area"])))

    col_ind <- cur_col_ramp[round(100*cur_scaling) + 1]

    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2]
    cur_col_ind <- col_ind[match(as.array(cur_Masks[[2]])[as.array(cur_Masks[[2]]) %in% cur_ids],
                                 cur_ids)]
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) %in% cur_ids] == cur_col_ind))

    # Third image
    cur_scaling <- .minMaxScaling(colData(pancreasSCE[,pancreasSCE$ImageNb == 3])[,"Area"],
                                  min_x = min(colData(pancreasSCE)[,"Area"]),
                                  max_x = max(colData(pancreasSCE)[,"Area"]))
    expect_equal(max(cur_scaling),
                 (max(colData(pancreasSCE[,pancreasSCE$ImageNb == 3])[,"Area"]) - min(colData(pancreasSCE)[,"Area"]))/(max(colData(pancreasSCE)[,"Area"]) - min(colData(pancreasSCE)[,"Area"])))

    col_ind <- cur_col_ramp[round(100*cur_scaling) + 1]

    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3]
    cur_col_ind <- col_ind[match(as.array(cur_Masks[[3]])[as.array(cur_Masks[[3]]) %in% cur_ids],
                                 cur_ids)]
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) %in% cur_ids] == cur_col_ind))

    # Check missing colour and background colour
    cur_col <- c(celltype_A = "red", celltype_B = "green", celltype_C = "blue")

    expect_silent(cur_out <- .colourMaskByMeta(object = pancreasSCE, mask = cur_Masks,
                                               cell_id = "CellNb", img_id = "ImageNb",
                                               colour_by = "CellType", cur_colour = cur_col,
                                               missing_colour = "grey", background_colour = "black"))

    expect_true(is.list(cur_out))
    expect_true(is.null(cur_out$cur_limit))
    cur_out <- cur_out$imgs
    
    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) == 0L] == "black"))
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) == 0L] == "black"))
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) == 0L] == "black"))

    # Subset SCE
    set.seed(1234)
    cur_sce <- pancreasSCE[,sample(1:ncol(pancreasSCE), 100)]
    expect_silent(cur_out <- .colourMaskByMeta(object = cur_sce, mask = cur_Masks,
                                               cell_id = "CellNb", img_id = "ImageNb",
                                               colour_by = "CellType", cur_colour = cur_col,
                                               missing_colour = "grey", background_colour = "black"))
    
    expect_true(is.list(cur_out))
    expect_true(is.null(cur_out$cur_limit))
    cur_out <- cur_out$imgs

    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) == 0L] == "black"))
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) == 0L] == "black"))
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) == 0L] == "black"))

    # Test if cells are correctly coloured
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1 & cur_sce$CellType == "celltype_A"]
    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) %in% cur_ids] == "red"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1 & cur_sce$CellType == "celltype_B"]
    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[1]][as.array(cur_Masks[[1]]) %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1]
    expect_true(all(cur_out[[1]][!(as.array(cur_Masks[[1]]) %in% cur_ids) & as.vector(cur_Masks[[1]] != 0L)] == "grey"))

    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_A"]
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) %in% cur_ids] == "red"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_B"]
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[2]][as.array(cur_Masks[[2]]) %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2]
    expect_true(all(cur_out[[2]][!(as.array(cur_Masks[[2]]) %in% cur_ids) & as.vector(cur_Masks[[2]] != 0L)] == "grey"))

    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_A"]
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) %in% cur_ids] == "red"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_B"]
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[3]][as.array(cur_Masks[[3]]) %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3]
    expect_true(all(cur_out[[3]][!(as.array(cur_Masks[[3]]) %in% cur_ids) & as.vector(cur_Masks[[3]] != 0L)] == "grey"))
})

test_that("On disk: masks can be coloured by features.", {
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

    cur_col <- list(H3 = c("black", "red"), CD99 = c("black", "green"), CDH = c("black", "blue"))
    plottingParam <- list(scale = TRUE)

    expect_silent(cur_out <- .colourMaskByFeature(object = pancreasSCE[,pancreasSCE$ImageNb == 1], mask = cur_Masks[1],
                                                  cell_id = "CellNb", img_id = "ImageNb",
                                                  colour_by = c("H3", "CD99", "CDH"),
                                                  exprs_values = "counts", cur_colour = cur_col,
                                                  missing_colour = "grey", background_colour = "black",
                                                  plottingParam = plottingParam))

    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit$H3, as.numeric(quantile(counts(pancreasSCE)["H3",pancreasSCE$ImageNb == 1], probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CD99, as.numeric(quantile(counts(pancreasSCE)["CD99",pancreasSCE$ImageNb == 1], probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CDH, as.numeric(quantile(counts(pancreasSCE)["CDH",pancreasSCE$ImageNb == 1], probs = c(0,1))))
    cur_out <- cur_out$imgs
    
    # Check if overlayed image is the same as EBImage output
    cur_img <- combine(as.array(cur_Masks[[1]]), as.array(cur_Masks[[1]]), as.array(cur_Masks[[1]]))
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
    cur_img_H3 <- as.array(cur_Masks[[1]])
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img[,,1] <- cur_img_H3
    cur_img_CD99 <- as.array(cur_Masks[[1]])
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- as.array(cur_Masks[[1]])
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img[,,3] <- cur_img_CDH

    cur_img <- EBImage::normalize(cur_img, separate = TRUE)
    cur_img[is.na(cur_img)] <- 0

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    # Now with global scaling
    expect_silent(cur_out <- .colourMaskByFeature(object = pancreasSCE, mask = cur_Masks,
                                                  cell_id = "CellNb", img_id = "ImageNb",
                                                  colour_by = c("H3", "CD99", "CDH"),
                                                  exprs_values = "counts", cur_colour = cur_col,
                                                  missing_colour = "grey", background_colour = "black",
                                                  plottingParam = plottingParam))
    
    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit$H3, as.numeric(quantile(counts(pancreasSCE)["H3",], probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CD99, as.numeric(quantile(counts(pancreasSCE)["CD99",], probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CDH, as.numeric(quantile(counts(pancreasSCE)["CDH",], probs = c(0,1))))
    cur_out <- cur_out$imgs

    # Check if overlayed image is the same as EBImage output
    cur_img <- combine(as.array(cur_Masks[[1]]), as.array(cur_Masks[[1]]), as.array(cur_Masks[[1]]))
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
    cur_img_H3 <- as.array(cur_Masks[[1]])
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE,
                                     inputRange = c(min(assay(pancreasSCE, "counts")["H3",]),
                                                    max(assay(pancreasSCE, "counts")["H3",])))
    cur_img[,,1] <- cur_img_H3
    cur_img_CD99 <- as.array(cur_Masks[[1]])
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                     inputRange = c(min(assay(pancreasSCE, "counts")["CD99",]),
                                                    max(assay(pancreasSCE, "counts")["CD99",])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- as.array(cur_Masks[[1]])
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")["CDH",]),
                                                     max(assay(pancreasSCE, "counts")["CDH",])))
    cur_img[,,3] <- cur_img_CDH

    cur_img[is.na(cur_img)] <- 0

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- combine(as.array(cur_Masks[[2]]), as.array(cur_Masks[[2]]), as.array(cur_Masks[[2]]))
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 2]
    cur_img_H3 <- as.array(cur_Masks[[2]])
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE,
                                     inputRange = c(min(assay(pancreasSCE, "counts")["H3",]),
                                                    max(assay(pancreasSCE, "counts")["H3",])))
    cur_img[,,1] <- cur_img_H3
    cur_img_CD99 <- as.array(cur_Masks[[2]])
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")["CD99",]),
                                                     max(assay(pancreasSCE, "counts")["CD99",])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- as.array(cur_Masks[[2]])
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                       inputRange = c(min(assay(pancreasSCE, "counts")["CDH",]),
                                                      max(assay(pancreasSCE, "counts")["CDH",])))
    cur_img[,,3] <- cur_img_CDH

    cur_img[is.na(cur_img)] <- 0

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- combine(as.array(cur_Masks[[3]]), as.array(cur_Masks[[3]]), as.array(cur_Masks[[2]]))
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 3]
    cur_img_H3 <- as.array(cur_Masks[[3]])
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE,
                                     inputRange = c(min(assay(pancreasSCE, "counts")["H3",]),
                                                    max(assay(pancreasSCE, "counts")["H3",])))
    cur_img[,,1] <- cur_img_H3
    cur_img_CD99 <- as.array(cur_Masks[[3]])
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")["CD99",]),
                                                     max(assay(pancreasSCE, "counts")["CD99",])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- as.array(cur_Masks[[3]])
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                       inputRange = c(min(assay(pancreasSCE, "counts")["CDH",]),
                                                      max(assay(pancreasSCE, "counts")["CDH",])))
    cur_img[,,3] <- cur_img_CDH

    cur_img[is.na(cur_img)] <- 0

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    # Set scaling to FALSE
    plottingParam <- list(scale = FALSE)
    colour_by <- c("H3", "CD99", "CDH")
    expect_silent(cur_out <- .colourMaskByFeature(object = pancreasSCE, mask = cur_Masks,
                                                  cell_id = "CellNb", img_id = "ImageNb",
                                                  colour_by = colour_by,
                                                  exprs_values = "counts", cur_colour = cur_col,
                                                  missing_colour = "grey", background_colour = "black",
                                                  plottingParam = plottingParam))

    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit$H3, as.numeric(quantile(counts(pancreasSCE)[colour_by,], probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CD99, as.numeric(quantile(counts(pancreasSCE)[colour_by,], probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CDH, as.numeric(quantile(counts(pancreasSCE)[colour_by,], probs = c(0,1))))
    cur_out <- cur_out$imgs
    
    # Check if overlayed image is the same as EBImage output
    cur_img <- combine(as.array(cur_Masks[[1]]), as.array(cur_Masks[[1]]), as.array(cur_Masks[[1]]))
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
    cur_img_H3 <- as.array(cur_Masks[[1]])
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE,
                                     inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                    max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,1] <- cur_img_H3
    cur_img_CD99 <- as.array(cur_Masks[[1]])
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- as.array(cur_Masks[[1]])
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                       inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                      max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,3] <- cur_img_CDH

    cur_img[is.na(cur_img)] <- 0

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- combine(as.array(cur_Masks[[2]]), as.array(cur_Masks[[2]]), as.array(cur_Masks[[2]]))
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 2]
    cur_img_H3 <- as.array(cur_Masks[[2]])
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE,
                                     inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                    max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,1] <- cur_img_H3
    cur_img_CD99 <- as.array(cur_Masks[[2]])
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- as.array(cur_Masks[[2]])
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                       inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                      max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,3] <- cur_img_CDH

    cur_img[is.na(cur_img)] <- 0

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- combine(as.array(cur_Masks[[3]]), as.array(cur_Masks[[3]]), as.array(cur_Masks[[2]]))
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 3]
    cur_img_H3 <- as.array(cur_Masks[[3]])
    cur_img_H3[cur_img_H3 == 0L] <- NA
    cur_ind <- match(cur_img_H3, cur_sce$CellNb)
    cur_img_H3[!is.na(cur_ind)] <- assay(cur_sce, "counts")["H3",cur_ind[!is.na(cur_ind)]]
    cur_img_H3 <- EBImage::normalize(cur_img_H3, separate = TRUE,
                                     inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                    max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,1] <- cur_img_H3
    cur_img_CD99 <- as.array(cur_Masks[[3]])
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- as.array(cur_Masks[[3]])
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                       inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                      max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,3] <- cur_img_CDH

    cur_img[is.na(cur_img)] <- 0

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
})

test_that("On disk: images can be coloured by features.", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)

    cur_col <- list(H3 = c("black", "red"), CD99 = c("black", "green"), CDH = c("black", "blue"))
    colour_by <- c("H3", "CD99", "CDH")
    plottingParam <- list(scale = TRUE)

    expect_silent(cur_out <- .colourImageByFeature(image = cur_Images[1], colour_by = colour_by,
                                                   bcg = list(H3 = c(0,1,1), CD99 = c(0,1,1), CDH = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    
    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit$H3, as.numeric(quantile(cur_Images[[1]][,,"H3"], probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CD99, as.numeric(quantile(cur_Images[[1]][,,"CD99"], probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CDH, as.numeric(quantile(cur_Images[[1]][,,"CDH"], probs = c(0,1))))
    cur_out <- cur_out$imgs

    # Compare to EBImage
    cur_img <- getChannels(cur_Images[1], c("H3", "CD99", "CDH"))
    cur_img <- EBImage::normalize(as.array(cur_img[[1]]))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    expect_silent(cur_out <- .colourImageByFeature(image = cur_Images, colour_by = colour_by,
                                                   bcg = list(H3 = c(0,1,1), CD99 = c(0,1,1), CDH = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    
    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit$H3, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,"H3"])})), 
                                                           probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CD99, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,"CD99"])})), 
                                                             probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CDH, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,"CDH"])})), 
                                                            probs = c(0,1))))
    cur_out <- cur_out$imgs

    # Compare to EBImage
    H3_dist <- unlist(lapply(getChannels(cur_Images, "H3"), as.numeric))
    CD99_dist <- unlist(lapply(getChannels(cur_Images, "CD99"), as.numeric))
    CDH_dist <- unlist(lapply(getChannels(cur_Images, "CDH"), as.numeric))
    cur_img <- getChannels(cur_Images[1], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(cur_Images[2], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(cur_Images[3], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    # Set scale to FALSE
    plottingParam <- list(scale = FALSE)

    expect_silent(cur_out <- .colourImageByFeature(image = cur_Images, colour_by = colour_by,
                                                   bcg = list(H3 = c(0,1,1), CD99 = c(0,1,1), CDH = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    
    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit$H3, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,colour_by])})), 
                                                           probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CD99, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,colour_by])})), 
                                                             probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CDH, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,colour_by])})), 
                                                            probs = c(0,1))))
    cur_out <- cur_out$imgs

    all_dist <- unlist(lapply(getChannels(cur_Images, colour_by), as.numeric))
    cur_img <- getChannels(cur_Images[1], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(cur_Images[2], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(cur_Images[3], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    # Set bcg
    plottingParam <- list(scale = TRUE)

    expect_silent(cur_out <- .colourImageByFeature(image = cur_Images, colour_by = colour_by,
                                                   bcg = list(H3 = c(0,0.5,1), CD99 = c(0,1,2), CDH = c(10,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    
    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit$H3, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,"H3"])})), 
                                                           probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CD99, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,"CD99"])})), 
                                                             probs = c(0,1))))
    expect_equal(cur_out$cur_limit$CDH, as.numeric(quantile(unlist(lapply(cur_Images, function(x){as.numeric(x[,,"CDH"])})), 
                                                            probs = c(0,1))))
    cur_out <- cur_out$imgs

    # Compare to EBImage
    H3_dist <- unlist(lapply(getChannels(cur_Images, "H3"), as.numeric))
    CD99_dist <- unlist(lapply(getChannels(cur_Images, "CD99"), as.numeric))
    CDH_dist <- unlist(lapply(getChannels(cur_Images, "CDH"), as.numeric))
    cur_img <- getChannels(cur_Images[1], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(cur_Images[2], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(cur_Images[3], c("H3", "CD99", "CDH"))
    cur_img <- Image(as.array(cur_img[[1]]))
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- NULL

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
})

test_that("On disk: images can be outlined by metadata.", {
    data("pancreasImages")
    data("pancreasMasks")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)

    cur_col <- list(H3 = c("black", "red"), CD99 = c("black", "green"), CDH = c("black", "blue"))
    colour_by <- c("H3", "CD99", "CDH")
    plottingParam <- list(scale = TRUE)

    expect_silent(out_img <- .colourImageByFeature(image = cur_Images, colour_by = colour_by,
                                                   bcg = list(H3 = c(0,1,1), CD99 = c(0,1,1), CDH = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))
    out_img <- out_img$imgs

    cur_col <- c(celltype_A = "red", celltype_B = "green", celltype_C = "blue")

    expect_silent(cur_out <- .outlineImageByMeta(object = pancreasSCE, mask = cur_Masks,
                                                 out_img = out_img, cell_id = "CellNb", img_id = "ImageNb",
                                                 outline_by = "CellType",
                                                 cur_colour = cur_col,
                                                 thick = FALSE))
    
    expect_true(is.list(cur_out))
    expect_true(is.null(cur_out$cur_limit))
    cur_out <- cur_out$imgs

    # Compare to EBImage
    cur_img <- out_img[[1]]
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
    cur_mask <- as.array(cur_Masks[[1]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_A"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "red")
    cur_mask <- as.array(cur_Masks[[1]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_B"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "green")
    cur_mask <- as.array(cur_Masks[[1]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_C"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "blue")

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")))

    cur_img <- out_img[[2]]
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 2]
    cur_mask <- as.array(cur_Masks[[2]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_A"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "red")
    cur_mask <- as.array(cur_Masks[[2]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_B"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "green")
    cur_mask <- as.array(cur_Masks[[2]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_C"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "blue")

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")))

    cur_img <- out_img[[3]]
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 3]
    cur_mask <- as.array(cur_Masks[[3]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_A"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "red")
    cur_mask <- as.array(cur_Masks[[3]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_B"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "green")
    cur_mask <- as.array(cur_Masks[[3]])
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_C"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "blue")

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")))

    # Continous scale
    cur_col <- c("black", "red")

    expect_silent(cur_out <- .outlineImageByMeta(object = pancreasSCE, mask = cur_Masks,
                                                 out_img = out_img, cell_id = "CellNb", img_id = "ImageNb",
                                                 outline_by = "Area",
                                                 cur_colour = cur_col,
                                                 thick = FALSE))

    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit, list(Area = c(2, 200)))
    cur_out <- cur_out$imgs
    
    # Test for completely continous scale
    cur_col <- c("blue", "red")

    expect_silent(cur_out <- .outlineImageByMeta(object = pancreasSCE, mask = cur_Masks,
                                                 out_img = out_img, cell_id = "CellNb", img_id = "ImageNb",
                                                 outline_by = "Pos_X",
                                                 cur_colour = cur_col,
                                                 thick = FALSE))
    expect_true(is.list(cur_out))
    expect_equal(cur_out$cur_limit, list(Pos_X = as.numeric(quantile(pancreasSCE$Pos_X, probs = c(0, 1)))))
    cur_out <- cur_out$imgs

})

test_that("On disk: images can be displayed.", {
    data("pancreasImages")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)

    # Generate images to plot
    out_img <- .colourImageByFeature(image = cur_Images, colour_by = "CD99",
                                     bcg = list(CD99 = c(0,1,1)), cur_colour = list(CD99 = c("black", "red")),
                                     plottingParam = list(scale = TRUE))
    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    cur_limits <- list(colour_by = out_img$cur_limit)

    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                   image = cur_Images,
                   exprs_values = "counts",
                   outline_by = NULL,
                   colour_by = "CD99",
                   mask = NULL,
                   out_img = out_img$imgs,
                   img_id = NULL,
                   cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                   plottingParam = plottingParam, cur_limits = cur_limits))
    expect_true(is.null(cur_out))

    plottingParam$legend <- NULL
    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                                            image = cur_Images,
                                            exprs_values = "counts",
                                            outline_by = NULL,
                                            colour_by = "CD99",
                                            mask = NULL,
                                            out_img = out_img$imgs,
                                            img_id = NULL,
                                            cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                                            plottingParam = plottingParam, cur_limits = cur_limits))
    expect_true(is.null(cur_out))

    plottingParam$legend <- NULL
    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                                            image = cur_Images,
                                            exprs_values = "counts",
                                            outline_by = NULL,
                                            colour_by = "CD99",
                                            mask = NULL,
                                            out_img = out_img$imgs,
                                            img_id = NULL,
                                            cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                                            plottingParam = plottingParam, cur_limits = cur_limits))
    expect_true(is.null(cur_out))

    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    plottingParam$return_plot <- TRUE
    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                                            image = cur_Images,
                                            exprs_values = "counts",
                                            outline_by = NULL,
                                            colour_by = "CD99",
                                            mask = NULL,
                                            out_img = out_img$imgs,
                                            img_id = NULL,
                                            cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                                            plottingParam = plottingParam, cur_limits = cur_limits))
    expect_true(is(cur_out, "recordedplot"))

    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    plottingParam$return_plot <- TRUE
    plottingParam$display <- "single"
    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                                            image = cur_Images,
                                            exprs_values = "counts",
                                            outline_by = NULL,
                                            colour_by = "CD99",
                                            mask = NULL,
                                            out_img = out_img$imgs,
                                            img_id = NULL,
                                            cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                                            plottingParam = plottingParam, cur_limits = cur_limits))
    expect_true(is(cur_out, "list"))
    expect_length(cur_out, 4)
    expect_equal(names(cur_out), c("legend", "E34_imc", "G01_imc", "J02_imc"))
    expect_true(is(cur_out[[1]], "recordedplot"))
    expect_true(is(cur_out[[2]], "recordedplot"))
    expect_true(is(cur_out[[3]], "recordedplot"))
    expect_true(is(cur_out[[4]], "recordedplot"))
})

test_that("On disk: legend can be plotted.", {
    data("pancreasSCE")
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)

    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    colour_by = c("CD99", "H3", "CDH", "CD8a")
    cur_col = list(colour_by = list(CD99 = c("black", "red"),
                                    H3 = c("black", "green"),
                                    CDH = c("black", "blue"),
                                    CD8a = c("black", "cyan")))
    out_img <- .colourImageByFeature(image = cur_Images, colour_by = colour_by,
                                     bcg = NULL, cur_colour = cur_col$colour_by,
                                     plottingParam = list(scale = TRUE))
    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    cur_limits <- list(colour_by = out_img$cur_limit)

    x_len <- 100
    y_len <- 100

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))
    

    expect_silent(.plotLegend(object = pancreasSCE,
                image = cur_Images,
                exprs_values = "counts",
                outline_by = NULL,
                colour_by = colour_by,
                m_width = x_len,
                m_height = y_len,
                cur_col = cur_col,
                plottingParam, cur_limits = cur_limits))

    x_len <- 1000
    y_len <- 1000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = cur_Images,
                              exprs_values = "counts",
                              outline_by = NULL,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))

    x_len <- 2000
    y_len <- 2000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = cur_Images,
                              exprs_values = "counts",
                              outline_by = NULL,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))
    
    x_len <- 20
    y_len <- 20
    
    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")
    
    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))
    
    expect_silent(.plotLegend(object = pancreasSCE,
                              image = cur_Images,
                              exprs_values = "counts",
                              outline_by = NULL,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))

    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    colour_by = "CellType"
    cur_col = list(colour_by = list(CellType = c(celltype_A = "red",
                                                 celltype_B = "blue",
                                                 celltype_C = "green")))
    cur_limits <- list(colour_by = NULL)

    x_len <- 100
    y_len <- 100

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = NULL,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))

    x_len <- 1000
    y_len <- 1000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = NULL,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))

    x_len <- 2000
    y_len <- 2000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = NULL,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))

    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    colour_by = "CellType"
    outline_by = "CellType"
    cur_col = list(colour_by = list(CellType = c(celltype_A = "red",
                                                 celltype_B = "blue",
                                                 celltype_C = "green")),
                   outline_by = list(CellType = c(celltype_A = "brown",
                                                 celltype_B = "magenta",
                                                 celltype_C = "cyan")))

    cur_limits <- list(colour_by = NULL, outline_by = NULL)
    
    x_len <- 100
    y_len <- 100

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = outline_by,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))

    x_len <- 1000
    y_len <- 1000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = outline_by,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))

    x_len <- 2000
    y_len <- 2000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = outline_by,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))
    
    # Continous scale
    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Masks)
    colour_by = "Area"
    outline_by = "Area"
    cur_col = list(colour_by = list(Area = inferno(100)),
                   outline_by = list(Area = viridis(100)))
    
    cur_limits <- list(colour_by = list(Area = c(min(pancreasSCE$Area), max(pancreasSCE$Area))),
                       outline_by = list(Area = c(min(pancreasSCE$Area), max(pancreasSCE$Area))))
    
    x_len <- 100
    y_len <- 100
    
    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")
    
    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))
    
    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = outline_by,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))
    
    x_len <- 1000
    y_len <- 1000
    
    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")
    
    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))
    
    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = outline_by,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))
    
    x_len <- 2000
    y_len <- 2000
    
    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")
    
    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))
    
    expect_silent(.plotLegend(object = pancreasSCE,
                              image = NULL,
                              exprs_values = "counts",
                              outline_by = outline_by,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam, cur_limits = cur_limits))


})

test_that("On disk: scale bar can be plotted.", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    
    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    plottingParam$scale_bar$colour <- "black"

    x_len <- 100
    y_len <- 100

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "black")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotScaleBar(scale_bar = plottingParam$scale_bar,
                  xl = 0, xr = x_len, yt = 0, yb = y_len,
                  m_w = x_len, m_h = y_len))

    x_len <- 1000
    y_len <- 1000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "black")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotScaleBar(scale_bar = plottingParam$scale_bar,
                                xl = 0, xr = x_len, yt = 0, yb = y_len,
                                m_w = x_len, m_h = y_len))

    x_len <- 50
    y_len <- 50

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "black")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotScaleBar(scale_bar = plottingParam$scale_bar,
                                xl = 0, xr = x_len, yt = 0, yb = y_len,
                                m_w = x_len, m_h = y_len))

})

test_that("On disk: title can be set", {
    data("pancreasImages")
    data("pancreasSCE")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)

    # Generate images to plot
    out_img <- .colourImageByFeature(image = cur_Images, colour_by = "CD99",
                                     bcg = list(CD99 = c(0,1,1)), cur_colour = list(CD99 = c("black", "red")),
                                     plottingParam = list(scale = TRUE))
    plottingParam <- .plottingParam(dotArgs = list(), image = cur_Images)
    plottingParam$image_title$colour <- "black"
    plottingParam$image_title$text <- "test"

    x_len <- 100
    y_len <- 100

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "black")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotImageTitle(out_img = NULL,
                                  mask = NULL,
                                  image = NULL,
                                  img_id = NULL,
                                  ind = 1,
                                  legend_ind = 0,
                                  image_title = plottingParam$image_title,
                                  dim_x = x_len,
                                  xl = 0, xr = x_len, yt = 0, yb = y_len,
                                  m_h = y_len))

    x_len <- 1000
    y_len <- 1000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "black")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotImageTitle(out_img = NULL,
                                  mask = NULL,
                                  image = NULL,
                                  img_id = NULL,
                                  ind = 1,
                                  legend_ind = 0,
                                  image_title = plottingParam$image_title,
                                  dim_x = x_len,
                                  xl = 0, xr = x_len, yt = 0, yb = y_len,
                                  m_h = y_len))

    x_len <- 12
    y_len <- 12

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "black")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotImageTitle(out_img = NULL,
                                  mask = NULL,
                                  image = NULL,
                                  img_id = NULL,
                                  ind = 1,
                                  legend_ind = 0,
                                  image_title = plottingParam$image_title,
                                  dim_x = x_len,
                                  xl = 0, xr = x_len, yt = 0, yb = y_len,
                                  m_h = y_len))
})


