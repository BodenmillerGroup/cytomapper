test_that("masks can be coloured by metadata.", {
    data("pancreasMasks")
    data("pancreasSCE")

    cur_col <- c(celltype_A = "red", celltype_B = "green", celltype_C = "blue")

    expect_silent(cur_out <- .colourMaskByMeta(object = pancreasSCE, mask = pancreasMasks,
                      cell_id = "CellNb", img_id = "ImageNb",
                      colour_by = "CellType", cur_colour = cur_col,
                      missing_colour = "grey", background_colour = "black"))

    # Test if cells are correctly coloured
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "green"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 1 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "blue"))

    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "green"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 2 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "blue"))

    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_A"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "red"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_B"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "green"))
    cur_ids <- pancreasSCE$CellNb[pancreasSCE$ImageNb == 3 & pancreasSCE$CellType == "celltype_C"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "blue"))

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
    cur_col <- c(celltype_A = "red", celltype_B = "green", celltype_C = "blue")

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
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[1]][pancreasMasks[[1]] %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 1]
    expect_true(all(cur_out[[1]][!(pancreasMasks[[1]] %in% cur_ids) & as.vector(pancreasMasks[[1]] != 0L)] == "grey"))

    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_A"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "red"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_B"]
    expect_true(all(cur_out[[2]][cur_out[[2]] %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[2]][pancreasMasks[[2]] %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 2]
    expect_true(all(cur_out[[2]][!(pancreasMasks[[2]] %in% cur_ids) & as.vector(pancreasMasks[[2]] != 0L)] == "grey"))

    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_A"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "red"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_B"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "green"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3 & cur_sce$CellType == "celltype_C"]
    expect_true(all(cur_out[[3]][pancreasMasks[[3]] %in% cur_ids] == "blue"))
    cur_ids <- cur_sce$CellNb[cur_sce$ImageNb == 3]
    expect_true(all(cur_out[[3]][!(pancreasMasks[[3]] %in% cur_ids) & as.vector(pancreasMasks[[3]] != 0L)] == "grey"))
})

test_that("masks can be coloured by features.", {
    data("pancreasMasks")
    data("pancreasSCE")

    cur_col <- list(H3 = c("black", "red"), CD99 = c("black", "green"), CDH = c("black", "blue"))
    plottingParam <- list(scale = TRUE)

    expect_silent(cur_out <- .colourMaskByFeature(object = pancreasSCE[,pancreasSCE$ImageNb == 1], mask = pancreasMasks[1],
                                                  cell_id = "CellNb", img_id = "ImageNb",
                                                  colour_by = c("H3", "CD99", "CDH"),
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
    cur_img_CD99 <- pancreasMasks[[1]]
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- pancreasMasks[[1]]
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img[,,3] <- cur_img_CDH

    cur_img <- EBImage::normalize(cur_img, separate = TRUE)
    cur_img[is.na(cur_img)] <- 0

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    # Now with global scaling
    expect_silent(cur_out <- .colourMaskByFeature(object = pancreasSCE, mask = pancreasMasks,
                                                  cell_id = "CellNb", img_id = "ImageNb",
                                                  colour_by = c("H3", "CD99", "CDH"),
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
    cur_img_CD99 <- pancreasMasks[[1]]
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                     inputRange = c(min(assay(pancreasSCE, "counts")["CD99",]),
                                                    max(assay(pancreasSCE, "counts")["CD99",])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- pancreasMasks[[1]]
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")["CDH",]),
                                                     max(assay(pancreasSCE, "counts")["CDH",])))
    cur_img[,,3] <- cur_img_CDH

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
    cur_img_CD99 <- pancreasMasks[[2]]
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")["CD99",]),
                                                     max(assay(pancreasSCE, "counts")["CD99",])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- pancreasMasks[[2]]
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                       inputRange = c(min(assay(pancreasSCE, "counts")["CDH",]),
                                                      max(assay(pancreasSCE, "counts")["CDH",])))
    cur_img[,,3] <- cur_img_CDH

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
    cur_img_CD99 <- pancreasMasks[[3]]
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")["CD99",]),
                                                     max(assay(pancreasSCE, "counts")["CD99",])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- pancreasMasks[[3]]
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
    cur_img_CD99 <- pancreasMasks[[1]]
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- pancreasMasks[[1]]
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                       inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                      max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,3] <- cur_img_CDH

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
    cur_img_CD99 <- pancreasMasks[[2]]
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- pancreasMasks[[2]]
    cur_img_CDH[cur_img_CDH == 0L] <- NA
    cur_ind <- match(cur_img_CDH, cur_sce$CellNb)
    cur_img_CDH[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CDH",cur_ind[!is.na(cur_ind)]]
    cur_img_CDH <- EBImage::normalize(cur_img_CDH, separate = TRUE,
                                       inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                      max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,3] <- cur_img_CDH

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
    cur_img_CD99 <- pancreasMasks[[3]]
    cur_img_CD99[cur_img_CD99 == 0L] <- NA
    cur_ind <- match(cur_img_CD99, cur_sce$CellNb)
    cur_img_CD99[!is.na(cur_ind)] <- assay(cur_sce, "counts")["CD99",cur_ind[!is.na(cur_ind)]]
    cur_img_CD99 <- EBImage::normalize(cur_img_CD99, separate = TRUE,
                                      inputRange = c(min(assay(pancreasSCE, "counts")[colour_by,]),
                                                     max(assay(pancreasSCE, "counts")[colour_by,])))
    cur_img[,,2] <- cur_img_CD99
    cur_img_CDH <- pancreasMasks[[3]]
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

test_that("images can be coloured by features.", {
    data("pancreasImages")

    cur_col <- list(H3 = c("black", "red"), CD99 = c("black", "green"), CDH = c("black", "blue"))
    colour_by <- c("H3", "CD99", "CDH")
    plottingParam <- list(scale = TRUE)

    expect_silent(cur_out <- .colourImageByFeature(image = pancreasImages[1], colour_by = colour_by,
                                                   bcg = list(H3 = c(0,1,1), CD99 = c(0,1,1), CDH = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))

    # Compare to EBImage
    cur_img <- getChannels(pancreasImages[1], c("H3", "CD99", "CDH"))
    cur_img <- EBImage::normalize(cur_img[[1]])

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    expect_silent(cur_out <- .colourImageByFeature(image = pancreasImages, colour_by = colour_by,
                                                   bcg = list(H3 = c(0,1,1), CD99 = c(0,1,1), CDH = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))

    # Compare to EBImage
    H3_dist <- unlist(lapply(getChannels(pancreasImages, "H3"), as.numeric))
    CD99_dist <- unlist(lapply(getChannels(pancreasImages, "CD99"), as.numeric))
    CDH_dist <- unlist(lapply(getChannels(pancreasImages, "CDH"), as.numeric))
    cur_img <- getChannels(pancreasImages[1], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(pancreasImages[2], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(pancreasImages[3], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    # Set scale to FALSE
    plottingParam <- list(scale = FALSE)

    expect_silent(cur_out <- .colourImageByFeature(image = pancreasImages, colour_by = colour_by,
                                                   bcg = list(H3 = c(0,1,1), CD99 = c(0,1,1), CDH = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))

    all_dist <- unlist(lapply(getChannels(pancreasImages, colour_by), as.numeric))
    cur_img <- getChannels(pancreasImages[1], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(pancreasImages[2], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(pancreasImages[3], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2], inputRange = c(min(all_dist), max(all_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3], inputRange = c(min(all_dist), max(all_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    # Set bcg
    plottingParam <- list(scale = TRUE)

    expect_silent(cur_out <- .colourImageByFeature(image = pancreasImages, colour_by = colour_by,
                                                   bcg = list(H3 = c(0,0.5,1), CD99 = c(0,1,2), CDH = c(10,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))

    # Compare to EBImage
    H3_dist <- unlist(lapply(getChannels(pancreasImages, "H3"), as.numeric))
    CD99_dist <- unlist(lapply(getChannels(pancreasImages, "CD99"), as.numeric))
    CDH_dist <- unlist(lapply(getChannels(pancreasImages, "CDH"), as.numeric))
    cur_img <- getChannels(pancreasImages[1], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(pancreasImages[2], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)

    cur_img <- getChannels(pancreasImages[3], c("H3", "CD99", "CDH"))
    cur_img <- Image(cur_img[[1]])
    cur_img[,,1] <- EBImage::normalize(cur_img[,,1] * 0.5, inputRange = c(min(H3_dist), max(H3_dist)))
    cur_img[,,2] <- EBImage::normalize(cur_img[,,2] ^ 2, inputRange = c(min(CD99_dist), max(CD99_dist)))
    cur_img[,,3] <- EBImage::normalize(cur_img[,,3] + 10, inputRange = c(min(CDH_dist), max(CDH_dist)))

    dimnames(cur_img) <- list(NULL, NULL, NULL)

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")), tolerance = 0.01)
})

test_that("images can be outlined by metadata.", {
    data("pancreasImages")
    data("pancreasMasks")
    data("pancreasSCE")

    cur_col <- list(H3 = c("black", "red"), CD99 = c("black", "green"), CDH = c("black", "blue"))
    colour_by <- c("H3", "CD99", "CDH")
    plottingParam <- list(scale = TRUE)

    expect_silent(out_img <- .colourImageByFeature(image = pancreasImages, colour_by = colour_by,
                                                   bcg = list(H3 = c(0,1,1), CD99 = c(0,1,1), CDH = c(0,1,1)),
                                                   cur_colour = cur_col, plottingParam = plottingParam))

    cur_col <- c(celltype_A = "red", celltype_B = "green", celltype_C = "blue")

    expect_silent(cur_out <- .outlineImageByMeta(object = pancreasSCE, mask = pancreasMasks,
                                                 out_img = out_img, cell_id = "CellNb", img_id = "ImageNb",
                                                 outline_by = "CellType",
                                                 cur_colour = cur_col, missing_colour = "grey"))

    # Compare to EBImage
    cur_img <- out_img[[1]]
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 1]
    cur_mask <- pancreasMasks[[1]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_A"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "red")
    cur_mask <- pancreasMasks[[1]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_B"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "green")
    cur_mask <- pancreasMasks[[1]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_C"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "blue")

    expect_equal(imageData(Image(cur_out[[1]])), imageData(Image(cur_img, colormode = "Color")))

    cur_img <- out_img[[2]]
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 2]
    cur_mask <- pancreasMasks[[2]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_A"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "red")
    cur_mask <- pancreasMasks[[2]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_B"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "green")
    cur_mask <- pancreasMasks[[2]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_C"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "blue")

    expect_equal(imageData(Image(cur_out[[2]])), imageData(Image(cur_img, colormode = "Color")))

    cur_img <- out_img[[3]]
    cur_sce <- pancreasSCE[,pancreasSCE$ImageNb == 3]
    cur_mask <- pancreasMasks[[3]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_A"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "red")
    cur_mask <- pancreasMasks[[3]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_B"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "green")
    cur_mask <- pancreasMasks[[3]]
    cur_mask[!(cur_mask %in% cur_sce$CellNb[cur_sce$CellType == "celltype_C"])] <- 0L
    cur_img <- paintObjects(cur_mask, Image(cur_img, colormode = "Color"), col = "blue")

    expect_equal(imageData(Image(cur_out[[3]])), imageData(Image(cur_img, colormode = "Color")))

    # Continous scale
    cur_col <- c("black", "red")

    expect_silent(cur_out <- .outlineImageByMeta(object = pancreasSCE, mask = pancreasMasks,
                                                 out_img = out_img, cell_id = "CellNb", img_id = "ImageNb",
                                                 outline_by = "Area",
                                                 cur_colour = cur_col, missing_colour = "grey"))

    # Test for completely continous scale
    cur_col <- c("blue", "red")

    expect_silent(cur_out <- .outlineImageByMeta(object = pancreasSCE, mask = pancreasMasks,
                                                 out_img = out_img, cell_id = "CellNb", img_id = "ImageNb",
                                                 outline_by = "Pos_X",
                                                 cur_colour = cur_col, missing_colour = "grey"))
})

test_that("colours can be selected.", {
    data("pancreasSCE")
    # Test the defaults
    # Discret
    cur_out <- .selectColours(object = pancreasSCE, colour_by = "CellType",
                   colour = NULL, call.arg = "colour_by")

    expect_equal(cur_out, list(CellType = c(celltype_B = "#A6CEE3",
                                            celltype_C = "#1F78B4",
                                            celltype_A = "#B2DF8A")))

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "CellType",
                              colour = NULL, call.arg = "outline_by")

    expect_equal(cur_out, list(CellType = c(celltype_B = "brown3",
                                            celltype_C = "#BC80BD",
                                            celltype_A = "#FDB462")))

    # Factor
    pancreasSCE$CellType2 <- factor(pancreasSCE$CellType)
    cur_out <- .selectColours(object = pancreasSCE, colour_by = "CellType2",
                              colour = NULL, call.arg = "colour_by")

    expect_equal(cur_out, list(CellType2 = c(celltype_B = "#A6CEE3",
                                            celltype_C = "#1F78B4",
                                            celltype_A = "#B2DF8A")))

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "CellType2",
                              colour = NULL, call.arg = "outline_by")

    expect_equal(cur_out, list(CellType2 = c(celltype_B = "brown3",
                                             celltype_C = "#BC80BD",
                                             celltype_A = "#FDB462")))

    # Logical
    cur_out <- .selectColours(object = pancreasSCE, colour_by = "Pattern",
                              colour = NULL, call.arg = "colour_by")

    expect_equal(cur_out, list(Pattern = c("FALSE" = "#A6CEE3",
                                             "TRUE" = "#1F78B4")))

    # Continous
    cur_out <- .selectColours(object = pancreasSCE, colour_by = "Area",
                              colour = NULL, call.arg = "colour_by")

    expect_equal(cur_out, list(Area = viridis(100)))

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "Area",
                              colour = NULL, call.arg = "outline_by")

    expect_equal(cur_out, list(Area = inferno(100)))

    # Expand the Number of features
    set.seed(1234)
    pancreasSCE$test <- sample(paste("test", 1:23), ncol(pancreasSCE), replace = TRUE)

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "test",
                              colour = NULL, call.arg = "colour_by")

    cur_vec <- c(brewer.pal(12, "Paired"),
                 brewer.pal(8, "Pastel2")[-c(3,5,8)],
                 brewer.pal(12, "Set3")[-c(2,3,8,9,11,12)])
    names(cur_vec) <- unique(pancreasSCE$test)

    expect_equal(cur_out, list(test = cur_vec))

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "test",
                              colour = NULL, call.arg = "outline_by")

    cur_vec <- rev(c(brewer.pal(12, "Paired"),
                     brewer.pal(8, "Pastel2")[-c(3,5,8)],
                     brewer.pal(12, "Set3")[-c(2,3,7,8,9,11,12)],
                     "brown3"))
    names(cur_vec) <- unique(pancreasSCE$test)

    expect_equal(cur_out, list(test = cur_vec))

    set.seed(1234)
    pancreasSCE$test <- sample(paste("test", 1:50), ncol(pancreasSCE), replace = TRUE)

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "test",
                              colour = NULL, call.arg = "colour_by")

    cur_vec <- viridis(50)
    names(cur_vec) <- unique(pancreasSCE$test)

    expect_equal(cur_out, list(test = cur_vec))

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "test",
                              colour = NULL, call.arg = "outline_by")

    cur_vec <- inferno(50)
    names(cur_vec) <- unique(pancreasSCE$test)

    expect_equal(cur_out, list(test = cur_vec))

    # Provide custom colours
    cur_out <- .selectColours(object = pancreasSCE, colour_by = "CellType",
                              colour = list(CellType = c(celltype_A = "red",
                                         celltype_B = "blue",
                                         celltype_C = "green")),
                              call.arg = "colour_by")

    expect_equal(cur_out, list(CellType = c(celltype_A = "red",
                                            celltype_B = "blue",
                                            celltype_C = "green")))

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "CellType",
                              colour = list(CellType = c(celltype_A = "red",
                                                         celltype_B = "blue",
                                                         celltype_C = "green")),
                              call.arg = "outline_by")

    expect_equal(cur_out, list(CellType = c(celltype_A = "red",
                                            celltype_B = "blue",
                                            celltype_C = "green")))

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "Area",
                              colour = list(Area = c("black", "red")),
                              call.arg = "colour_by")

    expect_equal(cur_out, list(Area = c("black", "red")))

    cur_out <- .selectColours(object = pancreasSCE, colour_by = "Area",
                              colour = list(Area = c("black", "red")),
                              call.arg = "outline_by")

    expect_equal(cur_out, list(Area = c("black", "red")))
})

test_that("min/max scaling works.", {
    set.seed(1234)
    cur_dist <- rnorm(1000)

    cur_out <- .minMaxScaling(cur_dist, min_x = min(cur_dist), max_x = max(cur_dist))
    expect_equal(max(cur_out), 1)
    expect_equal(min(cur_out), 0)
})

test_that("colour mixing works.", {
  test_col <- c("red", "green")
  expect_equal(.mixColours(test_col)[,1], c(red = 1, green = 1, blue = 0))

  test_col <- c("red", "blue")
  expect_equal(.mixColours(test_col)[,1], c(red = 1, green = 0, blue = 1))

  test_col <- c("blue", "green")
  expect_equal(.mixColours(test_col)[,1], c(red = 0, green = 1, blue = 1))

  test_col <- c("red", "green", "blue")
  expect_equal(.mixColours(test_col)[,1], c(red = 1, green = 1, blue = 1))

  test_col <- c("white", "red")
  expect_equal(.mixColours(test_col)[,1], c(red = 2, green = 1, blue = 1))

  # Compare to EBImage colour composition
  img1 <- Image(data = c("#FFED00", "#070a0a", "blue", "white"), dim = c(2,2,1))
  img2 <- Image(data = c("#FF0000", "#1ee6df", "blue", "red"), dim = c(2,2,1))
  img3 <- Image(data = c("#FF00AB", "#020808", "blue", "white"), dim = c(2,2,1))

  cur_image <- img1 + img2 + img3

  cur_test <- .mixColours(c("#070a0a", "#1ee6df", "#020808"))

  expect_equal(cur_test[1], imageData(cur_image)[2,1,1,1])
  expect_equal(cur_test[2], imageData(cur_image)[2,1,2,1])
  expect_equal(cur_test[3], imageData(cur_image)[2,1,3,1])

  cur_test <- .mixColours(c("#FFED00", "#FF0000", "#FF00AB"))

  expect_equal(cur_test[1], imageData(cur_image)[1,1,1,1])
  expect_equal(cur_test[2], imageData(cur_image)[1,1,2,1])
  expect_equal(cur_test[3], imageData(cur_image)[1,1,3,1])
})

test_that("colour vector creation works.", {
    data("pancreasSCE")

    expect_silent(cur_out <- .createColourVector(object = pancreasSCE,colour_by = "CD99",
                                   exprs_values = "counts", cur_colour = list(CD99 = c("black", "red")),
                                   plottingParam = list(scale = TRUE)))
    expect_s4_class(cur_out, "SingleCellExperiment")
    expect_true("CYTO_COLOUR" %in% names(int_colData(cur_out)))

    expect_silent(cur_out <- .createColourVector(object = pancreasSCE,colour_by = c("H3", "CD99"),
                                                 exprs_values = "counts",
                                                 cur_colour = list(CD99 = c("black", "red"),
                                                                   H3 = c("black", "green")),
                                                 plottingParam = list(scale = TRUE)))
    expect_s4_class(cur_out, "SingleCellExperiment")
    expect_true("CYTO_COLOUR" %in% names(int_colData(cur_out)))

    expect_silent(cur_out <- .createColourVector(object = pancreasSCE,colour_by = c("H3", "CD99"),
                                                 exprs_values = "exprs",
                                                 cur_colour = list(CD99 = c("black", "red"),
                                                                   H3 = c("black", "green")),
                                                 plottingParam = list(scale = TRUE)))
    expect_s4_class(cur_out, "SingleCellExperiment")
    expect_true("CYTO_COLOUR" %in% names(int_colData(cur_out)))

    # Generate example sce
    cur_test <- SingleCellExperiment(assay = list(counts = matrix(c(1:3, 1:3, 1:3, 1:3),
                                                                  ncol = 3,byrow = TRUE)))
    rownames(cur_test) <- paste0("test", 1:4)
    expect_silent(cur_out <- .createColourVector(object = cur_test,
                                                 colour_by = c("test1", "test2", "test3", "test4"),
                                                 exprs_values = "counts",
                                                 cur_colour = list(test1 = c("black", "red"),
                                                                   test2 = c("black", "green"),
                                                                   test3 = c("black", "blue"),
                                                                   test4 = c("black", "magenta")),
                                                 plottingParam = list(scale = TRUE)))

    cur_col <- col2rgb("red") + col2rgb("green") + col2rgb("blue") + col2rgb("magenta")
    cur_col <- (cur_col / max(cur_col)) * 255

    expect_equal(int_colData(cur_out)$CYTO_COLOUR[1], "#000000")
    expect_equal(as.numeric(col2rgb(int_colData(cur_out)$CYTO_COLOUR[2])), as.numeric(cur_col/2), tolerance = 0.5)
    expect_equal(as.numeric(col2rgb(int_colData(cur_out)$CYTO_COLOUR[3])), as.numeric(cur_col), tolerance = 0.5)

})

test_that("images can be displayed.", {
    data("pancreasImages")
    data("pancreasSCE")

    # Generate images to plot
    out_img <- .colourImageByFeature(image = pancreasImages, colour_by = "CD99",
                                     bcg = list(CD99 = c(0,1,1)), cur_colour = list(CD99 = c("black", "red")),
                                     plottingParam = list(scale = TRUE))
    plottingParam <- .plottingParam(dotArgs = list(), image = pancreasImages)

    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                   image = pancreasImages,
                   exprs_values = "counts",
                   outline_by = NULL,
                   colour_by = "CD99",
                   mask = NULL,
                   out_img = out_img,
                   img_id = NULL,
                   cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                   plottingParam = plottingParam))
    expect_true(is.null(cur_out))

    plottingParam$legend <- NULL
    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                                            image = pancreasImages,
                                            exprs_values = "counts",
                                            outline_by = NULL,
                                            colour_by = "CD99",
                                            mask = NULL,
                                            out_img = out_img,
                                            img_id = NULL,
                                            cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                                            plottingParam = plottingParam))
    expect_true(is.null(cur_out))

    plottingParam$legend <- NULL
    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                                            image = pancreasImages,
                                            exprs_values = "counts",
                                            outline_by = NULL,
                                            colour_by = "CD99",
                                            mask = NULL,
                                            out_img = out_img,
                                            img_id = NULL,
                                            cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                                            plottingParam = plottingParam))
    expect_true(is.null(cur_out))

    plottingParam <- .plottingParam(dotArgs = list(), image = pancreasImages)
    plottingParam$return_plot <- TRUE
    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                                            image = pancreasImages,
                                            exprs_values = "counts",
                                            outline_by = NULL,
                                            colour_by = "CD99",
                                            mask = NULL,
                                            out_img = out_img,
                                            img_id = NULL,
                                            cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                                            plottingParam = plottingParam))
    expect_true(is(cur_out, "recordedplot"))

    plottingParam <- .plottingParam(dotArgs = list(), image = pancreasImages)
    plottingParam$return_plot <- TRUE
    plottingParam$display <- "single"
    expect_silent(cur_out <- .displayImages(object = pancreasSCE,
                                            image = pancreasImages,
                                            exprs_values = "counts",
                                            outline_by = NULL,
                                            colour_by = "CD99",
                                            mask = NULL,
                                            out_img = out_img,
                                            img_id = NULL,
                                            cur_col = list(colour_by = list(CD99 = c("black", "red"))),
                                            plottingParam = plottingParam))
    expect_true(is(cur_out, "list"))
    expect_length(cur_out, 4)
    expect_equal(names(cur_out), c("legend", "E34_imc", "G01_imc", "J02_imc"))
    expect_true(is(cur_out[[1]], "recordedplot"))
    expect_true(is(cur_out[[2]], "recordedplot"))
    expect_true(is(cur_out[[3]], "recordedplot"))
    expect_true(is(cur_out[[4]], "recordedplot"))
})

test_that("legend can be plotted.", {
    data("pancreasSCE")
    data("pancreasImages")

    plottingParam <- .plottingParam(dotArgs = list(), image = pancreasImages)
    colour_by = c("CD99", "H3", "CDH", "CD8a")
    cur_col = list(colour_by = list(CD99 = c("black", "red"),
                                    H3 = c("black", "green"),
                                    CDH = c("black", "blue"),
                                    CD8a = c("black", "cyan")))

    x_len <- 100
    y_len <- 100

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                image = pancreasImages,
                exprs_values = "counts",
                outline_by = NULL,
                colour_by = colour_by,
                m_width = x_len,
                m_height = y_len,
                cur_col = cur_col,
                plottingParam))

    x_len <- 1000
    y_len <- 1000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = pancreasImages,
                              exprs_values = "counts",
                              outline_by = NULL,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam))

    x_len <- 2000
    y_len <- 2000

    par(bty="n", mai=c(0,0,0,0), xaxs="i",
        yaxs="i", xaxt="n", yaxt="n", col = "white")

    plot(c(0, x_len), c(0, y_len), type="n", xlab="", ylab="",
         asp = 1, ylim = rev(c(0, y_len)))

    expect_silent(.plotLegend(object = pancreasSCE,
                              image = pancreasImages,
                              exprs_values = "counts",
                              outline_by = NULL,
                              colour_by = colour_by,
                              m_width = x_len,
                              m_height = y_len,
                              cur_col = cur_col,
                              plottingParam))

    plottingParam <- .plottingParam(dotArgs = list(), image = pancreasImages)
    colour_by = "CellType"
    cur_col = list(colour_by = list(CellType = c(celltype_A = "red",
                                                 celltype_B = "blue",
                                                 celltype_C = "green")))

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
                              plottingParam))

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
                              plottingParam))

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
                              plottingParam))

    plottingParam <- .plottingParam(dotArgs = list(), image = pancreasImages)
    colour_by = "CellType"
    outline_by = "CellType"
    cur_col = list(colour_by = list(CellType = c(celltype_A = "red",
                                                 celltype_B = "blue",
                                                 celltype_C = "green")),
                   outline_by = list(CellType = c(celltype_A = "brown",
                                                 celltype_B = "magenta",
                                                 celltype_C = "cyan")))

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
                              plottingParam))

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
                              plottingParam))

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
                              plottingParam))


})

test_that("scale bar can be plotted.", {
    plottingParam <- .plottingParam(dotArgs = list(), image = pancreasImages)
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

test_that("title can be set", {
    data("pancreasImages")
    data("pancreasSCE")

    # Generate images to plot
    out_img <- .colourImageByFeature(image = pancreasImages, colour_by = "CD99",
                                     bcg = list(CD99 = c(0,1,1)), cur_colour = list(CD99 = c("black", "red")),
                                     plottingParam = list(scale = TRUE))
    plottingParam <- .plottingParam(dotArgs = list(), image = pancreasImages)
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


