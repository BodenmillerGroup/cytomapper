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
  # .colourMaskByFeature
})

test_that("masks can be outlined by metadata.", {
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


