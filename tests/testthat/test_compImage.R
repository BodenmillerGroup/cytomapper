test_that("compImage function works", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    # Mock up realistic spillover matrix
    sm_real <- matrix(c(1, 0.033, 0.01, 0.007, 0,
                   0.016, 1, 0.051, 0.01, 0,
                   0.004, 0.013, 1, 0.023, 0,
                   0.005, 0.008, 0.029, 1, 0.006,
                   0, 0, 0, 0.001, 1), byrow = TRUE,
                 ncol = 5, nrow = 5, 
                 dimnames = list(c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di"),
                                 c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")))
    
    sm_strong <- matrix(c(1, 0.33, 0.1, 0.07, 0,
                   0.16, 1, 0.51, 0.1, 0,
                   0.04, 0.13, 1, 0.23, 0,
                   0.05, 0.08, 0.29, 1, 0.06,
                   0, 0, 0, 0.01, 1), byrow = TRUE,
                 ncol = 5, nrow = 5, 
                 dimnames = list(c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di"),
                                 c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")))
    
    # Generate spillover images - strong spillover
    sm_images <- pancreasImages
    channelNames(sm_images) <- c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Dy161Di"])}))
    })
    channelNames(cur_ch) <- "Dy161Di"
    setChannels(sm_images,  "Dy161Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Dy162Di"])}))
    })
    channelNames(cur_ch) <- "Dy162Di"
    setChannels(sm_images,  "Dy162Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Dy163Di"])}))
    })
    channelNames(cur_ch) <- "Dy163Di"
    setChannels(sm_images,  "Dy163Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Dy164Di"])}))
    })
    channelNames(cur_ch) <- "Dy164Di"
    setChannels(sm_images,  "Dy164Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_strong[,"Ho165Di"])}))
    })
    channelNames(cur_ch) <- "Ho165Di"
    setChannels(sm_images,  "Ho165Di") <- cur_ch
    
    # Visual inspection
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(sm_images, colour_by = "Dy161Di")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(sm_images, colour_by = "Dy162Di")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(sm_images, colour_by = "Dy163Di")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(sm_images, colour_by = "Dy164Di")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(sm_images, colour_by = "Ho165Di")
    
    expect_silent(cur_out <- compImage(sm_images, sm_strong))
    
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(cur_out, colour_by = "Dy161Di")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(cur_out, colour_by = "Dy162Di")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(cur_out, colour_by = "Dy163Di")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(cur_out, colour_by = "Dy164Di")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(cur_out, colour_by = "Ho165Di")
    
    #expect_equal(imageData(pancreasImages$E34_imc)[,,1], imageData(cur_out$E34_imc)[,,1],
    #             check.attributes = FALSE)
    
    # weak spillover
    sm_images <- pancreasImages
    channelNames(sm_images) <- c("Dy161Di", "Dy162Di", "Dy163Di", "Dy164Di", "Ho165Di")
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Dy161Di"])}))
    })
    channelNames(cur_ch) <- "Dy161Di"
    setChannels(sm_images,  "Dy161Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Dy162Di"])}))
    })
    channelNames(cur_ch) <- "Dy162Di"
    setChannels(sm_images,  "Dy162Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Dy163Di"])}))
    })
    channelNames(cur_ch) <- "Dy163Di"
    setChannels(sm_images,  "Dy163Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Dy164Di"])}))
    })
    channelNames(cur_ch) <- "Dy164Di"
    setChannels(sm_images,  "Dy164Di") <- cur_ch
    
    cur_ch <- endoapply(sm_images, function(x){
        Image(apply(x, c(1,2), function(y){sum(y * sm_real[,"Ho165Di"])}))
    })
    channelNames(cur_ch) <- "Ho165Di"
    setChannels(sm_images,  "Ho165Di") <- cur_ch
    
    # Visual inspection
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(sm_images, colour_by = "Dy161Di")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(sm_images, colour_by = "Dy162Di")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(sm_images, colour_by = "Dy163Di")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(sm_images, colour_by = "Dy164Di")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(sm_images, colour_by = "Ho165Di")
    
    expect_silent(cur_out <- compImage(sm_images, sm_real))
    
    plotPixels(pancreasImages, colour_by = "H3")
    plotPixels(cur_out, colour_by = "Dy161Di")
    plotPixels(pancreasImages, colour_by = "CD99")
    plotPixels(cur_out, colour_by = "Dy162Di")
    plotPixels(pancreasImages, colour_by = "PIN")
    plotPixels(cur_out, colour_by = "Dy163Di")
    plotPixels(pancreasImages, colour_by = "CD8a")
    plotPixels(cur_out, colour_by = "Dy164Di")
    plotPixels(pancreasImages, colour_by = "CDH")
    plotPixels(cur_out, colour_by = "Ho165Di")

    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,1], imageData(sm_images$E34_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.001)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,1], imageData(cur_out$E34_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.001)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,2], imageData(sm_images$E34_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,2], imageData(cur_out$E34_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.05)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,3], imageData(sm_images$E34_imc)[,,3],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,3], imageData(cur_out$E34_imc)[,,3],
                 check.attributes = FALSE, tolerance = 0.05)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,4], imageData(sm_images$E34_imc)[,,4],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$E34_imc)[,,4], imageData(cur_out$E34_imc)[,,4],
                 check.attributes = FALSE, tolerance = 0.05)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$E34_imc)[,,5], imageData(sm_images$E34_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$E34_imc)[,,5], imageData(cur_out$E34_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,1], imageData(sm_images$G01_imc)[,,1],
                                  check.attributes = FALSE, tolerance = 0.001)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,1], imageData(cur_out$G01_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.001)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,2], imageData(sm_images$G01_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.005)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,2], imageData(cur_out$G01_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.005)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,3], imageData(sm_images$G01_imc)[,,3],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,3], imageData(cur_out$G01_imc)[,,3],
                 check.attributes = FALSE, tolerance = 0.05)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,4], imageData(sm_images$G01_imc)[,,4],
                                  check.attributes = FALSE, tolerance = 0.5)))
    expect_equal(imageData(pancreasImages$G01_imc)[,,4], imageData(cur_out$G01_imc)[,,4],
                 check.attributes = FALSE, tolerance = 0.5)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$G01_imc)[,,5], imageData(sm_images$G01_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$G01_imc)[,,5], imageData(cur_out$G01_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,1], imageData(sm_images$J02_imc)[,,1],
                                  check.attributes = FALSE, tolerance = 0.001)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,1], imageData(cur_out$J02_imc)[,,1],
                 check.attributes = FALSE, tolerance = 0.001)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,2], imageData(sm_images$J02_imc)[,,2],
                                  check.attributes = FALSE, tolerance = 0.005)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,2], imageData(cur_out$J02_imc)[,,2],
                 check.attributes = FALSE, tolerance = 0.005)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,3], imageData(sm_images$J02_imc)[,,3],
                                  check.attributes = FALSE, tolerance = 0.5)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,3], imageData(cur_out$J02_imc)[,,3],
                 check.attributes = FALSE, tolerance = 0.5)
    expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,4], imageData(sm_images$J02_imc)[,,4],
                                  check.attributes = FALSE, tolerance = 0.05)))
    expect_equal(imageData(pancreasImages$J02_imc)[,,4], imageData(cur_out$J02_imc)[,,4],
                 check.attributes = FALSE, tolerance = 0.05)
    #expect_false(isTRUE(all.equal(imageData(pancreasImages$J02_imc)[,,5], imageData(sm_images$J02_imc)[,,5],
    #                              check.attributes = FALSE, tolerance = 0.001)))
    #expect_equal(imageData(pancreasImages$J02_imc)[,,5], imageData(cur_out$J02_imc)[,,5],
    #             check.attributes = FALSE, tolerance = 0.001)
    
    # on disk
    cur_images <- CytoImageList(pancreasImages, on_disk = TRUE,
                                h5FilesPath = cur_path)
    
    expect_silent(cur_out_2 <- compImage(cur_images, sm)) 
    
    expect_equal(as.array(cur_out$E34_imc),
                 as.array(cur_out_2$E34_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_2$G01_imc))
    expect_equal(as.array(cur_out$G01_imc),
                 as.array(cur_out_2$G01_imc))
})
