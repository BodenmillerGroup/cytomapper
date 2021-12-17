test_that("compImage function works", {
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    # Mock up realistic spillover matrix
    sm <- matrix(runif(25, min = 0, max = 0.1), ncol = 5, nrow = 5, 
                  dimnames = list(channelNames(pancreasImages),
                               channelNames(pancreasImages)))
    diag(sm) <- 1
    
    expect_silent(cur_out <- compImage(pancreasImages, sm)) 
    
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
