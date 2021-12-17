test_that("compImage function works", {
    data("pancreasImages")
    
    # Mock up realistic spillover matrix
    sm <- matrix(runif(25, min = 0, max = 0.1), ncol = 5, nrow = 5, 
                  dimnames = list(channelNames(pancreasImages),
                               channelNames(pancreasImages)))
    diag(sm) <- 1
    
    expect_silent(cur_out <- compImage(pancreasImages, sm))        
})
