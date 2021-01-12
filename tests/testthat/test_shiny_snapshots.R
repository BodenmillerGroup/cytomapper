library(shinytest)

test_that("cytomapperShiny() works when only providing a SCE object", {
    # Don't run these tests on the CRAN build servers
    skip_on_cran()
    
    expect_pass(testApp(appDir = "Github/cytomapper/tests/testthat/apps/sce_app/", compareImages = FALSE))
})
