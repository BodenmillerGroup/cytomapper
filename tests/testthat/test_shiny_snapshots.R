library(shinytest)

test_that("cytomapperShiny() works when only providing a SCE object", {

    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "assay_selection", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "download", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "gating_assay_change", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "gating_1", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "gating_marker_change", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "gating_plot_reset", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "gating_sample_change", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "help", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "image_tab_selection", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "marker_selection", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "sample_selection", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "side_panel", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "tab_reset", compareImages = FALSE, quiet = TRUE))
    #expect_pass(testApp(appDir = "apps/sce_app/", testnames = "tab_reset_gate_clear", compareImages = FALSE, quiet = TRUE))
})

test_that("cytomapperShiny() works when masks are provided", {

    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "tab_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "simple_sample_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "simple_plot_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "simple_gating", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "simple_assay_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "marker_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "complex_sample_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "complex_plot_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "complex_gating", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app/", testnames = "complex_assay_change", compareImages = FALSE))
})

test_that("cytomapperShiny() works when masks and images are provided", {

    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "tab_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "simple_sample_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "simple_plot_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "simple_gating", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "simple_assay_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "marker_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "complex_sample_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "complex_plot_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "complex_gating", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "complex_assay_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app/", testnames = "contrasts_set", compareImages = FALSE))
})

test_that("On disk: cytomapperShiny() works when masks are provided", {
    
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "tab_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "simple_sample_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "simple_plot_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "simple_gating", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "simple_assay_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "marker_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "complex_sample_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "complex_plot_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "complex_gating", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/masks_app_ondisk/", testnames = "complex_assay_change", compareImages = FALSE))
})

test_that("On disk: cytomapperShiny() works when masks and images are provided", {
    
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "tab_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "simple_sample_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "simple_plot_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "simple_gating", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "simple_assay_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "marker_select", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "complex_sample_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "complex_plot_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "complex_gating", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "complex_assay_change", compareImages = FALSE))
    #expect_pass(testApp(appDir = "apps/images_app_ondisk/", testnames = "contrasts_set", compareImages = FALSE))
})
