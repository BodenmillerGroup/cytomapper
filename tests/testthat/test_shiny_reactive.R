test_that("test reactive input and output", {
    data("pancreasImages")
    data("pancreasMasks")
    data("pancreasSCE")
    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb", 
                                     cell_id = "CellNb", mask = pancreasMasks, image = pancreasImages), {
                                         session$setInputs(sample = 1,
                                                           plotCount = 1,
                                                           assay = "counts",
                                                           exprs_marker_1 = "H3",
                                                           exprs_marker_2 = "CD99",
                                                           contrast_marker_1 = 1,
                                                           contrast_marker_2 = 1)
                                         expect_equal(input$sample, 1)
                                         expect_equal(input$plotCount, 1)
                                         expect_equal(input$assay, "counts")
                                         expect_equal(input$exprs_marker_1, "H3")
                                         expect_equal(input$exprs_marker_2, "CD99")
                                         expect_equal(input$contrast_marker_1, 1)
                                         expect_equal(input$contrast_marker_2, 1)
                                     })
}
)
