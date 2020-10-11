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

test_that("Gates can be set", {
    data("pancreasMasks")
    data("pancreasSCE")
    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb", 
                                     cell_id = "CellNb", mask = pancreasMasks), {
                                         cur_gate_1 <- list(xmin = 0.5,
                                                            xmax = 1.5,
                                                            ymin = 12,
                                                            ymax = 34,
                                                            coords_css = list(xmin = 49,
                                                                              xmax = 154,
                                                                              ymin = 73,
                                                                              ymax = 258),
                                                            coords_img = list(xmin =  98,
                                                                              xmax = 309,
                                                                              ymin = 146,
                                                                              ymax =  516),
                                                            img_css_ratio = list(x = 2,
                                                                                 y = 2),
                                                            mapping = list(x = "sample",
                                                                           y = "CD99"),
                                                            domain = list(left = 0.4,
                                                                          right = 1.6,
                                                                          bottom = -1.820605,
                                                                          top = 42.91425,
                                                                          discrete_limits = list(x = "1")),
                                                            range = list(left = 66.25586,
                                                                         right = 309.0411,
                                                                         bottom = 757.7043,
                                                                         top = 10.9589),
                                                            log = list(x = NULL,
                                                                       y = NULL),
                                                            direction = "xy",
                                                            brushId = "plot_brush1",
                                                            outputId = "scatter1")
                                         
                                         session$setInputs(sample = 1,
                                                           plotCount = 1,
                                                           assay = "counts",
                                                           exprs_marker_1 = "CD99",
                                                           exprs_marker_2 = NULL,
                                                           plot_brush1 = cur_gate_1)
                                         
                                         rValues <- reactiveValues(ranges = NULL)
                                         objValues <- reactiveValues(object1 = pancreasSCE[,pancreasSCE$ImageNb == 1])
                                         
                                         expect_equal(output$info1, "Selection: xmin = 0.5 xmax = 1.5 ymin = 12 ymax = 34")
                                         
                                         .brushObject(input, session, objValues, iter = 1)
                                        
                                         
                                         
                                         
                                     })
}
)



