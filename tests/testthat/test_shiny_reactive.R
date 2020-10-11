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
                                                            mapping = list(x = "sample",
                                                                           y = "CD99"),
                                                            direction = "xy",
                                                            brushId = "plot_brush1",
                                                            outputId = "scatter1")
                                         
                                         session$setInputs(sample = 1,
                                                           plotCount = 1,
                                                           assay = "counts",
                                                           exprs_marker_1 = "CD99",
                                                           exprs_marker_2 = NULL,
                                                           plot_brush1 = cur_gate_1)
                                         
                                         objValues <- reactiveValues(object1 = pancreasSCE[,pancreasSCE$ImageNb == 1])
                                         
                                         expect_equal(output$info1, "Selection: xmin = 0.5 xmax = 1.5 ymin = 12 ymax = 34")
                                         
                                         .brushObject(input, session, objValues, iter = 1)
                                         
                                         expect_equal(counts(objValues$object2),
                                                      counts(objValues$object1)[,counts(objValues$object1)["CD99",] > 12 &
                                                                                   counts(objValues$object1)["CD99",] < 34])
                                         
                                         expect_equal(colnames(objValues$object2),
                                                      colnames(objValues$object1)[counts(objValues$object1)["CD99",] > 12 &
                                                                                    counts(objValues$object1)["CD99",] < 34])
                                         
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$gate[1],
                                                      12)
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$gate[2],
                                                      34)
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$exprs_values,
                                                      "counts")
                                         
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$img_id,
                                                      1)
                                         
                                         
                                     })
    
    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb", 
                                     cell_id = "CellNb", mask = pancreasMasks), {
                                         cur_gate_1 <- list(xmin = 0.5,
                                                            xmax = 1.5,
                                                            ymin = 1.5,
                                                            ymax = 2.5,
                                                            mapping = list(x = "CD99",
                                                                           y = "CDH"),
                                                            direction = "xy",
                                                            brushId = "plot_brush1",
                                                            outputId = "scatter1")
                                         
                                         session$setInputs(sample = 1,
                                                           plotCount = 1,
                                                           assay = "exprs",
                                                           exprs_marker_1 = "CD99",
                                                           exprs_marker_2 = "CDH",
                                                           plot_brush1 = cur_gate_1)
                                         
                                         objValues <- reactiveValues(object1 = pancreasSCE[,pancreasSCE$ImageNb == 1])
                                         
                                         expect_equal(output$info1, "Selection: xmin = 0.5 xmax = 1.5 ymin = 1.5 ymax = 2.5")
                                         
                                         .brushObject(input, session, objValues, iter = 1)
                                         
                                         expect_equal(assay(objValues$object2, "exprs"),
                                                      assay(objValues$object1, "exprs")[,assay(objValues$object1, "exprs")["CD99",] > 0.5 &
                                                                                    assay(objValues$object1, "exprs")["CD99",] < 1.5 &
                                                                                    assay(objValues$object1, "exprs")["CDH",] > 1.5 &
                                                                                    assay(objValues$object1, "exprs")["CDH",] < 2.5])
                                         
                                         expect_equal(colnames(objValues$object2),
                                                      colnames(objValues$object1)[assay(objValues$object1, "exprs")["CD99",] > 0.5 &
                                                                                      assay(objValues$object1, "exprs")["CD99",] < 1.5 &
                                                                                      assay(objValues$object1, "exprs")["CDH",] > 1.5 &
                                                                                      assay(objValues$object1, "exprs")["CDH",] < 2.5])
                                    
                                         
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$gate[1],
                                                      0.5)
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$gate[2],
                                                      1.5)
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$gate[3],
                                                      1.5)
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$gate[4],
                                                      2.5)
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$exprs_values,
                                                      "exprs")
                                         
                                         expect_equal(metadata(objValues$object2)$cytomapper_gate_1$img_id,
                                                      1)
                                         
                                         
                                     })
    
    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb", 
                                     cell_id = "CellNb", mask = pancreasMasks), {
                                         cur_gate_1 <- list(xmin = 0.5,
                                                            xmax = 1.5,
                                                            ymin = 1.5,
                                                            ymax = 2.5,
                                                            mapping = list(x = "CD99",
                                                                           y = "CDH"),
                                                            direction = "xy",
                                                            brushId = "plot_brush1",
                                                            outputId = "scatter1")
                                         
                                         cur_gate_2 <- list(xmin = 0.5,
                                                            xmax = 1.5,
                                                            ymin = 3,
                                                            ymax = 6,
                                                            mapping = list(x = "sample",
                                                                           y = "H3"),
                                                            direction = "xy",
                                                            brushId = "plot_brush1",
                                                            outputId = "scatter1")
                                         
                                         session$setInputs(sample = 1,
                                                           plotCount = 2,
                                                           assay = "exprs",
                                                           exprs_marker_1 = "CD99",
                                                           exprs_marker_2 = "CDH",
                                                           plot_brush1 = cur_gate_1,
                                                           plot_brush2 = cur_gate_2)
                                         
                                         objValues <- reactiveValues(object1 = pancreasSCE[,pancreasSCE$ImageNb == 1])
                                         
                                         expect_equal(output$info1, "Selection: xmin = 0.5 xmax = 1.5 ymin = 1.5 ymax = 2.5")
                                         expect_equal(output$info2, "Selection: xmin = 0.5 xmax = 1.5 ymin = 3 ymax = 6")
                                         
                                         .brushObject(input, session, objValues, iter = 1)
                                         .brushObject(input, session, objValues, iter = 2)
                                         
                                         expect_equal(assay(objValues$object3, "exprs"),
                                                      assay(objValues$object2, "exprs")[,assay(objValues$object2, "exprs")["H3",] > 3 &
                                                                                            assay(objValues$object2, "exprs")["H3",] < 6 ])
                                         
                                         expect_equal(colnames(objValues$object3),
                                                      colnames(objValues$object2)[assay(objValues$object2, "exprs")["H3",] > 3 &
                                                                                      assay(objValues$object2, "exprs")["H3",] < 6 ])
                                         
                                         
                                         expect_equal(metadata(objValues$object3)$cytomapper_gate_2$gate[1],
                                                      3)
                                         expect_equal(metadata(objValues$object3)$cytomapper_gate_2$gate[2],
                                                      6)

                                         expect_equal(metadata(objValues$object3)$cytomapper_gate_2$exprs_values,
                                                      "exprs")
                                         
                                         expect_equal(metadata(objValues$object3)$cytomapper_gate_2$img_id,
                                                      1)
                                         
                                         
        })
    
}
)



