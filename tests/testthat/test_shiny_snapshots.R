test_that("header,sidebar,body creates expected HTML", {
  data("pancreasSCE")
  data("pancreasMasks")
  data("pancreasImages")
  expect_snapshot_output(
    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb", 
                                                          cell_id = "CellNb", mask = pancreasMasks, image = pancreasImages), {
                                                            session$setInputs(sample = 1,
                                                                              plotCount = 1,
                                                                              assay = "counts",
                                                                              exprs_marker_1 = "H3",
                                                                              exprs_marker_2 = "CD99",
                                                                              contrast_marker_1 = 1,
                                                                              contrast_marker_2 = 1)
                                                            testthat_print(.cytomapper_header())
                                                            testthat_print(.cytomapper_sidebar())
                                                            testthat_print(.addPlots_tab1())
                                                            testthat_print(.addPlots_tab2())})
  )
})


