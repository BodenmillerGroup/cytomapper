data("pancreasImages")
data("pancreasMasks")
data("pancreasSCE")

shiny::testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb", cell_id = "CellNb"), {
    session$setInputs(sample = 1,
                      plotCount = 1)
    expect_equal(input$sample, 1)

})
