data("pancreasImages")
data("pancreasMasks")
data("pancreasSCE")

testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb"), {
    session$setInputs(sample = 1,
                      plotCount = 1)
    expect_equal(input$sample, 1)
    
})
