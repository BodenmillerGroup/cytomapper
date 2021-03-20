test_that("On disk: Plots in tab 2 are correctly rendered", {
    data("pancreasSCE")
    data("pancreasMasks")
    data("pancreasImages")
    
    cur_path <- tempdir()
    on.exit(unlink(cur_path))
    
    cur_Images <- CytoImageList(pancreasImages, on_disk = TRUE, h5FilesPath = cur_path)
    cur_Masks <- CytoImageList(pancreasMasks, on_disk = TRUE, h5FilesPath = cur_path)
    
    
    local_edition(3)

    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb",
                                     cell_id = "CellNb"),
               {

                   expect_null(.addPlots_tab2(input, object = pancreasSCE, mask = NULL, image = NULL))

               })

    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb",
                                     cell_id = "CellNb", mask = cur_Masks,
                                     image = NULL),
               {

                 expect_snapshot_output(.addPlots_tab2(input, object = pancreasSCE, mask = cur_Masks, image = NULL)(session)$html)

               })

    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb",
                                     cell_id = "CellNb", mask = cur_Masks,
                                     image = cur_Images),
               {

                 expect_snapshot_output(.addPlots_tab2(input, object = pancreasSCE, mask = cur_Masks, image = cur_Images)(session)$html)
               })

})

