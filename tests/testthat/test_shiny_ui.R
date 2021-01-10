
test_that("User interface is correctly rendered", {
    local_edition(3)

    cur_header <- .cytomapper_header()

    expect_equal(cur_header$name, "header")
    expect_equal(cur_header$attribs$class, "main-header")
    expect_null(cur_header$children[[1]])
    expect_snapshot_output(cur_header$children[[2]])
    expect_snapshot_output(cur_header$children[[3]])

    cur_sidebar <- .cytomapper_sidebar()

    expect_equal(cur_sidebar$name, "aside")
    expect_equal(cur_sidebar$attribs$id, "sidebarCollapsed")
    expect_equal(cur_sidebar$attribs$class, "main-sidebar")
    expect_equal(cur_sidebar$attribs$`data-collapsed`, "false")
    expect_null(cur_sidebar$children[[1]])
    expect_snapshot_output(cur_sidebar$children[[2]])

    skip_on_cran()
    cur_body <- .cytomapper_body()
    expect_equal(cur_body$name, "div")
    expect_equal(cur_body$attribs$class, "content-wrapper")
    cur_tab_id <- cur_body$children[[1]][[3]][[1]][[3]][[1]][[3]][[1]][[2]]$`data-tabsetid`
    expect_equal(as.character(cur_body$children[[1]]), paste0('<section class=\"content\">\n  <div class=\"col-sm-12\">\n    <div class=\"nav-tabs-custom\">\n      <ul class=\"nav nav-tabs shiny-tab-input\" id=\"tabbox1\" data-tabsetid=\"', cur_tab_id, '\">\n        <li class=\"active\">\n          <a href=\"#tab-', cur_tab_id, '-1\" data-toggle=\"tab\" data-value=\"tab1\">Scatter Plots</a>\n        </li>\n        <li>\n          <a href=\"#tab-', cur_tab_id, '-2\" data-toggle=\"tab\" data-value=\"tab2\">Images</a>\n        </li>\n      </ul>\n      <div class=\"tab-content\" data-tabsetid=\"', cur_tab_id, '\">\n        <div class=\"tab-pane active\" data-value=\"tab1\" id=\"tab-', cur_tab_id, '-1\">\n          <div id=\"AdditionalPlots_tab1\" class=\"shiny-html-output\"></div>\n        </div>\n        <div class=\"tab-pane\" data-value=\"tab2\" id=\"tab-', cur_tab_id, '-2\">\n          <div id=\"AdditionalPlots_tab2\" class=\"shiny-html-output\"></div>\n        </div>\n      </div>\n    </div>\n  </div>\n</section>'))
})

test_that("Sidebar is correctly rendered", {
    data("pancreasSCE")
  local_edition(3)

    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb",
                                     cell_id = "CellNb"),
               {
                   session$setInputs(plotCount = 1)

                  expect_snapshot_output(.addPlots_sidebar(input)(session)$html)

                   session$setInputs(plotCount = 2)

                   expect_snapshot_output(.addPlots_sidebar(input)(session)$html)

                   session$setInputs(plotCount = 3)

                   expect_snapshot_output(.addPlots_sidebar(input)(session)$html)

               })

})

test_that("Plots in tab 1 are correctly rendered", {
    data("pancreasSCE")
  local_edition(3)

    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb",
                                     cell_id = "CellNb"),
               {
                   session$setInputs(plotCount = 1)

                   expect_snapshot_output(.addPlots_tab1(input)(session)$html)

                   session$setInputs(plotCount = 2)

                   expect_snapshot_output(.addPlots_tab1(input)(session)$html)

                   session$setInputs(plotCount = 3)

                   expect_snapshot_output(.addPlots_tab1(input)(session)$html)

               })

})

test_that("Plots in tab 2 are correctly rendered", {
    data("pancreasSCE")
    data("pancreasMasks")
    data("pancreasImages")
    local_edition(3)

    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb",
                                     cell_id = "CellNb"),
               {

                   expect_null(.addPlots_tab2(input, object = pancreasSCE, mask = NULL, image = NULL))

               })

    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb",
                                     cell_id = "CellNb", mask = pancreasMasks,
                                     image = NULL),
               {

                 expect_snapshot_output(.addPlots_tab2(input, object = pancreasSCE, mask = pancreasMasks, image = NULL)(session)$html)

               })

    testServer(app = cytomapperShiny(object = pancreasSCE, img_id = "ImageNb",
                                     cell_id = "CellNb", mask = pancreasMasks,
                                     image = pancreasImages),
               {

                 expect_snapshot_output(.addPlots_tab2(input, object = pancreasSCE, mask = pancreasMasks, image = pancreasImages)(session)$html)
               })

})

