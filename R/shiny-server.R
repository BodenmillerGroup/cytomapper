# -----------------------------------------------------------------------------
# Definition of the shiny server
# -----------------------------------------------------------------------------

#' @importFrom SummarizedExperiment assay
.cytomapper_server <- function(object, mask, image, cell_id, img_id,
                               input, output, session, ...)
{
    # Session info observer
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    # Sample change observer - change tab if sample changes
    observeEvent(input$sample, {
        updateTabsetPanel(session, "tabbox1",
                          selected = "tab1"
        )
    })
    
    # Assay change observer - change tab if assay changes
    observeEvent(input$assay, {
        updateTabsetPanel(session, "tabbox1",
                          selected = "tab1"
        )
    })

    # Save some variables used throught the app
    rValues <- reactiveValues(ranges = NULL)
    
    # Reactive object list to store selected cells
    objValues <- reactiveValues(object1 = NULL)
    
    .create_interactive_observer(object, img_id, input, rValues, objValues)
    
    # Create updateSelectizeInput objects
    .create_updateSelectizeInput(object, img_id, input, session)
    
    # Dynamically generate wellPanels
    output$AdditionalPlots_sidebar <- .addPlots_sidebar(input)
    
    # Dynamically generate scatter plots
    output$AdditionalPlots_tab1 <- .addPlots_tab1(input)
    
    # Dynamically create image plot
    output$AdditionalPlots_tab2 <- .addPlots_tab2(input, object, mask, image)
    
    observe({
        
        lapply(seq_len(input$plotCount), function(cur_plot){
            output[[paste0("scatter", cur_plot)]] <- .createScatter(input, session, rValues, objValues, 
                                                                 iter = cur_plot, img_id = img_id, cell_id = cell_id)
            
            output[[paste0("info", cur_plot)]] <- renderText({
                paste0("Selection: ", .brushRange(input[[paste0("plot_brush", cur_plot)]]))
            })
        })
    })
    
    if (!is.null(mask) || !is.null(image)) {
        output$image_expression <- .createImageExpression(input, object, mask, image, img_id, cell_id, ...)
        
        output$image_selection <- .createImageSelection(input, objValues, mask, image, img_id, cell_id, ...)
    }
    
    output$downloadData <- .downloadSelection(input, objValues)
}