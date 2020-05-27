# -----------------------------------------------------------------------------
# Helper functions to modify the server side of the shiny app
# -----------------------------------------------------------------------------

# Function to report gate bounds
.brushRange <- function(brush) {
    if(is.null(brush)) return("NULL\n")
    paste0("xmin = ", round(brush$xmin, 2), " xmax = ", round(brush$xmax, 2),
           "ymin = ", round(brush$ymin, 2), " ymax = ", round(brush$ymax, 2))
}

# Function to perform min max scaling
.scaling <- function(x){(x - min(x))/(max(x) - min(x))}

# Generate help text
.general_help <- function(){
    pre(
        h2("Using this Shiny app"),
        p("To use this shiny..."),
        h2("Selecting the markers"),
        p("Select the markers..."),
        h2("Visualization on the images"),
        p("The selected cells can be seen on the images")
    )
}

# Create general observers for header
.create_general_observer <- function(input, si){
    
    # Return session info
    observeEvent(input$SessionInfo, {
        showModal(modalDialog(
            pre(paste(capture.output(si), collapse = "\n")),
            size="l",fade=TRUE,
            footer=NULL, easyClose=TRUE,
            title = "Session Info",
        ))
    })
    
    # Return helptext
    observeEvent(input$Help, {
        showModal(modalDialog(
            .general_help(),
            size="l",fade=TRUE,
            footer=NULL, easyClose=TRUE,
            title = "Help",
        ))
    })
}

# Create interactive observers
.create_interactive_observer <- function(object, img_id, input, rValues, objValues){
    
    # Select first object
    observeEvent(input$sample, {
        objValues$object1 <- object[,colData(object)[,img_id] == input$sample]
    }, ignoreInit = TRUE)
    
    observeEvent(input$assay, {
        cur_ranges <- rowRanges(assay(object, input$assay))
        rownames(cur_ranges) <- rownames(object)
        rValues$ranges <- cur_ranges
    }, ignoreInit = TRUE)
    
    # Observe plot counter
    observeEvent(input$add_plot, {
        rValues$plotCount <- rValues$plotCount + 1
    })
    
    # Smallest number should be 1
    observeEvent(input$remove_plot, {
        rValues$plotCount <- rValues$plotCount - 1
        if (rValues$plotCount < 1) {
            rValues$plotCount <- 1
        }
    })
}

# Create updateSelectizeInput objects
.create_updateSelectizeInput <- function(object, img_id, rValues, session){
    # Store image IDs and marker names
    img_IDs <- colData(object)[,img_id]
    markers <- rownames(object)
    
    updateSelectizeInput(session, inputId = "sample",
                         choices = unique(img_IDs),
                         selected = unique(img_IDs)[1])
    updateSelectizeInput(session, inputId = "assay",
                         choices = assayNames(object),
                         server = TRUE,
                         selected = assayNames(object)[1])
    updateSelectizeInput(session, inputId = "reducedDim",
                         choices = reducedDimNames(object),
                         server = TRUE,
                         selected = reducedDimNames(object)[1])
    observe({
        for (i in seq_len(rValues$plotCount)) {
            cur_val <- (i * 2) - 1
            updateSelectizeInput(session, paste0("Marker_", cur_val),
                                 choices = markers,
                                 server = TRUE,
                                 selected = "")
            updateSelectizeInput(session, paste0("Marker_", cur_val + 1),
                                 choices = markers,
                                 server = TRUE, 
                                 selected = "")
        }
    })
}

# Create selectInput options in sidebar
.addPlots_sidebar <- function(rValues) {
    renderUI({
        lapply(seq_len(rValues$plotCount), function(cur_plot) {
            cur_val <- (cur_plot * 2) - 1
            wellPanel(
                h3(paste("Plot", cur_plot), style = "color: black"),
                selectizeInput(paste0("Marker_", cur_val), 
                               label = span(paste("Select marker", cur_val), 
                                            style = "color: black; padding-top: 0px"), 
                               choices = NULL,
                               options = list(placeholder = 'Select a marker name', 
                                              maxItems = 1)),
                selectizeInput(paste0("Marker_", cur_val + 1), 
                               label = span(paste("Select marker", cur_val + 1), 
                                            style = "color: black; padding-top: 0px"), 
                               choices = NULL,
                               options = list(placeholder = 'Select a marker name', 
                                              maxItems = 1)), 
                style = "background-color: lightblue; padding-bottom: 0px; padding-top: 0px")
        })
    })
}

.addPlots_main <- function(rValues) {
    
    renderUI({
        cur_row <- ceiling(rValues$plotCount / 3)
        # Generate boxes
        box_list <- lapply(seq_len(rValues$plotCount), function(cur_plot) {
            box(plotOutput(paste0("scatter", cur_plot), 
                           brush = paste0("plot_brush", cur_plot)),
                verbatimTextOutput(paste0("info", cur_plot)),
                title = paste("Plot", cur_plot), 
                status = "primary",
                width = 4)
            })
        
        lapply(seq_len(cur_row), function(cr) {
            cur_val <- (cr * 3) - 2
            fluidRow(box_list[seq.int(cur_val, cur_val + 2)])
        })
    })
}

# Create scatter plots
.createScatter <- function(input, rValues, objValues, iter){
    renderPlot({
        cur_val <- (iter * 2) - 1
        
        req(rValues$ranges, objValues[[paste0("object", iter)]], 
            input$assay, input[[paste0("Marker_", cur_val)]])
        
        # Build data frame for visualization
        cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]], 
                                         input$assay)))
        cur_df$sample <- input$sample
        
        if(input[[paste0("Marker_", cur_val + 1)]] != ""){

            # Scatter plot    
            ggplot(cur_df) +
                geom_point(aes_(as.name(input[[paste0("Marker_", cur_val)]]), 
                                as.name(input[[paste0("Marker_", cur_val + 1)]])), 
                           show.legend = FALSE) +
                ylab(input[[paste0("Marker_", cur_val + 1)]]) +
                theme_minimal() + 
                ylim(c(rValues$ranges[input[[paste0("Marker_", cur_val + 1)]], 1], 
                       rValues$ranges[input[[paste0("Marker_", cur_val + 1)]], 2])) +
                xlim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1], 
                       rValues$ranges[input[[paste0("Marker_", cur_val)]], 2]))
            
        } else {
            
            # Distributions of marker proteins
            ggplot(cur_df) +
                geom_density(aes_(x = as.name(input[[paste0("Marker_", cur_val)]])), 
                             show.legend = FALSE) +
                theme(axis.text.y = element_blank(),
                      panel.background = element_blank()) +
                ylab("") +
                xlim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1], 
                       rValues$ranges[input[[paste0("Marker_", cur_val)]], 2]))
        }
    })
}

.brushObject <- function(input, objValues, iter){
    
    cur_val <- (iter * 2) - 1
        
    req(objValues[[paste0("object", iter)]], 
        input$assay, input[[paste0("Marker_", cur_val)]], 
        input[[paste0("plot_brush", iter)]])
        
    # Build data frame 
    cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]], input$assay)))
    cur_df$sample <- input$sample
    
    # Brush the data.frame
    cur_selection <- brushedPoints(cur_df, input[[paste0("plot_brush", iter)]], allRows = TRUE)
        
    # Save the Gate
    cur_gate <- list()
    cur_gate$gate <- matrix(data = c(input[[paste0("plot_brush", iter)]]$xmin, 
                                     input[[paste0("plot_brush", iter)]]$xmax, 
                                     input[[paste0("plot_brush", iter)]]$ymin, 
                                     input[[paste0("plot_brush", iter)]]$ymax),
                       nrow = 2, ncol = 2,
                       byrow = TRUE,
                       dimnames = list(c(input[[paste0("Makrer_", cur_val)]], 
                                         input[[paste0("Makrer_", cur_val + 1)]]), c("min", "max")))
    cur_gate$exprs_values <- input$assay
    cur_gate$img_id <- input$sample
    
    # Save gates
    next_obj <- objValues[[paste0("object", iter)]]
    metadata(next_obj)[[paste0("cytomapper_gate_", iter)]] <- cur_gate
        
    objValues[[paste0("object", iter + 1)]] <- next_obj[,cur_selection$selected_]
    
}

# Create the server
#' @importFrom DelayedArray rowRanges
#' @importFrom SummarizedExperiment assay
.cytomapper_server <- function(object, mask, image, cell_id, img_id,
                               input, output, session, ...)
{
    # Session info observer
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    # Save some variables used throught the app
    rValues <- reactiveValues(ranges = NULL,
                              plotCount = 1)
    # Reactive object list to store selected cells
    objValues <- reactiveValues(object1 = NULL)
    
    .create_interactive_observer(object, img_id, input, rValues, objValues)
    
    # Create updateSelectizeInput objects
    .create_updateSelectizeInput(object, img_id, rValues, session)
    
    # Dynamically generate wellPanels
    output$AdditionalPlots_sidebar <- .addPlots_sidebar(rValues)
    
    # Dynamically generate scatter plots
    output$AdditionalPlots_main <- .addPlots_main(rValues)
    
    # Create scatter plots
    observe({
        
        lapply(seq_len(rValues$plotCount), function(x){
            cur_val <- (x * 2) - 1
            
            if (x > 1) {
                    if (is.null(input[[paste0("plot_brush", x - 1)]]) || 
                        input[[paste0("Marker_", cur_val)]] == "") {
                        output[[paste0("scatter", x)]] <- NULL
                    } else {
                        output[[paste0("scatter", x)]] <- .createScatter(input, rValues, objValues, 
                                                                         iter = x)
                        .brushObject(input, objValues, iter = x)
                    }
            } else {
                output[[paste0("scatter", x)]] <- .createScatter(input, rValues, objValues, 
                                                                 iter = x)
                .brushObject(input, objValues, iter = x)
            }
            
            
        })
    })
    
    output$image_expression <- renderPlot({
        
        #if (cur_marker2 == "") {
        #    cur_markers <- cur_marker1
        #} else {
        #    cur_markers <- c(cur_marker1, cur_marker2)
        #}
        
        #if (is.null(image)) {
        #    cur_mask <- mask[mcols(mask)[,img_id] == cur_sample]
        #    plotCells(object = cur_object,
        #              mask = cur_mask,
        #              cell_id = cell_id,
        #              img_id = img_id,
        #              colour_by = cur_markers,
        #              exprs_values = cur_assay,
        #              ...)
        #} else {
        #    cur_image <- image[mcols(image)[,img_id] == sample]
        #    plotPixels(image = cur_image,
        #               colour_by = cur_markers,
        #               ...)
        #}
        
    })
    
    output$image_selection <- renderPlot({
        #cur_object$selected <- TRUE
        #if(is.null(image)){
        #    cur_mask <- mask[mcols(mask)[,img_id] == sample] 
        #    plotCells(object = cur_object,
        #              mask = cur_mask,
        #              cell_id = cell_id,
        #              img_id = img_id,
        #              colour_by = "selected",
        #              colour = list(selected = c("TRUE" = "dark red", "FALSE" = "gray")),
        #              ...)
        #} else {
        #    cur_mask <- mask[mcols(mask)[,img_id] == sample]
        #    cur_image <- image[mcols(image)[,img_id] == sample]
        #    plotPixels(image = cur_image,
        #              object = cur_object,
        #              mask = cur_mask,
        #              cell_id = cell_id,
        #              img_id = img_id,
        #              colour_by = "selected",
        #              colour = list(selected = c("TRUE" = "dark red", "FALSE" = "gray")),
        #              ...)
        #}
        #cur_object$selected <- NULL
    })

    output$downloadData <- downloadHandler(
        #filename = "gated_sce.rds",
        #content = function(filename) {
        #    saveRDS(cur_object, filename)
        #}
    )
}

