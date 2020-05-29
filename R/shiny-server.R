# -----------------------------------------------------------------------------
# Helper functions to modify the server side of the shiny app
# -----------------------------------------------------------------------------

# Function to generate colours
.create_colours <- function(x){
    cur_col <- c("#70C389", "#39BEB4", "#3F85A7", "#494579", "#5B1C55",
                 "#971B4B", "#C81F43", "#F26738", "#F79C1D", "#F7CD0F",
                 "#EBE24A", "#B4D55A")
    return(cur_col[x])
}

# Function to report gate bounds
.brushRange <- function(brush) {
    if(is.null(brush)) return("NULL\n")
    paste0("xmin = ", round(brush$xmin, 1), " xmax = ", round(brush$xmax, 1),
           " ymin = ", round(brush$ymin, 1), " ymax = ", round(brush$ymax, 1))
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
.create_interactive_observer <- function(object, img_id, input, rValues, 
                                         objValues, markValues, brushValues){
    
    # Select first object
    observeEvent(input$sample, {
        objValues$object1 <- object[,colData(object)[,img_id] == input$sample]
    }, ignoreInit = TRUE)
    
    observeEvent(input$assay, {
        cur_ranges <- rowRanges(assay(object, input$assay))
        rownames(cur_ranges) <- rownames(object)
        rValues$ranges <- cur_ranges
    }, ignoreInit = TRUE)
    
    # Observe marker change
    observe({
        for (i in grep("Marker_", names(input), value = TRUE)) {
            markValues[[i]] <- input[[i]]
        }
    })
    
    # Observe brush change
    observe({
        for (i in grep("plot_brush", names(input), value = TRUE)) {
            brushValues[[i]] <- input[[i]]
        }
    })
    
    # Observe plot counter
    observeEvent(input$add_plot, {
        rValues$plotCount <- rValues$plotCount + 1
        
        max_val <- ((rValues$plotCount) * 2) - 1
        markValues[[paste0("Marker_", max_val)]] <- NULL
        markValues[[paste0("Marker_", max_val + 1)]] <- NULL
        
        objValues[[paste0("object", rValues$plotCount)]] <- NULL
        
        brushValues[[paste0("plot_brush", rValues$plotCount)]] <- NULL
    })
    
    # Smallest number should be 1
    observeEvent(input$remove_plot, {
        rValues$plotCount <- rValues$plotCount - 1
        if (rValues$plotCount < 1) {
            rValues$plotCount <- 1
        }
        
        max_val <- ((rValues$plotCount + 1) * 2) - 1
        markValues[[paste0("Marker_", max_val)]] <- NULL
        markValues[[paste0("Marker_", max_val + 1)]] <- NULL
        
        objValues[[paste0("object", rValues$plotCount + 1)]] <- NULL
        
        brushValues[[paste0("plot_brush", rValues$plotCount + 1)]] <- NULL
    })
}

# Create updateSelectizeInput objects
.create_updateSelectizeInput <- function(object, img_id, rValues, input, session, markValues){
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
    observeEvent(rValues$plotCount, {
        
        for (i in seq_len(rValues$plotCount)) {
            cur_val <- (i * 2) - 1
            
            # Select initial markers
            if (is.null(markValues[[paste0("Marker_", cur_val)]])) {
                cur_marker_1 <- ""
            } else {
                cur_marker_1 <- markValues[[paste0("Marker_", cur_val)]]
            }
            
            if (is.null(markValues[[paste0("Marker_", cur_val + 1)]])) {
                cur_marker_2 <- ""
            } else {
                cur_marker_2 <- markValues[[paste0("Marker_", cur_val + 1)]]
            }
            
            updateSelectizeInput(session, paste0("Marker_", cur_val),
                                 choices = markers,
                                 server = TRUE,
                                 selected = cur_marker_1)
            updateSelectizeInput(session, paste0("Marker_", cur_val + 1),
                                 choices = markers,
                                 server = TRUE, 
                                 selected = cur_marker_2)
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
                               options = list(placeholder = 'Select marker', 
                                              maxItems = 1)),
                selectizeInput(paste0("Marker_", cur_val + 1), 
                               label = span(paste("Select marker", cur_val + 1), 
                                            style = "color: black; padding-top: 0px"), 
                               choices = NULL,
                               options = list(placeholder = 'Select marker', 
                                              maxItems = 1)), 
                style = paste0("background-color: ", .create_colours(cur_plot), 
                               "; border-color: ", .create_colours(cur_plot),
                               "; padding-bottom: 0px; padding-top: 0px"))
        })
    })
}

.addPlots_main <- function(rValues) {
    
    renderUI({
        cur_row <- ceiling(rValues$plotCount / 3)
        # Generate boxes
        box_list <- lapply(seq_len(rValues$plotCount), function(cur_plot) {
            
            box(plotOutput(paste0("scatter", cur_plot), 
                           brush = brushOpts(paste0("plot_brush", cur_plot),
                                             fill = .create_colours(cur_plot),
                                             stroke = .create_colours(cur_plot))),
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
.createScatter <- function(input, rValues, objValues, markValues, iter){
    renderPlot({
        cur_val <- (iter * 2) - 1
        
        req(rValues$ranges, objValues[[paste0("object", iter)]], 
            input$assay, markValues[[paste0("Marker_", cur_val)]])
        
        # Build data frame for visualization
        cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]], 
                                         input$assay)))
        cur_df$sample <- input$sample
        
        if(markValues[[paste0("Marker_", cur_val + 1)]] != ""){

            # Scatter plot    
            ggplot(cur_df) +
                geom_point(aes_(as.name(markValues[[paste0("Marker_", cur_val)]]), 
                                as.name(markValues[[paste0("Marker_", cur_val + 1)]])), 
                           show.legend = FALSE) +
                ylab(input[[paste0("Marker_", cur_val + 1)]]) +
                theme_minimal() + 
                ylim(c(rValues$ranges[markValues[[paste0("Marker_", cur_val + 1)]], 1], 
                       rValues$ranges[markValues[[paste0("Marker_", cur_val + 1)]], 2])) +
                xlim(c(rValues$ranges[markValues[[paste0("Marker_", cur_val)]], 1], 
                       rValues$ranges[markValues[[paste0("Marker_", cur_val)]], 2]))
            
        } else {
            
            # Distributions of marker proteins
            ggplot(cur_df) +
                geom_density(aes_(x = as.name(markValues[[paste0("Marker_", cur_val)]])), 
                             show.legend = FALSE) +
                theme(axis.text.y = element_blank(),
                      panel.background = element_blank()) +
                ylab("") +
                xlim(c(rValues$ranges[markValues[[paste0("Marker_", cur_val)]], 1], 
                       rValues$ranges[markValues[[paste0("Marker_", cur_val)]], 2]))
        }
    })
}

.brushObject <- function(input, objValues, markValues, brushValues, iter){
    
    cur_val <- (iter * 2) - 1
        
    req(objValues[[paste0("object", iter)]], 
        input$assay, markValues[[paste0("Marker_", cur_val)]], 
        brushValues[[paste0("plot_brush", iter)]])
        
    # Build data frame 
    cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]], input$assay)))
    cur_df$sample <- input$sample
    
    # Brush the data.frame
    cur_selection <- brushedPoints(cur_df, brushValues[[paste0("plot_brush", iter)]], allRows = TRUE)
        
    # Save the Gate
    cur_gate <- list()
    cur_gate$gate <- matrix(data = c(brushValues[[paste0("plot_brush", iter)]]$xmin, 
                                     brushValues[[paste0("plot_brush", iter)]]$xmax, 
                                     brushValues[[paste0("plot_brush", iter)]]$ymin, 
                                     brushValues[[paste0("plot_brush", iter)]]$ymax),
                       nrow = 2, ncol = 2,
                       byrow = TRUE,
                       dimnames = list(c(markValues[[paste0("Marker_", cur_val)]], 
                                         markValues[[paste0("Marker_", cur_val + 1)]]), c("min", "max")))
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
    
    # Reactive object for marker names
    markValues <- reactiveValues(Marker_1 = NULL,
                                 Marker_2 = NULL)
    
    # Reactive object for brush values
    brushValues <- reactiveValues(plot_brush1 = NULL)
    
    .create_interactive_observer(object, img_id, input, rValues, objValues, markValues, brushValues)
    
    # Create updateSelectizeInput objects
    .create_updateSelectizeInput(object, img_id, rValues, input, session, markValues)
    
    # Dynamically generate wellPanels
    output$AdditionalPlots_sidebar <- .addPlots_sidebar(rValues)
    
    # Dynamically generate scatter plots
    output$AdditionalPlots_main <- .addPlots_main(rValues)
    
    # Create scatter plots
    observe({
        
        lapply(seq_len(rValues$plotCount), function(x){
            cur_val <- (x * 2) - 1
            
            if (x > 1) {
                    if (is.null(input[[paste0("plot_brush", x - 1)]])) {
                        output[[paste0("scatter", x)]] <- NULL
                    } else {
                        output[[paste0("scatter", x)]] <- .createScatter(input, rValues, objValues, markValues,
                                                                         iter = x)
                        
                        output[[paste0("info", x)]] <- renderText({
                            paste0("Selection: ", .brushRange(brushValues[[paste0("plot_brush", x)]]))
                        })
                        
                        .brushObject(input, objValues, markValues, brushValues, iter = x)
                    }
            } else {
                
                output[[paste0("scatter", x)]] <- .createScatter(input, rValues, objValues, markValues,
                                                                 iter = x)
                output[[paste0("info", x)]] <- renderText({
                    paste0("Selection: ", .brushRange(brushValues[[paste0("plot_brush", x)]]))
                })
                .brushObject(input, objValues, markValues, brushValues, iter = x)
            }
            
            
        })
    })
    
    output$image_expression <- renderPlot({
        
        cur_val <- (rValues$plotCount * 2) - 1
        
        req(markValues$Marker_1)
        
        if (rValues$plotCount > 1) {
            if (!is.null(brushValues[[paste0("plot_brush", rValues$plotCount - 1)]]) &&
                markValues[[paste0("Marker_", cur_val)]] != "") {
                if (markValues[[paste0("Marker_", cur_val + 1)]] == "") {
                    cur_markers <- markValues[[paste0("Marker_", cur_val)]]
                } else {
                    cur_markers <- c(markValues[[paste0("Marker_", cur_val)]], 
                                     markValues[[paste0("Marker_", cur_val + 1)]])
                }
            } else {
                if (markValues[[paste0("Marker_", cur_val - 1)]] == "") {
                    cur_markers <- markValues[[paste0("Marker_", cur_val - 2)]]
                } else {
                    cur_markers <- c(markValues[[paste0("Marker_", cur_val - 2)]], 
                                     markValues[[paste0("Marker_", cur_val - 1)]])
                } 
            }
        } else {
            if (markValues[[paste0("Marker_", cur_val + 1)]] == "") {
                cur_markers <- markValues[[paste0("Marker_", cur_val)]]
            } else {
                cur_markers <- c(markValues[[paste0("Marker_", cur_val)]], 
                                 markValues[[paste0("Marker_", cur_val + 1)]])
            }
        }
        
        if (is.null(image)) {
            cur_mask <- mask[mcols(mask)[,img_id] == input$sample]
            plotCells(object = objValues$object1,
                      mask = cur_mask,
                      cell_id = cell_id,
                      img_id = img_id,
                      colour_by = cur_markers,
                      exprs_values = input$assay,
                      ...)
        } else {
            cur_image <- image[mcols(image)[,img_id] == input$sample]
            plotPixels(image = cur_image,
                       colour_by = cur_markers,
                       ...)
        }
        
    })
    
    output$image_selection <- renderPlot({
        
        #for (i in rev(names(objValues))) {
        #    if (!is.null(objValues[[i]])) {
        #        cur_object <- objValues[[i]]
        #        break
        #    }
        #}
        
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
        # Add session info
        # Add date
        
        #filename = "gated_sce.rds",
        #content = function(filename) {
        #    saveRDS(cur_object, filename)
        #}
    )
}

