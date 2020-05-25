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

# Create scatter plots
.createScatter <- function(input, rValues, markers){
    renderPlot({
        
        req(rValues$object, rValues$ranges, input$assay, markers[[1]]())
        
        # Build data frame for visualization
        cur_df <- as.data.frame(t(assay(rValues$object, input$assay)))
        cur_df$sample <- input$sample
        
        if(markers[[2]]() != ""){

            # Scatter plot    
            ggplot(cur_df) +
                geom_point(aes_(as.name(markers[[1]]()), as.name(markers[[2]]())), 
                           show.legend = FALSE) +
                ylab(markers[[2]]()) +
                theme_minimal() + 
                ylim(c(rValues$ranges[markers[[2]](), 1], rValues$ranges[markers[[2]](), 2])) +
                xlim(c(rValues$ranges[markers[[1]](), 1], rValues$ranges[markers[[1]](), 2]))
            
        } else {
            
            # Distributions of marker proteins
            ggplot(cur_df) +
                geom_density(aes_(x = as.name(markers[[1]]())), 
                             show.legend = FALSE) +
                theme(axis.text.y = element_blank(),
                      panel.background = element_blank()) +
                ylab("") +
                xlim(c(rValues$ranges[markers[[1]](), 1], rValues$ranges[markers[[1]](), 2]))
        }
    })
}

.brushObject <- function(input, rValues, markers, brush){
    
    observe({
        
        req(rValues$object, input$assay, markers[[1]](), brush())
        
        # Build data frame 
        cur_df <- as.data.frame(t(assay(rValues$object, input$assay)))
        cur_df$sample <- input$sample
    
        # Brush the data.frame
        cur_selection <- brushedPoints(cur_df, brush(), allRows = TRUE)
        
        # Save the Gate
        cur_gate <- list()
        cur_gate$gate <- matrix(data = c(brush()$xmin, brush()$xmax, 
                                    brush()$ymin, brush()$ymax),
                           nrow = 2, ncol = 2,
                           byrow = TRUE,
                           dimnames = list(c(markers[[1]](), markers[[2]]()), c("min", "max")))
        cur_gate$exprs_values <- input$assay
        cur_gate$img_id <- input$sample
    
        # Saved gates
        all_gates <- names(metadata(rValues$object))[grepl("cytomapper_gate", names(metadata(rValues$object)))]
        if (is.null(all_gates)) {
            metadata(rValues$object)$cytomapper_gate_1 <- cur_gate
        } else {
            cur_num <- as.numeric(vapply(all_gates, FUN = function(x){unlist(strsplit(x, split = "_"))[3]}, 
                                         FUN.VALUE = character(1)))
            metadata(rValues$object)[[paste0("cytomapper_gate_", max(cur_num) + 1)]] <- cur_gate
        }
        
        rValues$object <- rValues$object()[,cur_selection$selected_]
    })
    
}

# Create the server
#' @importFrom DelayedArray rowRanges
#' @importFrom SummarizedExperiment assay
.cytomapper_server <- function(object, mask, image, cell_id, img_id,
                               input, output, session, ...)
{
    
    # Store image IDs and marker names
    img_IDs <- colData(object)[,img_id]
    markers <- rownames(object)
    
    updateSelectizeInput(session, 'sample',
                         choices = unique(img_IDs),
                         selected = unique(img_IDs)[1])
    updateSelectizeInput(session, 'assay',
                         choices = assayNames(object),
                         server = TRUE,
                         selected = assayNames(object)[1])
    updateSelectizeInput(session, 'reducedDim',
                         choices = reducedDimNames(object),
                         server = TRUE,
                         selected = reducedDimNames(object)[1])
    updateSelectizeInput(session, 'Marker_1',
                         choices = markers,
                         server = TRUE,
                         selected = markers[1])
    updateSelectizeInput(session, 'Marker_2',
                         choices = markers,
                         server = TRUE, selected = "")
    updateSelectizeInput(session, 'Marker_3',
                         choices = markers,
                         server = TRUE, selected = "")
    updateSelectizeInput(session, 'Marker_4',
                         choices = markers,
                         server = TRUE, selected = "")
    updateSelectizeInput(session, 'Marker_5',
                         choices = markers,
                         server = TRUE, selected = "")
    updateSelectizeInput(session, 'Marker_6',
                         choices = markers,
                         server = TRUE, selected = "")
    
    # Save some variables used throught the app
    rValues <- reactiveValues()
    
    observeEvent(input$sample, {
        rValues$object <- object[,colData(object)[,img_id] == input$sample]
    }, ignoreInit = TRUE)
    
    observeEvent(input$assay, {
        cur_ranges <- rowRanges(assay(object, input$assay))
        rownames(cur_ranges) <- rownames(object)
        rValues$ranges <- cur_ranges
    }, ignoreInit = TRUE)
    
    # Session info observer
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    # First scatter plot
    cur_marker1 <- reactive({input$Marker_1})
    cur_marker2 <- reactive({input$Marker_2})
    output$scatter1 <- .createScatter(input, rValues, 
                            markers = list(cur_marker1, cur_marker2))
    
    # Brushing 1
    cur_brush1 <- reactive({input$plot_brush1})
    .brushObject(input, rValues,
                 markers = list(cur_marker1, cur_marker2), 
                 brush = cur_brush1)
    
    #output$info1 <- renderText({
    #    paste0(
    #        "Selection: ", .brushRange(input$plot_brush1)
    #    )
    #})
    
    # Second scatter plot
    #observe({
    #    if (!is.null(input$plot_brush1) && input$Marker_3 != "") {
    #        output$scatter2 <- .createScatter(input, rValues, 
    #                                          markers = c(input$Marker_3, input$Marker_4))
    #            
    #        # Select SCE
    #        .brushObject(input, rValues,
    #                     markers = c(input$Marker_3, input$Marker_4), 
    #                     brush = input$plot_brush2)
    #            
    #        output$info2 <- renderText({
    #            paste0(
    #                "Selection: ", .brushRange(input$plot_brush2)
    #            )
    #        })
    #    }  
    #})
    
    # Third scatter plot
    #observe({
    #    if (!is.null(input$plot_brush2) && input$Marker_5 != "") {
    #        output$scatter3 <- .createScatter(input, rValues, 
    #                                          markers = c(input$Marker_5, input$Marker_6))
    #        
    #        # Select SCE
    #        .brushObject(input, rValues,
    #                     markers = c(input$Marker_5, input$Marker_6), 
    #                     brush = input$plot_brush3)
    #        
    #        output$info3 <- renderText({
    #            paste0(
    #                "Selection: ", .brushRange(input$plot_brush3)
    #            )
    #        })
    #    }  
    #})
    
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

