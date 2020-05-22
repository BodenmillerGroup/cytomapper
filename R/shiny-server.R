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
.createScatter <- function(object, markers, sample, assay, img_id, ranges){
    renderPlot({
        
        # Build data frame for visualization
        cur_df <- assay(object, assay)[,colData(object)[,img_id] == sample]
        cur_df$sample <- sample
        
        if(markers[2] == ""){

            # Distributions of marker proteins
            ggplot(cur_df) +
                geom_density(aes_(x = as.name(markers[1])), 
                             show.legend = FALSE) +
                theme(axis.text.y = element_blank(),
                      panel.background = element_blank()) +
                ylab("") +
                xlim(c(ranges[markers[1],1], ranges[markers[1],2]))
        }
        else{
            ggplot(cur_df) +
                geom_point(aes_(as.name(markers[1]), as.name(markers[2])), 
                           show.legend = FALSE) +
                ylab(markers[2]) +
                theme_minimal() + xlab(markers[1]) +
                ylim(c(ranges[markers[2],1], ranges[markers[2],2])) +
                xlim(c(ranges[markers[1],1], ranges[markers[1],2]))
            
        }
    })
}

.brushObject <- function(object, markers, sample, assay, 
                         img_id, brush){
    # Generate the data.frame
    cur_df <- assay(object, assay)[,colData(object)[,img_id] == sample]
    cur_df$sample <- sample
    
    # Brush the data.frame
    cur_selection <- brushedPoints(cur_df, brush, allRows = TRUE)
    
    # Save the Gate
    cur_gate <- list()
    cur_gate$gate <- matrix(data = c(brush$xmin, brush$xmax, 
                                brush$ymin, brush$ymax),
                       nrow = 2, ncol = 2,
                       byrow = TRUE,
                       dimnames = list(markers, c("min", "max")))
    cur_gate$exprs_values <- assay
    
    # Saved gates
    all_gates <- names(metadata(object))[grepl("cytomapper_gate", names(metadata(object)))]
    if (is.null(all_gates)) {
        metadata(object)$cytomapper_gate_1 <- cur_gate
    } else {
        cur_num <- as.numeric(vapply(all_gates, FUN = function(x){unlist(strsplit(x, split = "_"))[3]}, 
                                     FUN.VALUE = character(1)))
        metadata(object)[[paste0("cytomapper_gate_", max(cur_num) + 1)]] <- cur_gate
    }
    
    return(object[,cur_selection$selected_])
    
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
    cur_sample <- reactive({input$sample})
    cur_assay <- reactive({input$assay})
    cur_rd <- reactive({input$reducedDim})
    cur_ranges <- reactive({
        cur_r <- rowRanges(assay(object, cur_assay()))
        rownames(cur_r) <- rownames(object)
        cur_r
        })
    cur_object <- reactive({object[,colData(object)[,img_id] == cur_sample()]})
    
    
    # Session info observer
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    # First scatter plot
    cur_marker1 <- reactive({input$Marker_1})
    cur_marker2 <- reactive({input$Marker_2})
    output$scatter1 <- .createScatter(object = cur_object, 
                                     markers = c(cur_marker1, cur_marker2), 
                                     sample = sample,
                                     assay = assay,
                                     img_id = img_id,
                                     ranges = cur_ranges[c(c(cur_marker1, cur_marker2)),])
    
    # Select SCE
    cur_brush <- reactive({input$plot_brush1})
    cur_object <- .brushObject(object = cur_object, 
                               markers = c(cur_marker1, cur_marker2), 
                               sample = sample,
                               assay = assay,
                               img_id = img_id,
                               brush = cur_brush)
    
    output$info1 <- renderText({
        paste0(
            "Selection: ", .brushRange(input$plot_brush1)
        )
    })
    
    # Second scatter plot
    if (!is.null(metadata(cur_object)$cytomapper_gate1)) {
        if (input$Marker_3 != "") {
            cur_marker1 <- reactive({input$Marker_3})
            cur_marker2 <- reactive({input$Marker_4})
            output$scatter1 <- .createScatter(object = cur_object, 
                                        markers = c(cur_marker1, cur_marker2), 
                                        sample = sample,
                                        assay = assay,
                                        img_id = img_id,
                                        ranges = cur_ranges[c(c(cur_marker1, cur_marker2)),])
    
            # Select SCE
            cur_brush <- reactive({input$plot_brush2})
            cur_object <- .brushObject(object = cur_object, 
                                markers = c(cur_marker1, cur_marker2), 
                                sample = sample,
                                assay = assay,
                                img_id = img_id,
                                brush = cur_brush)
            
            output$info2 <- renderText({
                paste0(
                    "Selection: ", .brushRange(input$plot_brush2)
                )
            })
        }
    }
    
    # Third scatter plot
    if (!is.null(metadata(cur_object)$cytomapper_gate2)) {
        if (input$Marker_5 != "") {
            cur_marker1 <- reactive({input$Marker_5})
            cur_marker2 <- reactive({input$Marker_6})
            output$scatter1 <- .createScatter(object = cur_object, 
                                          markers = c(cur_marker1, cur_marker2), 
                                          sample = sample,
                                          assay = assay,
                                          img_id = img_id,
                                          ranges = cur_ranges[c(c(cur_marker1, cur_marker2)),])
        
            # Select SCE
            cur_brush <- reactive({input$plot_brush3})
            cur_object <- .brushObject(object = cur_object, 
                                   markers = c(cur_marker1, cur_marker2), 
                                   sample = sample,
                                   assay = assay,
                                   img_id = img_id,
                                   brush = cur_brush)
            
            output$info3 <- renderText({
                paste0(
                    "Selection: ", .brushRange(input$plot_brush3)
                )
            })
        }
    }
    
    
    # Reduced dimensions plot
    output$reducedDim_expression <- renderPlot({
        
        if (!is.null(reducedDim(object, cur_rd))) {
            sample_object <- object[,colData(object)[,img_id] == cur_sample]
            sample_df <- reducedDim(object, cur_rd)
            colnames(sample_df) <- c(paste(cur_rd, "1"), paste(cur_rd, "2"))
            cur_object_df <- reducedDim(cur_object, cur_rd)
            colnames(cur_object_df) <- c(paste(cur_rd, "1"), paste(cur_rd, "2"))
            
            ggplot() +
                geom_point(aes_(as.name(paste(cur_rd, "1")),
                                paste(cur_rd, "2")), data = sample_df,
                           colour = "gray") +
                geom_point(aes_(as.name(paste(cur_rd, "1")),
                                paste(cur_rd, "2")), data = cur_object_df,
                           colour = "dark red") 
        } else {
            return(NULL)
        }
    })
    
    output$image_expression <- renderPlot({
        
        if (cur_marker2 == "") {
            cur_markers <- cur_marker1
        } else {
            cur_markers <- c(cur_marker1, cur_marker2)
        }
        
        if (is.null(image)) {
            cur_mask <- mask[mcols(mask)[,img_id] == cur_sample]
            plotCells(object = cur_object,
                      mask = cur_mask,
                      cell_id = cell_id,
                      img_id = img_id,
                      colour_by = cur_markers,
                      exprs_values = cur_assay,
                      ...)
        } else {
            cur_image <- image[mcols(image)[,img_id] == sample]
            plotPixels(image = cur_image,
                       colour_by = cur_markers,
                       ...)
        }
        
    })
    
    output$image_selection <- renderPlot({
        cur_object$selected <- TRUE
        if(is.null(image)){
            cur_mask <- mask[mcols(mask)[,img_id] == sample] 
            plotCells(object = cur_object,
                      mask = cur_mask,
                      cell_id = cell_id,
                      img_id = img_id,
                      colour_by = "selected",
                      colour = list(selected = c("TRUE" = "dark red", "FALSE" = "gray")),
                      ...)
        } else {
            cur_mask <- mask[mcols(mask)[,img_id] == sample]
            cur_image <- image[mcols(image)[,img_id] == sample]
            plotPixels(image = cur_image,
                      object = cur_object,
                      mask = cur_mask,
                      cell_id = cell_id,
                      img_id = img_id,
                      colour_by = "selected",
                      colour = list(selected = c("TRUE" = "dark red", "FALSE" = "gray")),
                      ...)
        }
        cur_object$selected <- NULL
    })

    output$downloadData <- downloadHandler(
        filename = "gated_sce.rds",
        content = function(filename) {
            saveRDS(cur_object, filename)
        }
    )
}

