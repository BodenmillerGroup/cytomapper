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
    if (is.null(brush)) {
        return(NULL) 
    }
    if (brush$direction == "xy") {
        paste0("xmin = ", round(brush$xmin, 1), " xmax = ", round(brush$xmax, 1),
               " ymin = ", round(brush$ymin, 1), " ymax = ", round(brush$ymax, 1))
    } else {
        paste0("xmin = ", round(brush$xmin, 1), " xmax = ", round(brush$xmax, 1))
    }
}

# Generate help text
.general_help <- function(){
    pre(
        h2("Using this Shiny app"),
        p("Use the sidebar of the app to define how many plots and what markers you want to use to define your cell population of interest. If the provided SCE object has multiple assays available you can also "),
        h2("Change the Assay"),
        p(""),
        h2("Visualization on the images"),
        p("The selected cells can be seen on the images in the `Image` Tab. Double-click either on the `Expression` or `Selection` image to zoom-in.")
    )
}

# Create general observers for header
#' @importFrom utils capture.output
.create_general_observer <- function(input, si){
    
    # Return session info
    observeEvent(input$SessionInfo, {
        showModal(modalDialog(
            pre(paste(capture.output(si), collapse = "\n")),
            size = "l",fade = TRUE,
            footer = NULL, easyClose = TRUE,
            title = "Session Info",
        ))
    })
    
    # Return helptext
    observeEvent(input$Help, {
        showModal(modalDialog(
            .general_help(),
            size = "l",fade = TRUE,
            footer = NULL, easyClose = TRUE,
            title = "Help",
        ))
    })
}

# Create interactive observers
#' @importFrom matrixStats rowRanges
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
    
}

# Create updateSelectizeInput objects
.create_updateSelectizeInput <- function(object, img_id, input, session){
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
    observeEvent(input$plotCount, {
        
        for (i in seq_len(input$plotCount)) {
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
.addPlots_sidebar <- function(input) {
    renderUI({
        lapply(seq_len(input$plotCount), function(cur_plot) {
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

.addPlots_tab1 <- function(input) {
    
    renderUI({
        cur_row <- ceiling(input$plotCount / 3)
        # Generate boxes
        box_list <- lapply(seq_len(input$plotCount), function(cur_plot) {
            
            cur_val <- (cur_plot * 2) - 1
            
            # Create brush options
            cur_brush_opts <- brushOpts(paste0("plot_brush", cur_plot),
                                        fill = .create_colours(cur_plot),
                                        stroke = .create_colours(cur_plot),
                                        direction = "xy",
                                        resetOnNew = FALSE)
            
            box(plotOutput(paste0("scatter", cur_plot), 
                           brush = cur_brush_opts),
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

.addPlots_tab2 <- function(input, object, mask, image) {
    
    if (is.null(mask) && is.null(image)) {
        return(NULL)
    } else if (!is.null(image)) {
        contrast_input_1 <- numericInput("contrast_marker_1",
                                label = span(paste("Contrast marker 1"), 
                                    style = "color: black; padding-top: 0px"),
                                value = 1)
        contrast_input_2 <- numericInput("contrast_marker_2",
                                 label = span(paste("Contrast marker 2"), 
                                    style = "color: black; padding-top: 0px"),
                                 value = 1)
    } else {
        contrast_input_1 <- contrast_input_2 <- NULL
    }
    
    markers <- rownames(object)
    
    renderUI({
        fluidRow(
            box(
                column(width = 6, 
                    selectizeInput("exprs_marker_1",
                        label = span(paste("Select marker 1"), 
                            style = "color: black; padding-top: 0px"), 
                        choices = c(markers, ""),
                        selected = "",
                        options = list(placeholder = '', maxItems = 1)),
                    contrast_input_1),
                column(width = 6, 
                    selectizeInput("exprs_marker_2",
                        label = span(paste("Select marker 2"), 
                            style = "color: black; padding-top: 0px"), 
                        choices = c(markers, ""),
                        selected = "",
                        options = list(placeholder = '', maxItems = 1)),
                    contrast_input_2),
                svgPanZoomOutput("image_expression", height = "300px"), 
                title = "Expression", status = "primary",
                width = 6, height = "550px"
                ),
            box(
                svgPanZoomOutput("image_selection"), 
                title = "Selection", status = "primary",
                width = 6, height = "550px")
            )
    })
}

# Function to allow brushing
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment metadata<-
.brushObject <- function(input, session, objValues, iter){
    
    cur_val <- (iter * 2) - 1
    
    # Build data frame 
    cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]], input$assay)))
    cur_df$sample <- input$sample
    
    # Brush the data.frame
    cur_selection <- brushedPoints(cur_df, input[[paste0("plot_brush", iter)]], allRows = TRUE)
    
    # Save the Gate
    cur_gate <- list()
    
    gate <- matrix(data = c(input[[paste0("plot_brush", iter)]]$xmin, 
                            input[[paste0("plot_brush", iter)]]$xmax, 
                            input[[paste0("plot_brush", iter)]]$ymin, 
                            input[[paste0("plot_brush", iter)]]$ymax),
                   nrow = 2, ncol = 2,
                   byrow = TRUE,
                   dimnames = list(c(input[[paste0("plot_brush", iter)]]$mapping$x, 
                                     input[[paste0("plot_brush", iter)]]$mapping$y), c("min", "max")))
    
    if (rownames(gate)[1] == "sample") {
        gate <- gate[-1,]
    }
    
    cur_gate$gate <- gate
    cur_gate$exprs_values <- input$assay
    cur_gate$img_id <- input$sample
    
    # Save gates
    next_obj <- objValues[[paste0("object", iter)]]
    metadata(next_obj)[[paste0("cytomapper_gate_", iter)]] <- cur_gate
    
    if (sum(cur_selection$selected_) > 0) {
        objValues[[paste0("object", iter + 1)]] <- next_obj[,cur_selection$selected_]
    } else {
        objValues[[paste0("object", iter + 1)]] <- NULL
    }
    
}

# Create scatter plots
#' @import ggplot2
#' @importFrom ggbeeswarm geom_quasirandom
.createScatter <- function(input, session, rValues, objValues, iter, img_id, cell_id){
    
    renderPlot({
        
        cur_val <- (iter * 2) - 1
        
        req(rValues$ranges, objValues[[paste0("object", iter)]], 
            input$assay, input[[paste0("Marker_", cur_val)]])
        
        if (iter > 1 && is.null(input[[paste0("plot_brush", iter - 1)]])) {
            return(NULL)
        }
        
        if (is.null(input[[paste0("plot_brush", iter)]])) {
            
            # Remove all objects higher than iter
            lapply(seq_len(length(reactiveValuesToList(objValues))), function(cur_obj){
                if (cur_obj > iter) {
                    objValues[[paste0("object", cur_obj)]] <- NULL
                    session$resetBrush(paste0("plot_brush", cur_obj))
                }
            })
            
        } else {
            .brushObject(input, session, objValues, iter = iter) 
        }
        
        # Build data frame for visualization
        cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]], 
                                        input$assay)))
        cur_df$sample <- input$sample
        
        if (input[[paste0("Marker_", cur_val + 1)]] != "") {
            
            # Scatter plot    
            p <- ggplot(cur_df) +
                geom_point(aes_(as.name(input[[paste0("Marker_", cur_val)]]), 
                                as.name(input[[paste0("Marker_", cur_val + 1)]])), 
                           show.legend = FALSE) +
                ylab(input[[paste0("Marker_", cur_val + 1)]]) +
                theme_minimal() + 
                ylim(c(rValues$ranges[input[[paste0("Marker_", cur_val + 1)]], 1], 
                       rValues$ranges[input[[paste0("Marker_", cur_val + 1)]], 2])) +
                xlim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1], 
                       rValues$ranges[input[[paste0("Marker_", cur_val)]], 2]))
            
            if (!is.null(objValues[[paste0("object", iter + 1)]])) {
                
                cur_df_1 <- as.data.frame(t(assay(objValues[[paste0("object", iter + 1)]], 
                                                  input$assay)))
                cur_df_1$sample <- input$sample
                
                p <- p + geom_point(aes_(as.name(input[[paste0("Marker_", cur_val)]]), 
                                         as.name(input[[paste0("Marker_", cur_val + 1)]])), 
                                    show.legend = FALSE, data = cur_df_1, colour = "red")
            }
            
            return(p)
            
        } else {
            
            p <- ggplot(cur_df) +
                geom_quasirandom(aes_(x = quote(sample),
                                      y = as.name(input[[paste0("Marker_", cur_val)]])), 
                                 show.legend = FALSE) + 
                theme(axis.text.x = element_blank(),
                      panel.background = element_blank()) +
                ylim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1], 
                       rValues$ranges[input[[paste0("Marker_", cur_val)]], 2])) 
            
            if (!is.null(objValues[[paste0("object", iter + 1)]])) {
                
                cur_df$selected <- colData(objValues[[paste0("object", iter)]])[,cell_id] %in%
                    colData(objValues[[paste0("object", iter + 1)]])[,cell_id]
                
                p <- p +
                    geom_quasirandom(aes_(x = quote(sample),
                                          y = as.name(input[[paste0("Marker_", cur_val)]]),
                                          colour = quote(selected)), 
                                     show.legend = FALSE, data = cur_df) + 
                    scale_colour_manual(values = c(`FALSE` = "black",
                                                   `TRUE` = "red"))
                
            }
            
            return(p)
            
        }
    })
}

# Helper function to select markers
.select_markers <- function(input){
    if (input$exprs_marker_1 != "") {
        if (input$exprs_marker_2 != "") {
            cur_markers <- c(input$exprs_marker_1, input$exprs_marker_2)
        } 
        else{
            cur_markers <- input$exprs_marker_1
        }
    } else {
        req(input$Marker_1)
        
        cur_markers <- reactiveValuesToList(input)
        cur_markers <- cur_markers[grepl("Marker_", names(cur_markers))]
        cur_markers <- cur_markers[unlist(lapply(cur_markers, function(x){x != ""}))]
        
        if (length(cur_markers) %% 2 == 0) {
            cur_markers <- c(cur_markers[[paste0("Marker_", length(cur_markers) - 1)]],
                             cur_markers[[paste0("Marker_", length(cur_markers))]])
        } else {
            cur_markers <- cur_markers[[paste0("Marker_", length(cur_markers))]]
        }
    }
    return(cur_markers)
}

# Helper function to define bcg parameter when using plotPixels()
.select_contrast <- function(input){
    cur_markers <- .select_markers(input)
    
    if (length(cur_markers) == 1) {
        cur_bcg <- list(c(0, input$contrast_marker_1, 1))
        names(cur_bcg) <- cur_markers
    } else {
        cur_bcg <- list(c(0, input$contrast_marker_1, 1), 
                        c(0, input$contrast_marker_2, 1))
        names(cur_bcg) <- cur_markers
    }
    
    return(cur_bcg)
}

# Visualize marker expression on images
#' @importFrom svgPanZoom svgPanZoom renderSvgPanZoom
#' @importFrom svglite stringSVG
.createImageExpression <- function(input, object, mask, image, img_id, cell_id, ...){
    renderSvgPanZoom({
        
        cur_markers <- .select_markers(input)
        cur_bcg <- .select_contrast(input)
        
        if (is.null(image)) {
            cur_mask <- mask[mcols(mask)[,img_id] == input$sample]
            suppressMessages(
                svgPanZoom(zoomScaleSensitivity = 0.4, stringSVG(
                    plotCells(object = object,
                            mask = cur_mask,
                            cell_id = cell_id,
                            img_id = img_id,
                            colour_by = cur_markers,
                            exprs_values = input$assay,
                            ...)))
            )
        } else {
            cur_image <- image[mcols(image)[,img_id] == input$sample]
            suppressMessages(
                svgPanZoom(zoomScaleSensitivity = 0.4, stringSVG(
                    plotPixels(image = cur_image,
                            colour_by = cur_markers,
                            bcg = cur_bcg, 
                            ...)))
            )
        }
    })
}

# Visualize selected cells on images
#' @importFrom svgPanZoom svgPanZoom renderSvgPanZoom
#' @importFrom svglite stringSVG
#' @importFrom S4Vectors metadata
.createImageSelection <- function(input, objValues, mask, image, img_id, cell_id, ...){
    renderSvgPanZoom({
        
        cur_val <- (input$plotCount * 2) - 1
        
        req(objValues$object2)
        
        cur_markers <- .select_markers(input)
        cur_bcg <- .select_contrast(input)
        
        cur_object <- reactiveValuesToList(objValues)
        cur_object <- cur_object[!unlist(lapply(cur_object, is.null))]
        cur_object <- cur_object[[paste0("object", length(cur_object))]]
        
        cur_object$selected <- TRUE
    
        if (is.null(image)) {
            cur_mask <- mask[mcols(mask)[,img_id] == input$sample] 
            suppressMessages(
                svgPanZoom(zoomScaleSensitivity = 0.4, stringSVG(
                    plotCells(object = cur_object,
                              mask = cur_mask,
                              cell_id = cell_id,
                              img_id = img_id,
                              colour_by = "selected",
                              colour = list(selected = c("TRUE" = "dark red", "FALSE" = "gray")),
                              legend = NULL,
                              ...)))
            )
            
        } 
        else {
            cur_mask <- mask[mcols(mask)[,img_id] == input$sample]
            cur_image <- image[mcols(image)[,img_id] == input$sample]
            suppressMessages(
                svgPanZoom(zoomScaleSensitivity = 0.4, stringSVG(
                    plotPixels(image = cur_image,
                               object = cur_object,
                               mask = cur_mask,
                               cell_id = cell_id,
                               img_id = img_id,
                               colour_by = cur_markers,
                               outline_by = "selected",
                               colour = list(selected = c("TRUE" = "white", "FALSE" = "gray")),
                               legend = NULL, 
                               bcg = cur_bcg,
                               ...)))
            )
        }
    })
}


# Download the selected data
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment metadata<-
#' @importFrom utils sessionInfo
.downloadSelection <- function(input, objValues){
    downloadHandler(
        filename = function(){
            paste0(input$labelCellsBy, ".rds")
        },
        content = function(file){
            cur_object <- reactiveValuesToList(objValues)
            cur_object <- cur_object[!unlist(lapply(cur_object, is.null))]
            cur_object <- cur_object[[paste0("object", length(cur_object))]]
            
            cur_object$CellLabel <- input$labelCellsBy
            
            # Add session info
            metadata(cur_object)$SessionInfo <- sessionInfo()
            
            # Add date
            metadata(cur_object)$GatingDate <- Sys.Date()
            
            saveRDS(cur_object, file)
        }
    )
}