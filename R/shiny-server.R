# -----------------------------------------------------------------------------
# Helper functions to modify the server side of the shiny app
# -----------------------------------------------------------------------------

# Function to report gate bounds
.xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin = ", round(e$xmin, 2), " xmax = ", round(e$xmax, 2),
           "ymin = ", round(e$ymin, 2), " ymax = ", round(e$ymax, 2))
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
#' @importFrom DelayedArray
.cytomapper_server <- function(object, mask, image, cell_id, img_id,
                               input, output, session, ...)
{
    # Save some variables used throught the app
    img_IDs <- colData(object)[,img_id]
    markers <- rownames(object)
    cur_sample <- reactive({input$sample})
    cur_assay <- reactive({input$assay})
    cur_rd <- reactive({input$reducedDim})
    cur_ranges <- rowRanges(assay(object, cur_assay))
    rownames(cur_ranges) <- rownames(object)
    cur_object <- object[,colData(object)[,img_id] == cur_sample]
    
    updateSelectizeInput(session, 'sample',
                         choices = unique(sample_names),
                         selected = unique(sample_names)[1])
    updateSelectizeInput(session, 'assay',
                         choices = assayNames(object),
                         server = TRUE,
                         selected = "counts")
    updateSelectizeInput(session, 'reducedDim',
                         choices = reducedDimNames(object),
                         server = TRUE,
                         selected = "TSNE")
    updateSelectizeInput(session, 'Marker_1',
                         choices = markers,
                         server = TRUE)
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
    
    # Session info observer
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    # First scatter plot
    cur_marker1 <- reactive({input$Marker_1})
    cur_marker2 <- reactive({input$Marker_2})
    cur_object <- 
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
        }
    }
    
    createPlot.reducedDim_expression <- renderPlot({
        
        if (length(reducedDimNames(object)) == 0) {
            return(NULL)
        }
        
        # Read in selected values
        marker1 <- input$Marker_1
        marker2 <- input$Marker_2
        marker3 <- input$Marker_3
        marker4 <- input$Marker_4
        marker5 <- input$Marker_5
        marker6 <- input$Marker_6
        
        # Which assay to display
        cur_assay <- input$assay
        
        # Which samples to display
        sample <- input$sample
        
        # Which reducedDim to display
        cur_reducedDim <- input$reducedDim
        
        # Update sample names
        sample_names_update <- sample_names[sample_names %in% sample]
        
        # Select reducedDim
        cur_reducedDim <- as.data.frame(reducedDims(object)[[cur_reducedDim]][names(sample_names_update),])
        
        # Select expression
        marker1_dat <- assay(object, cur_assay)[marker1, sample_names %in% sample]
        if(marker2 != ""){
            marker2_dat <- assay(object, cur_assay)[marker2, sample_names %in% sample]
        } else {
            marker2_dat <- NA
        }
        if(marker3 != ""){
            marker3_dat <- assay(object, cur_assay)[marker3, sample_names %in% sample]
        } else {
            marker3_dat <- NA
        }
        if(marker4 != ""){
            marker4_dat <- assay(object, cur_assay)[marker4, sample_names %in% sample]
        } else {
            marker4_dat <- NA
        }
        if(marker5 != ""){
            marker5_dat <- assay(object, cur_assay)[marker5, sample_names %in% sample]
        } else {
            marker5_dat <- NA
        }
        if(marker6 != ""){
            marker6_dat <- assay(object, cur_assay)[marker6, sample_names %in% sample]
        } else {
            marker6_dat <- NA
        }
        
        cur_df <- data.frame(row.names = names(marker1_dat),
                             redDim1 = cur_reducedDim[,1],
                             redDim2 = cur_reducedDim[,2],
                             marker1 = marker1_dat,
                             marker2 = marker2_dat,
                             marker3 = marker3_dat,
                             marker4 = marker4_dat,
                             marker5 = marker5_dat,
                             marker6 = marker6_dat,
                             sample = sample_names_update)
        
        if(is.null(input$plot_brush1)){
            cur_selection <- cur_df
            cur_selection$markerA <- cur_selection$marker1
            cur_selection$markerB <- cur_selection$marker2
            cur_selection$nameA <- marker1
            cur_selection$nameB <- marker2
            cur_selection$selected_ <- FALSE
        }
        else if(!is.null(input$plot_brush1) & is.null(input$plot_brush2)){
            cur_selection <- brushedPoints(cur_df, input$plot_brush1, allRows = TRUE)
            cur_selection$markerA <- cur_selection$marker1
            cur_selection$markerB <- cur_selection$marker2
            cur_selection$nameA <- marker1
            cur_selection$nameB <- marker2
        }
        else if(!is.null(input$plot_brush2) & is.null(input$plot_brush3)){
            cur_selection1 <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
            cur_selection <- brushedPoints(cur_selection1, input$plot_brush2, allRows = TRUE)
            cur_selection$markerA <- cur_selection$marker3
            cur_selection$markerB <- cur_selection$marker4
            cur_selection$nameA <- marker3
            cur_selection$nameB <- marker4
        }
        else if(!is.null(input$plot_brush3)){
            cur_selection1 <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
            cur_selection2 <- brushedPoints(cur_selection1, input$plot_brush2, allRows = FALSE)
            cur_selection <- brushedPoints(cur_selection2, input$plot_brush3, allRows = TRUE)
            cur_selection$markerA <- cur_selection$marker5
            cur_selection$markerB <- cur_selection$marker6
            cur_selection$nameA <- marker5
            cur_selection$nameB <- marker6
        }
        
        if(sum(!is.na(cur_selection$markerB)) == 0){
            ggplot() +
                geom_point(data = cur_selection[!cur_selection$selected_,],
                           aes(redDim1, redDim2), size = 0.5, colour = "grey") +
                geom_point(data = cur_selection[cur_selection$selected_,],
                           aes(redDim1, redDim2, colour = markerA), size = 0.5) +
                xlab("redDim1") + ylab("redDim2") +
                scale_colour_viridis_c(name = unique(cur_selection$nameA ))
        }
        else{
            dat <- expand.grid(green=seq(0, 100, by=1), red=seq(0, 100, by=1))
            dat <- within(dat, mix <- rgb(green=green, red=red, blue=0, maxColorValue=100))
            dat$joined <- paste(dat$green, dat$red, sep = "_")
            
            marker.vec1 <- .scaling(cur_selection$markerA)
            marker.vec2 <- .scaling(cur_selection$markerB)
            
            col_final <- dat$mix[match(paste(round(marker.vec1*100, digits = 0),
                                             round(marker.vec2*100, digits = 0), sep = "_"), dat$joined)]
            
            cur_selection$col <- col_final
            
            ggplot() +
                geom_point(data = cur_selection[!cur_selection$selected_,],
                           aes(redDim1, redDim2), size = 0.5, colour = "grey") +
                geom_point(data = cur_selection[cur_selection$selected_,],
                           aes(redDim1, redDim2), colour = cur_selection[cur_selection$selected_,"col"], size = 0.5) +
                xlab("redDim1") + ylab("redDim2")
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
    
    datasetInput <- renderPlot({
        # Read in selected values
        marker1 <- input$Marker_1
        marker2 <- input$Marker_2
        marker3 <- input$Marker_3
        marker4 <- input$Marker_4
        marker5 <- input$Marker_5
        marker6 <- input$Marker_6
        
        # Which assay to display
        cur_assay <- input$assay
        
        # Which samples to display
        sample <- input$sample
        
        # Select object
        cur_object <- object[,sample_names %in% sample]
        
        # Update sample names
        sample_names_update <- sample_names[sample_names %in% sample]
        
        # Select expression
        marker1_dat <- assay(object, cur_assay)[marker1, sample_names %in% sample]
        if(marker2 != ""){
            marker2_dat <- assay(object, cur_assay)[marker2, sample_names %in% sample]
        } else {
            marker2_dat <- NA
        }
        if(marker3 != ""){
            marker3_dat <- assay(object, cur_assay)[marker3, sample_names %in% sample]
        } else {
            marker3_dat <- NA
        }
        if(marker4 != ""){
            marker4_dat <- assay(object, cur_assay)[marker4, sample_names %in% sample]
        } else {
            marker4_dat <- NA
        }
        if(marker5 != ""){
            marker5_dat <- assay(object, cur_assay)[marker5, sample_names %in% sample]
        } else {
            marker5_dat <- NA
        }
        if(marker6 != ""){
            marker6_dat <- assay(object, cur_assay)[marker6, sample_names %in% sample]
        } else {
            marker6_dat <- NA
        }
        
        cur_df <- data.frame(row.names = names(marker1_dat),
                             marker1 = marker1_dat,
                             marker2 = marker2_dat,
                             marker3 = marker3_dat,
                             marker4 = marker4_dat,
                             marker5 = marker5_dat,
                             marker6 = marker6_dat,
                             assay = cur_assay,
                             sample = sample,
                             cell_ID = colData(cur_object)[,cell_id])
        
        if(!is.null(input$plot_brush1) & is.null(input$plot_brush2)){
            cur_selection <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
            if(marker2 == ""){
                cur_selection[,marker1] <- cur_selection$marker1
            }
            else{
                cur_selection[,marker1] <- cur_selection$marker1
                cur_selection[,marker2] <- cur_selection$marker1
            }
            cur_selection$marker1 <- NULL
            cur_selection$marker2 <- NULL
            cur_selection$marker3 <- NULL
            cur_selection$marker4 <- NULL
            cur_selection$marker5 <- NULL
            cur_selection$marker6 <- NULL
        }
        else if(!is.null(input$plot_brush2) & is.null(input$plot_brush3)){
            cur_selection1 <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
            cur_selection <- brushedPoints(cur_selection1, input$plot_brush2, allRows = FALSE)
            if(marker2 == ""){
                cur_selection[,marker1] <- cur_selection$marker1
            }
            else{
                cur_selection[,marker1] <- cur_selection$marker1
                cur_selection[,marker2] <- cur_selection$marker1
            }
            if(marker4 == ""){
                cur_selection[,marker3] <- cur_selection$marker3
            }
            else{
                cur_selection[,marker3] <- cur_selection$marker3
                cur_selection[,marker4] <- cur_selection$marker4
            }
            cur_selection$marker1 <- NULL
            cur_selection$marker2 <- NULL
            cur_selection$marker3 <- NULL
            cur_selection$marker4 <- NULL
            cur_selection$marker5 <- NULL
            cur_selection$marker6 <- NULL
        }
        else if(!is.null(input$plot_brush3)){
            cur_selection1 <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
            cur_selection2 <- brushedPoints(cur_selection1, input$plot_brush2, allRows = FALSE)
            cur_selection <- brushedPoints(cur_selection2, input$plot_brush3, allRows = FALSE)
            if(marker2 == ""){
                cur_selection[,marker1] <- cur_selection$marker1
            }
            else{
                cur_selection[,marker1] <- cur_selection$marker1
                cur_selection[,marker2] <- cur_selection$marker1
            }
            if(marker4 == ""){
                cur_selection[,marker3] <- cur_selection$marker3
            }
            else{
                cur_selection[,marker3] <- cur_selection$marker3
                cur_selection[,marker4] <- cur_selection$marker4
            }
            if(marker6 == ""){
                cur_selection[,marker5] <- cur_selection$marker5
            }
            else{
                cur_selection[,marker5] <- cur_selection$marker5
                cur_selection[,marker6] <- cur_selection$marker6
            }
            cur_selection$marker1 <- NULL
            cur_selection$marker2 <- NULL
            cur_selection$marker3 <- NULL
            cur_selection$marker4 <- NULL
            cur_selection$marker5 <- NULL
            cur_selection$marker6 <- NULL
        }
        
        cur_selection
    })
    
    
    output$reducedDim_expression <- createPlot.reducedDim_expression
    
    
    output$image_selection <- createPlot.image_selection
    
    output$info1 <- renderText({
        paste0(
            "Selection: ", .xy_range_str(input$plot_brush1)
        )
    })
    
    output$info2 <- renderText({
        paste0(
            "Selection: ", .xy_range_str(input$plot_brush2)
        )
    })
    
    output$info3 <- renderText({
        paste0(
            "Selection: ", .xy_range_str(input$plot_brush3)
        )
    })
    
    output$downloadData <- downloadHandler(
        filename = "cell_type.csv",
        content = function(filename) {
            write.csv(datasetInput, filename, row.names = FALSE)
        }
    )
}

