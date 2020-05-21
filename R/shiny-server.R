# -----------------------------------------------------------------------------
# Helper functions to modify the server side of the shiny app
# -----------------------------------------------------------------------------

# Function to report gate bounds
.xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
           " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
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

# Create the server
.cytomapper_server <- function(object, mask, image, cell_id, img_id,
                               input, output, session, ...)
{
    sample_names <- colData(object)[,img_id]
    names(sample_names) <- colnames(object)
    
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
                         choices = rownames(object),
                         server = TRUE)
    updateSelectizeInput(session, 'Marker_2',
                         choices = rownames(object),
                         server = TRUE, selected = "")
    updateSelectizeInput(session, 'Marker_3',
                         choices = rownames(object),
                         server = TRUE, selected = "")
    updateSelectizeInput(session, 'Marker_4',
                         choices = rownames(object),
                         server = TRUE, selected = "")
    updateSelectizeInput(session, 'Marker_5',
                         choices = rownames(object),
                         server = TRUE, selected = "")
    updateSelectizeInput(session, 'Marker_6',
                         choices = rownames(object),
                         server = TRUE, selected = "")
    
    # Session info observer
    cur_sessionInfo <- sessionInfo()
    .create_general_observer(input, si = cur_sessionInfo)
    
    #### Visualize marker expression
    createPlot.scatter1 <- renderPlot({
        
        # Read in selected values
        marker1 <- input$Marker_1
        marker2 <- input$Marker_2
        
        # Which samples to display
        sample <- input$sample
        
        # Which assay to display
        cur_assay <- input$assay
        
        # Update sample names
        sample_names_update <- sample_names[sample_names %in% sample]
        
        if(marker2 == ""){
            marker1_dat <- assay(object, cur_assay)[marker1, sample_names %in% sample]
            
            # Distributions of marker proteins
            ggplot(data.frame(row.names = names(marker1_dat),
                              marker1 = marker1_dat,
                              sample = sample_names_update)) +
                geom_density_ridges2(aes(x = marker1, y = sample,
                                         fill = sample), show.legend = FALSE) +
                theme(axis.text.y = element_blank(),
                      panel.background = element_blank()) +
                ylab("") +
                xlim(c(min(assay(object, cur_assay)[marker1,]),max(assay(object, cur_assay)[marker1,])))
        }
        else{
            marker1_dat <- assay(object, cur_assay)[marker1, sample_names %in% sample]
            marker2_dat <- assay(object, cur_assay)[marker2, sample_names %in% sample]
            
            ggplot(data.frame(row.names = names(marker1_dat),
                              marker1 = marker1_dat,
                              marker2 = marker2_dat,
                              sample = sample_names_update)) +
                geom_point(aes(marker1, marker2, colour = sample), show.legend = FALSE) +
                ylab(marker2) +
                theme_minimal() + xlab(marker1) +
                ylim(c(min(assay(object, cur_assay)[marker2,]),max(assay(object, cur_assay)[marker2,]))) +
                xlim(c(min(assay(object, cur_assay)[marker1,]),max(assay(object, cur_assay)[marker1,])))
            
        }
    })
    
    #### Visualize second expression
    createPlot.scatter2 <- renderPlot({
        if(!is.null(input$plot_brush1)){
            # Read in selected values
            marker1 <- input$Marker_1
            marker2 <- input$Marker_2
            marker3 <- input$Marker_3
            marker4 <- input$Marker_4
            
            # Which samples to display
            sample <- input$sample
            
            # Which assay to display
            cur_assay <- input$assay
            
            # Update sample names
            sample_names_update <- sample_names[sample_names %in% sample]
            
            if(marker3 != "" & marker4 == ""){
                marker1_dat <- assay(object, cur_assay)[marker1, sample_names %in% sample]
                if(marker2 != ""){
                    marker2_dat <- assay(object, cur_assay)[marker2, sample_names %in% sample]
                } else {
                    marker2_dat <- NA
                }
                marker3_dat <- assay(object, cur_assay)[marker3, sample_names %in% sample]
                
                # selection
                cur_df <- data.frame(row.names = names(marker1_dat),
                                     marker1 = marker1_dat,
                                     marker2 = marker2_dat,
                                     marker3 = marker3_dat,
                                     sample = sample_names_update)
                cur_selection <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
                
                # Distributions of marker proteins
                ggplot(cur_selection) +
                    geom_density_ridges2(aes(x = marker3, y = sample,
                                             fill = sample), show.legend = FALSE) +
                    theme(axis.text.y = element_blank(),
                          panel.background = element_blank()) +
                    ylab("") +
                    xlim(c(min(assay(object, cur_assay)[marker3,]),max(assay(object, cur_assay)[marker3,])))
            }
            else if(marker3 != "" & marker4 != ""){
                marker1_dat <- assay(object, cur_assay)[marker1, sample_names %in% sample]
                if(marker2 != ""){
                    marker2_dat <- assay(object, cur_assay)[marker2, sample_names %in% sample]
                } else {
                    marker2_dat <- NA
                }
                marker3_dat <- assay(object, cur_assay)[marker3, sample_names %in% sample]
                marker4_dat <- assay(object, cur_assay)[marker4, sample_names %in% sample]
                
                # selection
                cur_df <- data.frame(row.names = names(marker1_dat),
                                     marker1 = marker1_dat,
                                     marker2 = marker2_dat,
                                     marker3 = marker3_dat,
                                     marker4 = marker4_dat,
                                     sample = sample_names_update)
                cur_selection <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
                
                ggplot(cur_selection) +
                    geom_point(aes(marker3, marker4, colour = sample), show.legend = FALSE) +
                    ylab(marker4) +
                    theme_minimal() + xlab(marker3) +
                    ylim(c(min(assay(object, cur_assay)[marker4,]),max(assay(object, cur_assay)[marker4,]))) +
                    xlim(c(min(assay(object, cur_assay)[marker3,]),max(assay(object, cur_assay)[marker3,])))
                
            }
        }
        
    })
    
    #### Visualize second expression
    createPlot.scatter3 <- renderPlot({
        if(!is.null(input$plot_brush1) & !is.null(input$plot_brush2)){
            # Read in selected values
            marker1 <- input$Marker_1
            marker2 <- input$Marker_2
            marker3 <- input$Marker_3
            marker4 <- input$Marker_4
            marker5 <- input$Marker_5
            marker6 <- input$Marker_6
            
            # Which samples to display
            sample <- input$sample
            
            # Which assay to display
            cur_assay <- input$assay
            
            # Update sample names
            sample_names_update <- sample_names[sample_names %in% sample]
            
            if(marker5 != "" & marker6 == ""){
                marker1_dat <- assay(object, cur_assay)[marker1, sample_names %in% sample]
                if(marker2 != ""){
                    marker2_dat <- assay(object, cur_assay)[marker2, sample_names %in% sample]
                } else {
                    marker2_dat <- NA
                }
                marker3_dat <- assay(object, cur_assay)[marker3, sample_names %in% sample]
                if(marker4 != ""){
                    marker4_dat <- assay(object, cur_assay)[marker4, sample_names %in% sample]
                } else {
                    marker4_dat <- NA
                }
                marker5_dat <- assay(object, cur_assay)[marker5, sample_names %in% sample]
                
                # selection
                cur_df <- data.frame(row.names = names(marker3_dat),
                                     marker1 = marker1_dat,
                                     marker2 = marker2_dat,
                                     marker3 = marker3_dat,
                                     marker4 = marker4_dat,
                                     marker5 = marker5_dat,
                                     sample = sample_names_update)
                cur_selection1 <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
                cur_selection2 <- brushedPoints(cur_selection1, input$plot_brush2, allRows = FALSE)
                
                # Distributions of marker proteins
                ggplot(cur_selection2) +
                    geom_density_ridges2(aes(x = marker5, y = sample,
                                             fill = sample), show.legend = FALSE) +
                    theme(axis.text.y = element_blank(),
                          panel.background = element_blank()) +
                    ylab("")  +
                    xlim(c(min(assay(object, cur_assay)[marker5,]),max(assay(object, cur_assay)[marker5,])))
            }
            else if(marker5 != "" & marker6 != ""){
                marker1_dat <- assay(object, cur_assay)[marker1, sample_names %in% sample]
                if(marker2 != ""){
                    marker2_dat <- assay(object, cur_assay)[marker2, sample_names %in% sample]
                } else {
                    marker2_dat <- NA
                }
                marker3_dat <- assay(object, cur_assay)[marker3, sample_names %in% sample]
                if(marker4 != ""){
                    marker4_dat <- assay(object, cur_assay)[marker4, sample_names %in% sample]
                } else {
                    marker4_dat <- NA
                }
                marker5_dat <- assay(object, cur_assay)[marker5, sample_names %in% sample]
                marker6_dat <- assay(object, cur_assay)[marker6, sample_names %in% sample]
                
                # selection
                cur_df <- data.frame(row.names = names(marker3_dat),
                                     marker1 = marker1_dat,
                                     marker2 = marker2_dat,
                                     marker3 = marker3_dat,
                                     marker4 = marker4_dat,
                                     marker5 = marker5_dat,
                                     marker6 = marker6_dat,
                                     sample = sample_names_update)
                cur_selection1 <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
                cur_selection2 <- brushedPoints(cur_selection1, input$plot_brush2, allRows = FALSE)
                
                ggplot(cur_selection2) +
                    geom_point(aes(marker5, marker6, colour = sample), show.legend = FALSE) +
                    ylab(marker6) +
                    theme_minimal() + xlab(marker5) +
                    ylim(c(min(assay(object, cur_assay)[marker6,]),max(assay(object, cur_assay)[marker6,]))) +
                    xlim(c(min(assay(object, cur_assay)[marker5,]),max(assay(object, cur_assay)[marker5,])))
                
            }
        }
        
    })
    
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
    
    createPlot.image_expression <- renderPlot({
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
        
        # Select the correct markers to display
        if(marker3 == "" | (marker3 != "" & is.null(input$plot_brush1))){
            markerA <- marker1
            if(marker2 != ""){
                markerB <- marker2
            } else {
                markerB <- NA
            }
        } else if(marker5 == "" | (marker5 != "" & is.null(input$plot_brush2))){
            markerA <- marker3
            if(marker4 != ""){
                markerB <- marker4
            } else {
                markerB <- NA
            }
        } else if(marker5 != "" & !is.null(input$plot_brush2)){
            markerA <- marker5
            if(marker6 != ""){
                markerB <- marker6
            } else {
                markerB <- NA
            }
        }
        
        if(is.na(markerB)){
            
            if(is.null(image)){
                cur_mask <- mask[mcols(mask)[,img_id] == sample]
                plotCells(object = cur_object,
                          mask = cur_mask,
                          cell_id = cell_id,
                          img_id = img_id,
                          colour_by = markerA,
                          exprs_values = cur_assay)
            } else {
                cur_mask <- mask[mcols(mask)[,img_id] == sample]
                cur_image <- image[mcols(image)[,img_id] == sample]
                plotPixels(image = cur_image,
                           object = cur_object,
                           mask = cur_mask,
                           cell_id = cell_id,
                           img_id = img_id,
                           colour_by = markerA)
            }
            
        }
        else{
            if(is.null(image)){
                cur_mask <- mask[mcols(mask)[,img_id] == sample]
                plotCells(object = cur_object,
                          mask = cur_mask,
                          cell_id = cell_id,
                          img_id = img_id,
                          colour_by = c(markerA, markerB),
                          exprs_values = cur_assay)
            } else {
                cur_mask <- mask[mcols(mask)[,img_id] == sample]
                cur_image <- image[mcols(image)[,img_id] == sample]
                plotPixels(image = cur_image,
                           object = cur_object,
                           mask = cur_mask,
                           cell_id = cell_id,
                           img_id = img_id,
                           colour_by = c(markerA, markerB))
            }
        }
        
    })
    
    createPlot.image_selection <- renderPlot({
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
                             sample = sample_names_update,
                             cell_ID = colData(cur_object)[,cell_id])
        
        if(is.null(input$plot_brush1)){
            cur_selection <- cur_df
            cur_selection$selected_ <- FALSE
        }
        else if(!is.null(input$plot_brush1) & is.null(input$plot_brush2)){
            cur_selection <- brushedPoints(cur_df, input$plot_brush1, allRows = TRUE)
        }
        else if(!is.null(input$plot_brush2) & is.null(input$plot_brush3)){
            cur_selection1 <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
            cur_selection <- brushedPoints(cur_selection1, input$plot_brush2, allRows = TRUE)
        }
        else if(!is.null(input$plot_brush3)){
            cur_selection1 <- brushedPoints(cur_df, input$plot_brush1, allRows = FALSE)
            cur_selection2 <- brushedPoints(cur_selection1, input$plot_brush2, allRows = FALSE)
            cur_selection <- brushedPoints(cur_selection2, input$plot_brush3, allRows = TRUE)
        }
        
        cur_object$selected <- FALSE
        colData(cur_object)[colData(cur_object)[,cell_id] %in%
                                cur_selection$cell_ID,"selected"] <- as.character(cur_selection$selected_)
        
        if(is.null(image)){
            cur_mask <- mask[mcols(mask)[,img_id] == sample] 
            plotCells(object = cur_object,
                      mask = cur_mask,
                      cell_id = cell_id,
                      img_id = img_id,
                      colour_by = "selected",
                      colour = list(selected = c("TRUE" = "dark red", "FALSE" = "gray")))
        } else {
            cur_mask <- mask[mcols(mask)[,img_id] == sample]
            plotCells(object = cur_object,
                      mask = cur_mask,
                      cell_id = cell_id,
                      img_id = img_id,
                      colour_by = "selected",
                      colour = list(selected = c("TRUE" = "dark red", "FALSE" = "gray")))
        }
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
    
    output$scatter1 <- createPlot.scatter1
    
    output$scatter2 <- createPlot.scatter2
    
    output$scatter3 <- createPlot.scatter3
    
    output$reducedDim_expression <- createPlot.reducedDim_expression
    
    output$image_expression <- createPlot.image_expression
    
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
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
}

