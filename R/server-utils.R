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
    
    paste0("xmin = ", round(brush$xmin, 1), " xmax = ", round(brush$xmax, 1),
            " ymin = ", round(brush$ymin, 1), " ymax = ", round(brush$ymax, 1))
}

# Helper function to clear objects
.clearObjects <- function(objValues, iter){
    lapply(seq_len(length(reactiveValuesToList(objValues))), function(cur_obj){
        if (cur_obj > iter) {
            objValues[[paste0("object", cur_obj)]] <- NULL
        }
    })
}

# Helper function to reset brush
.clearBrush <- function(input, session, iter){

    cur_brushs <- reactiveValuesToList(input)
    cur_brushs <- cur_brushs[grepl("plot_brush", names(cur_brushs))]

    lapply(seq_len(length(cur_brushs)), function(cur_obj){
        if (cur_obj > iter) {
            session$resetBrush(paste0("plot_brush", cur_obj))
        }
    })
}

# Generate help text
.general_help <- function(){
    tagList(
        h3("Using the Shiny application"),
        p("This help page provides a recommended workflow on how to most ",
        "efficiently use the app. The workflow is solely a recommendation - ",
        "the app provides full flexibility to change settings during each ",
        "step. To see the full documentation, please refer to the help page ",
        "found at", em("?cytomapperShiny")),
        h3("1. Select the number of plots"),
        p("The slider under ", em("General controls"), 
        " can be used to specify ",
        "the number of plots on which to perform gating. Up to two markers ",
        "can be visualized per plot."),
        h3("2. Select the sample"),
        p("The ", em("assay"), " dropdown selection under ", 
        em("General controls"), " allows the user to specify on",
        "which assay entry to perform gating. In most cases, a log- or ",
        "arcsinh-transformation can help to distinguish between 'positive' ",
        "and 'negative' populations."),
        h3("3. Select the markers"),
        p("For each plot, up to two markers can be specified. If selecting ",
        "a single marker, please specify this marker in the first of the ",
        "two dropdown menus. A violin plot is used to visualize the ",
        "expression of a single marker while a scatter plot is used to ",
        "visualize the expression of two markers."),
        h3("4. Gate cells"),
        p("When selecting cells in one plot, only those cells are visualized ",
        "on the following plot. Once markers, the assay or the number of ",
        "plots are changed, gates are cleared."),
        h3("5. Observe the selected cells"),
        p("After gating, the selected cells are visualized on the ",
        "corresponding images by switching to the ",
        em("Images"), " tab. By default, the first marker is selected. ",
        "The user can change the displayed marker or press reset marker ",
        "to switch to the markers used for gating. If a multi-channel ",
        "image object is provided, the contrast of the image can be ",
        "changed. The right panel visualizes the selected cells either ",
        "by filling in the segmentation masks or by outlining the ",
        "cells on the images."),
        h3("6. Change samples"),
        p("Samples can now be iteratively changed using the dropdown ",
        "menu under ", em("General controls"), ". The gates will remain ",
        "on the plots and can be adjusted for each sample."),
        h3("7. Save the selected cells"),
        p("Finally, the selected cells can be saved by clicking the download ",
        "button next to the '?' symbol. The selected cells will be stored ",
        "as a ", em("SingleCellExperiment"), " object in .rds format.",
        "Per selection, the user can provide a ", em("Cell label"), 
        " that will be stored in the ", em("colData"), " under the ", 
        em("cytomapper_CellLabel"), " entry of the downloaded object.")
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
.create_interactive_observer <- function(object, img_id, input, session,
                                            rValues, objValues){

    # Next Image Observer
    observeEvent(input$next.sample, {
        img_IDs <- unique(colData(object)[,img_id])
        cur_index <- match(input$sample, img_IDs)
        updated_index <- ifelse(cur_index == length(img_IDs), 1, cur_index + 1)
    
        # return updated img_id 
        updated_sample <- img_IDs[updated_index]
    
        updateSelectInput(session, inputId = "sample",
                        choices = unique(img_IDs),
                        selected = updated_sample)
    
        }, ignoreInit = TRUE)    

    # Previous Image Observer
    observeEvent(input$previous.sample, {
        img_IDs <- unique(colData(object)[,img_id])
        cur_index <- match(input$sample, img_IDs)
        updated_index <- ifelse(cur_index == 1,  length(img_IDs), cur_index - 1)
    
        # return updated img_id
        updated_sample <- img_IDs[updated_index]
    
        updateSelectInput(session, inputId = "sample",
                        choices = unique(img_IDs),
                        selected = updated_sample)

    }, ignoreInit = TRUE)    

    # Select first object
    observeEvent(input$sample, {
        objValues$object1 <- object[,colData(object)[,img_id] == input$sample]

        updateTabsetPanel(session, "tabbox1",
                        selected = "tab1")

    }, ignoreInit = TRUE)

    observeEvent(input$assay, {
        # Save ranges
        cur_ranges <- rowRanges(assay(object, input$assay))
        rownames(cur_ranges) <- rownames(object)
        rValues$ranges <- cur_ranges

        # Reset gates and objects
        .clearObjects(objValues, iter = 1)
        .clearBrush(input, session, iter = 1)

    }, ignoreInit = TRUE)

    # Plot change observer - change tab if number of plots ar changed
    observeEvent(input$plotCount, {

        updateTabsetPanel(session, "tabbox1",
                            selected = "tab1")

        # Reset gates and objects
        .clearObjects(objValues, iter = 1)
        .clearBrush(input, session, iter = 0)
    })

    # Assay change observer - change tab if assay changes
    observeEvent(input$assay, {
        updateTabsetPanel(session, "tabbox1",
                            selected = "tab1"
        )

        # Reset gates and objects
        .clearObjects(objValues, iter = 1)
        .clearBrush(input, session, iter = 1)
    })

    # Marker change observer - change tab if markers change
    observeEvent({
            lapply(seq_len(24), function(cur_mark){
                input[[paste0("Marker_", cur_mark)]]
            })
        }, {
            updateTabsetPanel(session, "tabbox1",
                            selected = "tab1"
            )

            # Reset gates and objects
            .clearObjects(objValues, iter = 1)
            .clearBrush(input, session, iter = 1)

    })

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

    observeEvent(input$resetMarkers, {

        cur_markers <- .select_markers(input, exprs_marker_update = FALSE)

        updateSelectInput(session, "exprs_marker_1",
                            choices = markers,
                            selected = cur_markers[1])
        updateSelectInput(session, "exprs_marker_2",
                            choices = markers,
                            selected = ifelse(length(cur_markers) > 1, 
                                                cur_markers[2], ""))
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
                                label = span(paste("Select marker", 
                                                    cur_val + 1),
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

        fluidRow(box(column(width = 12,
                        actionButton("resetMarkers", label = "Reset markers",
                        style = "background-color: #46EC46; color: black;")),
                column(width = 6,
                    selectInput("exprs_marker_1",
                        label = span(paste("Select marker 1"),
                            style = "color: black"),
                        choices = markers),
                    contrast_input_1),
                column(width = 6,
                    selectInput("exprs_marker_2",
                        label = span(paste("Select marker 2"),
                            style = "color: black"),
                        choices = c(markers, ""),
                        selected = ""),
                    contrast_input_2),
                column(width = 12,
                        svgPanZoomOutput("image_expression", height = "300px")),
                title = "Expression", status = "primary",
                width = 6, height = "550px"),
            box(
                column(width = 12,
                    svgPanZoomOutput("image_selection")),
                    title = "Selection", id = "selection", status = "primary",
                    width = 6, height = "550px"))
    })
}

# Function to allow brushing
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment metadata<-
.brushObject <- function(input, session, objValues, iter){

    cur_val <- (iter * 2) - 1

    if (is.null(objValues[[paste0("object", iter)]])) {
        return(NULL)
    }

    # Build data frame
    cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]], 
                                    input$assay)))
    cur_df$sample <- input$sample

    # Brush the data.frame
    cur_selection <- brushedPoints(cur_df, input[[paste0("plot_brush", iter)]], 
                                    allRows = TRUE)

    # Save the Gate
    cur_gate <- list()

    gate <- matrix(data = c(input[[paste0("plot_brush", iter)]]$xmin,
                            input[[paste0("plot_brush", iter)]]$xmax,
                            input[[paste0("plot_brush", iter)]]$ymin,
                            input[[paste0("plot_brush", iter)]]$ymax),
                    nrow = 2, ncol = 2,
                    byrow = TRUE,
                    dimnames = list(
                        c(input[[paste0("plot_brush", iter)]]$mapping$x,
                            input[[paste0("plot_brush", iter)]]$mapping$y), 
                        c("min", "max")
                        )
                    )

    if (rownames(gate)[1] == "sample") {
        gate <- gate[-1, , drop = FALSE]
    }

    cur_gate$gate <- gate
    cur_gate$exprs_values <- input$assay
    cur_gate$img_id <- input$sample

    # Save gates
    next_obj <- objValues[[paste0("object", iter)]]

    if (!is.null(metadata(next_obj)) && iter == 1) {
        cur_meta <- list(metadata = metadata(next_obj))
        cur_meta[[paste0("cytomapper_gate_", iter)]] <- cur_gate
        metadata(next_obj) <- cur_meta
    } else {
        metadata(next_obj)[[paste0("cytomapper_gate_", iter)]] <- cur_gate
    }

    if (sum(cur_selection$selected_) > 0) {
        objValues[[paste0("object", iter + 1)]] <- 
            next_obj[,cur_selection$selected_]
    } else {
        # Set next object to NULL
        objValues[[paste0("object", iter + 1)]] <- NULL

        # Clear all following objects
        .clearObjects(objValues, iter)
    }

}

# Scatter plot helpers
.plotScatter <- function(input, rValues, objValues, iter, cur_val){

    if (!is.null(objValues[[paste0("object", iter)]])) {
        cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]],
                                        input$assay)))
        cur_df$sample <- input$sample

        p <- ggplot(cur_df) +
            geom_point(aes_(as.name(input[[paste0("Marker_", cur_val)]]),
                            as.name(input[[paste0("Marker_", cur_val + 1)]])),
                        show.legend = FALSE) +
            theme(panel.background = element_blank()) +
            ylim(c(rValues$ranges[input[[paste0("Marker_", cur_val + 1)]], 1],
                    rValues$ranges[input[[paste0("Marker_", 
                                                cur_val + 1)]], 2])) +
            xlim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1],
                    rValues$ranges[input[[paste0("Marker_", cur_val)]], 2]))

        if (!is.null(objValues[[paste0("object", iter + 1)]])) {

            cur_df_1 <- as.data.frame(t(assay(objValues[[paste0("object", 
                                                                iter + 1)]],
                                            input$assay)))
            cur_df_1$sample <- input$sample

            p <- p + geom_point(aes_(as.name(input[[paste0("Marker_", 
                                                            cur_val)]]),
                                    as.name(input[[paste0("Marker_", 
                                                            cur_val + 1)]])),
                                show.legend = FALSE, data = cur_df_1, 
                                colour = "red")
        }

    } else {

        # We need to simulate a dataframe to allow brushing
        cur_df <- data.frame(row.names = "ph")
        cur_df[[input[[paste0("Marker_", cur_val)]]]] <- 1
        cur_df[[input[[paste0("Marker_", cur_val + 1)]]]] <- 1

        p <- ggplot(cur_df) +
            geom_blank(aes_(as.name(input[[paste0("Marker_", cur_val)]]),
                            as.name(input[[paste0("Marker_", cur_val + 1)]])),
                        show.legend = FALSE) +
            ylim(c(rValues$ranges[input[[paste0("Marker_", cur_val + 1)]], 1],
                rValues$ranges[input[[paste0("Marker_", cur_val + 1)]], 2])) +
            xlim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1],
                rValues$ranges[input[[paste0("Marker_", cur_val)]], 2])) +
            xlab(input[[paste0("Marker_", cur_val)]]) +
            ylab(input[[paste0("Marker_", cur_val + 1)]]) +
            theme(panel.background = element_blank())
    }
    return(p)
}

# Violin plot helper
.plotViolin <- function(input, rValues, objValues, iter, cur_val, cell_id){

    if (!is.null(objValues[[paste0("object", iter)]])) {

        cur_df <- as.data.frame(t(assay(objValues[[paste0("object", iter)]],
                                        input$assay)))
        cur_df$sample <- input$sample

        # If fewer than 3 points are selected, we will use point 
        # instead of violin plots
        if (nrow(cur_df) < 3) {
            p <- ggplot(cur_df) +
                geom_point(aes_(x = quote(sample),
                            y = as.name(input[[paste0("Marker_", cur_val)]])),
                            show.legend = FALSE) +
                xlab(input$sample) +
                theme(axis.text.x = element_blank(),
                        panel.background = element_blank()) +
                ylim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1],
                    rValues$ranges[input[[paste0("Marker_", cur_val)]], 2]))
        } else {
            p <- ggplot(cur_df) +
                geom_violin(aes_(x = quote(sample),
                            y = as.name(input[[paste0("Marker_", cur_val)]])),
                            show.legend = FALSE) +
                geom_quasirandom(aes_(x = quote(sample),
                                y = as.name(input[[paste0("Marker_", 
                                                        cur_val)]])),
                                show.legend = FALSE,
                                groupOnX = TRUE) +
                xlab(input$sample) +
                theme(axis.text.x = element_blank(),
                    panel.background = element_blank()) +
                ylim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1],
                        rValues$ranges[input[[paste0("Marker_", cur_val)]], 2]))
        }

        if (!is.null(objValues[[paste0("object", iter + 1)]])) {

            cur_df$selected <- 
                colData(objValues[[paste0("object", iter)]])[,cell_id] %in%
                colData(objValues[[paste0("object", iter + 1)]])[,cell_id]

            if (nrow(cur_df) < 3) {
                p <- p +
                    geom_point(aes_(x = quote(sample),
                                    y = as.name(input[[paste0("Marker_", 
                                                            cur_val)]]),
                                    colour = quote(selected)),
                                show.legend = FALSE, data = cur_df) +
                    scale_colour_manual(values = c(`FALSE` = "black",
                                                    `TRUE` = "red"))
            } else {
                p <- p +
                    geom_quasirandom(aes_(x = quote(sample),
                                        y = as.name(input[[paste0("Marker_", 
                                                                    cur_val)]]),
                                        colour = quote(selected)),
                                        show.legend = FALSE, data = cur_df,
                                        groupOnX = TRUE) +
                    scale_colour_manual(values = c(`FALSE` = "black",
                                                    `TRUE` = "red"))
            }

        }

    } else {

        # We need to simulate a dataframe to allow brushing
        cur_df <- data.frame(row.names = "ph")
        cur_df[[input[[paste0("Marker_", cur_val)]]]] <- 1
        cur_df$sample <- 1

        p <- ggplot(cur_df) +
            geom_blank(aes_(x = quote(sample),
                            y = as.name(input[[paste0("Marker_", cur_val)]])),
                        show.legend = FALSE) +
            ylim(c(rValues$ranges[input[[paste0("Marker_", cur_val)]], 1],
                    rValues$ranges[input[[paste0("Marker_", cur_val)]], 2])) +
            xlim(c(0.5, 1.5)) +
            ylab(input[[paste0("Marker_", cur_val)]]) +
            xlab(input$sample) +
            theme(axis.text.x = element_blank(),
                    panel.background = element_blank())
    }
    return(p)
}

# Create scatter plots
#' @import ggplot2
#' @importFrom ggbeeswarm geom_quasirandom
.createScatter <- function(input, session, rValues, objValues, 
                            iter, img_id, cell_id){

    renderPlot({

        req(input$sample)

        cur_val <- (iter * 2) - 1

        if (iter == 1) {
            req(rValues$ranges, objValues$object1,
                input$assay, input$Marker_1)
        } else {
            req(rValues$ranges, input[[paste0("plot_brush", iter - 1)]],
                input$assay, input[[paste0("Marker_", cur_val)]])
        }

        if (iter > 1 && is.null(input[[paste0("plot_brush", iter - 1)]])) {
            return(NULL)
        }

        if (is.null(input[[paste0("plot_brush", iter)]])) {

            .clearObjects(objValues, iter)
            .clearBrush(input, session, iter)

        } else {
            .brushObject(input, session, objValues, iter = iter)
        }

        # Plot scatter or violin
        if (input[[paste0("Marker_", cur_val + 1)]] != "") {
            p <- .plotScatter(input, rValues, objValues, iter, cur_val)
        } else {
            p <- .plotViolin(input, rValues, objValues, iter, cur_val, cell_id)
        }

        return(p)
    })
}

# Helper function to select markers
# By default, we want to display the expression of the marker on
# which gating was performed last.
# The user can further select other markers for displaying
.select_markers <- function(input, exprs_marker_update = TRUE){
    if (exprs_marker_update && !is.null(input$exprs_marker_1) &&
        input$exprs_marker_1 != "") {
        if (input$exprs_marker_2 != "") {
            cur_markers <- c(input$exprs_marker_1, input$exprs_marker_2)
    }
    else{
            cur_markers <- input$exprs_marker_1
        }
    } else {

        req(input$Marker_1)

        cur_input <- reactiveValuesToList(input)
        cur_markers <- cur_input[grepl("Marker_", names(cur_input))]
        cur_markers <- cur_markers[unlist(lapply(cur_markers, 
                                                function(x){x != ""}))]

        # Subset markers to those of the last gate + 1
        cur_brushes <- cur_input[grepl("plot_brush", names(cur_input))]

        if(length(cur_brushes) > 0) {
            cur_brushes <- cur_brushes[!unlist(lapply(cur_brushes, is.null))]
        }

        if (length(cur_brushes) > 0) {
            max_brush <- max(as.numeric(sub("plot_brush", "", 
                                            names(cur_brushes))))

            if (!is.null(input[[paste0("Marker_", 
                                        ((max_brush + 1) * 2) - 1)]]) &&
                input[[paste0("Marker_", ((max_brush + 1) * 2) - 1)]] != "") {
                cur_markers <- cur_markers[
                    names(cur_markers) == paste0("Marker_", 
                                                ((max_brush + 1) * 2) - 1) |
                    names(cur_markers) == paste0("Marker_", 
                                                (max_brush + 1) * 2)]
            } else {
                cur_markers <- cur_markers[
                    names(cur_markers) == paste0("Marker_", 
                                                ((max_brush) * 2) - 1) |
                    names(cur_markers) == paste0("Marker_", (max_brush) * 2)]
            }

        } else {
            cur_markers <- cur_markers[names(cur_markers) == "Marker_1" |
                                            names(cur_markers) == "Marker_2"]
        }

        max_marker <- max(as.numeric(sub("Marker_", "", names(cur_markers))))

        if (max_marker %% 2 == 0) {
            cur_markers <- c(cur_markers[[paste0("Marker_", max_marker - 1)]],
                                cur_markers[[paste0("Marker_", max_marker)]])
        } else {
            cur_markers <- cur_markers[[paste0("Marker_", max_marker)]]
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
.createImageExpression <- function(input, object, mask, 
                                    image, img_id, cell_id, ...){
    renderSvgPanZoom({

        cur_markers <- .select_markers(input)
        cur_bcg <- .select_contrast(input)

        if (is.null(image)) {
            cur_mask <- mask[mcols(mask)[,img_id] == input$sample]
            suppressMessages(svgPanZoom(stringSVG(
                    plotCells(object = object,
                            mask = cur_mask,
                            cell_id = cell_id,
                            img_id = img_id,
                            colour_by = cur_markers,
                            exprs_values = input$assay,
                            ...)),
                    zoomScaleSensitivity = 0.4, maxZoom = 20,
                    controlIconsEnabled = TRUE, viewBox = FALSE))
        } else {

            if (length(cur_markers) > 1) {
                validate(need(cur_markers[1] != cur_markers[2],
                            message = "Please specify two different markers"))
            }

            cur_image <- image[mcols(image)[,img_id] == input$sample]
            suppressMessages(svgPanZoom(stringSVG(
                    plotPixels(image = cur_image,
                            colour_by = cur_markers,
                            bcg = cur_bcg,
                            ...)),
                    zoomScaleSensitivity = 0.4, maxZoom = 20,
                    controlIconsEnabled = TRUE, viewBox = FALSE))
        }
    })
}

# Visualize selected cells on images
#' @importFrom svgPanZoom svgPanZoom renderSvgPanZoom
#' @importFrom svglite stringSVG
#' @importFrom S4Vectors metadata
.createImageSelection <- function(input, objValues, 
                                    mask, image, img_id, cell_id, ...){
    renderSvgPanZoom({

        cur_val <- (input$plotCount * 2) - 1

        req(input$plot_brush1)

        cur_markers <- .select_markers(input)
        cur_bcg <- .select_contrast(input)
        
        cur_object <- reactiveValuesToList(objValues)
        cur_object <- cur_object[!unlist(lapply(cur_object, is.null))]
        
        cur_ln <- length(cur_object)
        
        cur_object <- cur_object[[paste0("object", length(cur_object))]]

        cur_mask <- mask[mcols(mask)[,img_id] == input$sample]

        if (unique(mcols(cur_mask)[,img_id]) != 
            unique(colData(cur_object)[,img_id])) {
            return(NULL)
        }

        if (is.null(image)) {
            
            if (cur_ln == 1) {
                suppressMessages(svgPanZoom(stringSVG(
                    plotCells(mask = cur_mask,
                                legend = NULL,
                                ...)),
                    zoomScaleSensitivity = 0.4, maxZoom = 20,
                    controlIconsEnabled = TRUE, viewBox = FALSE))
            } else {
                cur_object$selected <- TRUE
                suppressMessages(svgPanZoom(stringSVG(
                    plotCells(object = cur_object,
                                mask = cur_mask,
                                cell_id = cell_id,
                                img_id = img_id,
                                colour_by = "selected",
                                colour = list(selected = c("TRUE" = "dark red", 
                                                            "FALSE" = "gray")),
                                legend = NULL,
                                ...)),
                    zoomScaleSensitivity = 0.4, maxZoom = 20,
                    controlIconsEnabled = TRUE, viewBox = FALSE))
            }

        } else {

            if (length(cur_markers) > 1) {
                validate(need(cur_markers[1] != cur_markers[2],
                            message = "Please specify two different markers"))
            }

            cur_image <- image[mcols(image)[,img_id] == input$sample]
            
            
            if (cur_ln == 1) {
                suppressMessages(svgPanZoom(stringSVG(
                    plotPixels(image = cur_image,
                                colour_by = cur_markers,
                                legend = NULL,
                                bcg = cur_bcg,
                                ...)),
                    zoomScaleSensitivity = 0.4, maxZoom = 20,
                    controlIconsEnabled = TRUE, viewBox = FALSE))
            } else {
                cur_object$selected <- TRUE
                suppressMessages(svgPanZoom(stringSVG(
                    plotPixels(image = cur_image,
                                object = cur_object,
                                mask = cur_mask,
                                cell_id = cell_id,
                                img_id = img_id,
                                colour_by = cur_markers,
                                outline_by = "selected",
                                colour = list(selected = c("TRUE" = "white", 
                                                            "FALSE" = "gray")),
                                legend = NULL,
                                bcg = cur_bcg,
                                ...)),
                    zoomScaleSensitivity = 0.4, maxZoom = 20,
                    controlIconsEnabled = TRUE, viewBox = FALSE))
            }
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

            cur_object$cytomapper_CellLabel <- input$labelCellsBy

            # Add session info
            metadata(cur_object)$cytomapper_SessionInfo <- sessionInfo()

            # Add date
            metadata(cur_object)$cytomapper_GatingDate <- Sys.Date()

            saveRDS(cur_object, file)
        }
    )
}
