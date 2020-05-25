# -----------------------------------------------------------------------------
# Helper functions to modify the visual appearance of the shiny app
# -----------------------------------------------------------------------------

# Create the application header
#' @importFrom utils packageVersion
.cytomapper_header <- function(){
    cm_head <- dashboardHeader(
        title = paste0("cytomapper v",
                        packageVersion("cytomapper")),
        dropdownMenu(
            notificationItem(
                text = downloadButton(
                    outputId = "downloadData",       
                    label = "Download selection"
                ),
                icon = icon(""),
                status = "info"
            ),
            type = "tasks",
            icon = icon("fas fa-download"),
            badgeStatus = NULL,
            headerText = ""),
        dropdownMenu(
            notificationItem(
                text = actionButton(
                    inputId = "SessionInfo",       
                    label = "Session Info"
                ),
                icon = icon(""),
                status = "info"
            ),
            notificationItem(
                text = actionButton(
                    inputId = "Help",       
                    label = "Help"
                ),
                icon = icon(""),
                status = "info"
            ),
            type = "tasks",
            icon = icon("fas fa-question"),
            badgeStatus = NULL,
            headerText = "")
        )
    return(cm_head)
    }

# Create the side bar layout
#' @importFrom shinydashboard dashboardSidebar
.cytomapper_sidebar <- function(){
    cm_side <- dashboardSidebar(
        
        sidebarMenu(
            menuItem("Plots",
                h3("Plot 1"),
                selectizeInput("Marker_1", label = "Select marker 1", choices = NULL,
                               options = list(placeholder = 'Select a marker name', maxItems = 1)),
                selectizeInput("Marker_2", label = "Select marker 2", choices = NULL, 
                               options = list(placeholder = NULL, maxItems = 1)),
                h3("Plot 2"),
                selectizeInput("Marker_3", label = "Select marker 3", choices = NULL,
                               options = list(placeholder = 'Select a marker name', maxItems = 1)),
                selectizeInput("Marker_4", label = "Select marker 4", choices = NULL, 
                               options = list(placeholder = NULL, maxItems = 1)),
                h3("Plot 3"),
                selectizeInput("Marker_1", label = "Select marker 5", choices = NULL,
                               options = list(placeholder = 'Select a marker name', maxItems = 1)),
                selectizeInput("Marker_2", label = "Select marker 6", choices = NULL, 
                               options = list(placeholder = NULL, maxItems = 1)),
                icon = icon("far fa-chart-bar"), startExpanded = TRUE),
            selectizeInput("sample", label = "Select sample",
                           choices = NULL,
                           options = list(placeholder = 'Select a condition', maxItems = 1)),
            selectizeInput("assay", label = "Select which assay to display",
                           choices = NULL, options = list(placeholder = 'Select an assay', maxItems = 1)),
            selectizeInput("reducedDim", label = "Select which reduced dimension to display",
                           choices = NULL, options = list(placeholder = 'Select a reduced dimension', maxItems = 1)),
            id = "sidebar"
            )
        )
    
    return(cm_side)
    
    }

# Create the main body
#' @importFrom shinydashboard dashboardBody
.cytomapper_body <- function(){
    cm_body <- dashboardBody(
        fluidRow(box(plotOutput("scatter1", brush = "plot_brush1"),
                        verbatimTextOutput("info1"),
                     title = "Plot 1", status = "primary",
                     width = 4),
                 box(plotOutput("scatter2", brush = "plot_brush2"),
                     verbatimTextOutput("info2"),
                     title = "Plot 2", status = "primary",
                     width = 4),
                 box(plotOutput("scatter3", brush = "plot_brush3"),
                     verbatimTextOutput("info3"),
                     title = "Plot 3", status = "primary",
                     width = 4)),
        fluidRow(box(plotOutput("image_expression"), 
                     title = "Expression", status = "primary",
                     width = 6),
                 box(plotOutput("image_selection"), 
                     title = "Selection", status = "primary",
                     width = 6))
        )
    return(cm_body)
    }

