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
        selectizeInput("Marker_1", label = "Select marker 1", choices = NULL, options =
                                   list(placeholder = 'Select a marker name', maxItems = 1)),
        selectizeInput("Marker_2", label = "Select marker 2", choices = NULL, options =
                                   list(placeholder = NULL, maxItems = 1)),
        selectizeInput("Marker_3", label = "Select marker 3", choices = NULL, options =
                                   list(placeholder = NULL, maxItems = 1)),
        selectizeInput("Marker_4", label = "Select marker 4", choices = NULL, options =
                                   list(placeholder = NULL, maxItems = 1)),
        selectizeInput("Marker_5", label = "Select marker 5", choices = NULL, options =
                                   list(placeholder = NULL, maxItems = 1)),
        selectizeInput("Marker_6", label = "Select marker 6", choices = NULL, options =
                                   list(placeholder = NULL, maxItems = 1)),
        selectizeInput("sample", label = "Select sample",
                               choices = NULL,
                               options = list(placeholder = 'Select a condition', maxItems = 1)),
        selectizeInput("assay", label = "Select which assay to display",
                               choices = NULL, options =
                                   list(placeholder = 'Select an assay', maxItems = 1)),
        selectizeInput("reducedDim", label = "Select which reduced dimension to display",
                               choices = NULL, options =
                                   list(placeholder = 'Select a reduced dimension', maxItems = 1))
        )
    
    return(cm_side)
    }

# Create the main body
#' @importFrom shinydashboard dashboardBody
.cytomapper_body <- function(){
    cm_body <- dashboardBody(
        fluidRow(column(6, plotOutput("scatter1", brush = "plot_brush1"),
                        verbatimTextOutput("info1")),
                 column(6, plotOutput("scatter2", brush = "plot_brush2"),
                        verbatimTextOutput("info2"))),
        fluidRow(column(6, plotOutput("scatter3", brush = "plot_brush3"),
                        verbatimTextOutput("info3")),
                 column(6, plotOutput("reducedDim_expression"))),
        fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("image_expression"),
                             plotOutput("image_selection")))
        )
    return(cm_body)
    }

