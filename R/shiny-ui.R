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
                text = textInput(inputId = "labelCellsBy",
                            label = "Cell label",
                            value = "Cell-Type"),
                icon = icon(""),
                status = "info"
            ),
            notificationItem(
                text = downloadButton(
                    outputId = "downloadData",       
                    label = "Download selection",
                    style = "background-color: #3C8DBC; color: white; border-color: #7EA6F8"
                ),
                icon = icon(""),
                status = "info"
            ),
            type = "notification",
            icon = icon("fas fa-download"),
            badgeStatus = NULL,
            headerText = ""),
        dropdownMenu(
            notificationItem(
                text = actionButton(
                    inputId = "SessionInfo",       
                    label = "Session Info",
                    style = "background-color: #3C8DBC; color: white; border-color: #3C8DBC"
                ),
                icon = icon(""),
                status = "info"
            ),
            notificationItem(
                text = actionButton(
                    inputId = "Help",       
                    label = "Help",
                    style = "background-color: #3C8DBC; color: white; border-color: #3C8DBC"
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
            menuItem("General controls",
                     sliderInput("plotCount", label = "Select number of plots",
                                    min = 1, max = 12, value = 1),
                     selectizeInput("sample", label = "Select sample",
                                    choices = NULL,
                                    options = list(placeholder = 'Select a condition', maxItems = 1)),
                     selectizeInput("assay", label = "Select which assay to display",
                                    choices = NULL, options = list(placeholder = 'Select an assay', maxItems = 1)),
                     selectizeInput("reducedDim", label = "Select which reduced dimension to display",
                                    choices = NULL, options = list(placeholder = 'Select an embedding', maxItems = 1)),
                     icon = icon("fas fa-sliders-h"), 
                     startExpanded = TRUE),
            menuItem("Plots",
                uiOutput("AdditionalPlots_sidebar"),
                icon = icon("far fa-chart-bar"), startExpanded = TRUE),
            id = "sidebar"
            )
        )
    
    return(cm_side)
    
    }

# Create the main body
#' @importFrom shinydashboard dashboardBody
.cytomapper_body <- function(){
    cm_body <- dashboardBody(
        uiOutput("AdditionalPlots_main"),
        fluidRow(box(plotOutput("image_expression"), 
                     title = "Expression", status = "primary",
                     width = 6),
                 box(plotOutput("image_selection"), 
                     title = "Selection", status = "primary",
                     width = 6))
        )
    return(cm_body)
    }

