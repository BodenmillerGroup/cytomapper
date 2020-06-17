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

body <- dashboardBody(
    fluidRow(
        tabBox(
            title = "First tabBox",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", height = "250px",
            tabPanel("Tab1", "First tab content"),
            tabPanel("Tab2", "Tab content 2")
        ),
        tabBox(
            side = "right", height = "250px",
            selected = "Tab3",
            tabPanel("Tab1", "Tab content 1"),
            tabPanel("Tab2", "Tab content 2"),
            tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
        )
    ),
    fluidRow(
        tabBox(
            # Title can include an icon
            title = tagList(shiny::icon("gear"), "tabBox status"),
            tabPanel("Tab1",
                     "Currently selected tab from first box:",
                     verbatimTextOutput("tabset1Selected")
            ),
            tabPanel("Tab2", "Tab content 2")
        )
    )
)

.cytomapper_body <- function(){
    cm_body <- dashboardBody(
        tabBox(width = 12,
            tabPanel(title = "Scatter Plots", uiOutput("AdditionalPlots_main")),
            tabPanel(title = "Images", fluidRow(box(column(width = 6, selectizeInput("exprs_marker_1",
                                                                                          label = span(paste("Select marker 1"), 
                                                                                          style = "color: black; padding-top: 0px"), 
                                                                                          choices = NULL,
                                                                                          options = list(placeholder = '', 
                                                                                                         maxItems = 1))),
                                                         column(width = 6, selectizeInput("exprs_marker_2",
                                                                                          label = span(paste("Select marker 2"), 
                                                                                                       style = "color: black; padding-top: 0px"), 
                                                                                          choices = NULL,
                                                                                          options = list(placeholder = '', 
                                                                                                         maxItems = 1))),
                                                         plotOutput("image_expression", height = "300px"), 
                                                         title = "Expression", status = "primary",
                                                         width = 6, height = "500px"),
                                                     box(svgPanZoomOutput("image_selection"), 
                                                         title = "Selection", status = "primary",
                                                         width = 6, height = "500px")))
        )
        )
    return(cm_body)
    }


