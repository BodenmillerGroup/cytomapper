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
.cytomapper_sidebar <- function(){
    cm_side <- dashboardSidebar(
        sidebarMenu(
            menuItem("General controls",
                     sliderInput("plotCount", label = "Select number of plots",
                                 min = 1, max = 12, value = 1),
                     fluidRow(column(
                         12, p(strong("Select sample")), style="padding-left:30px;")),
                     fluidRow(column(2, 
                         actionButton("previous.sample", label = NULL,
                                      icon = icon("angle-left", class="fa-2x"),
                                      style = "background-color:transparent;border-color:transparent;color:white;margin-left:0px;")),
                         column(8, style="padding-left:0px;padding-right:0px;",
                         selectizeInput("sample", label = NULL,
                                        width = "100%",
                                        choices = NULL,
                                        options = list(placeholder = 'Select a sample', 
                                                       maxItems = 1))),
                         column(2,style="padding-left:0px;",
                         actionButton("next.sample", label = NULL,
                                      icon = icon(name = "angle-right", class="fa-2x"),
                                      style = "background-color:transparent;border-color:transparent;color:white;margin-left:0px;padding-left:0px;"))),

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
#' @importFrom svgPanZoom svgPanZoomOutput
.cytomapper_body <- function(){
    cm_body <- dashboardBody(

        tabBox(width = 12, id = "tabbox1",
            tabPanel(title = "Scatter Plots", value = "tab1", uiOutput("AdditionalPlots_tab1")),
            tabPanel(title = "Images", value = "tab2", uiOutput("AdditionalPlots_tab2")))
        )

    return(cm_body)

    }


