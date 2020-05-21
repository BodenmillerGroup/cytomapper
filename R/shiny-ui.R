# -----------------------------------------------------------------------------
# Helper functions to modify the visual appearance of the shiny app
# -----------------------------------------------------------------------------

# Create the application header
#' @importFrom shinydashboard dashboardHeader
.cytomapper_header <- function(){
    cm_head <- dashboardHeader(title = "cytomapper gating")
    return(cm_head)
    }

# Create the side bar layout
#' @importFrom shinydashboard dashboardSidebar
.cytomapper_sidebar <- function(){
    cm_side <- dashboardSidebar(
        helpText("This Shiny App visualizes protein abundance on reduced dimensions and images.",
                         "It further visualizes the counts on images and allows gating of cells."),
        selectizeInput("Marker_1", label = "Select marker 1", choices = NULL, options =
                                   list(placeholder = 'Select a marker name', maxItems = 1)),
        helpText("Please select the protein marker you would like to visualize.",
                         "When only selecting the first marker, the distribution of ion counts are shown."),
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
        helpText("Please select one sample and ROI for visualization."),
        selectizeInput("assay", label = "Select which assay to display",
                               choices = NULL, options =
                                   list(placeholder = 'Select an assay', maxItems = 1)),
        helpText("Here, you can select from which assay to display the counts"),
        selectizeInput("reducedDim", label = "Select which reduced dimension to display",
                               choices = NULL, options =
                                   list(placeholder = 'Select a reduced dimension', maxItems = 1)),
        helpText("Please select the reduced dimension to display"),
                
        downloadButton("downloadData", "Download")
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

