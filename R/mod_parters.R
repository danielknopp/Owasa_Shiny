################################################################################
##                         OWASA Portal                                       ##
##                                                                            ##
##                   Module: Partnerships                                     ##
################################################################################
## Main description:                                                          ##
##                                                                            ##
##  This module displays the partners associated to OWASA projects            ##
##    products. The users can visualize the location and description of       ##
##    activities that are being carried out along the project. Also a         ##
##    description of the general idea of the gap to be filled by us           ##
##    visualise i) the annual variations and ii) the seasonal distribution.   ##
##                                                                            ##
##  Authors:                                                                  ##
##                                                                            ##
##    - Daniel Knopp Manquean                                                 ##
##    - Justyna Sycz                                                          ##
##                                                                            ##
################################################################################


# UI function of the module
mod_partners_ui <- function(id) {
  ns <- NS(id)
  tagList(shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(title = "Owasa Partnership"),
    shinydashboardPlus::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("OWASA Partnerships", tabName = "map", icon = icon("gears")),
        shinydashboard::menuItem("About Us", tabName = "about", icon = icon("th"),
                                 badgeLabel = "In Progress", badgeColor = "navy")
      )
    ),
    shinydashboard::dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet",type = "text/css", href = "CSS/stakeholders_map.css")
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "map", 
                                    fluidRow(
                                      column(width = 6,
                                      leafletOutput(ns("mymap"), width ="100%", height= "400px")
                                      ),
                                      column(
                                        width = 6,
                                        shinydashboard::tabBox(id = "sidebar",
                                                             width = 12,
                                                             tabPanel(
                                                               "Marker Info",
                                                               icon = icon("gears"),
                                                               div(
                                                                 id = ns("markerInfoWrapper"),
                                                                 uiOutput(ns("markerInfo"))
                                                                 )
                                                               )
                                                             )
                                        )
                                      )
                                ),
        shinydashboard::tabItem(tabName = "about",
                                fluidRow(
                                  column(width = 6,
                                         h2("In Progress")
                                         )
                                  )
                                )
        )
      )
    )
  )
}

mod_partners_server <- function(id, map) {
  moduleServer(
    id,
    function(input, output, session) {
      output$mymap <- leaflet::renderLeaflet(map)
      
      observeEvent(input$mymap_marker_click, {
        # Get the clicked marker's ID
        clicked_id <- input$mymap_marker_click$lat
        data <- get_partner_data()
        # Find the marker information based on the clicked ID
        marker_info <- data[data$latitud == clicked_id, ]
        
        # Fade out the existing content
        shinyjs::runjs("$('#markerInfoWrapper').fadeOut(200);")
        
        # Update the marker information with a delay
        shiny::invalidateLater(800, session, {
          output$markerInfo <- renderUI({
            tags$div(
              class = "marker-details",
              tags$style(HTML("
                .marker-details {
                  font-size: 14px;
                  max-height: 600px;
                  overflow-y: auto;
                }
              ")),
              tags$div(HTML(markdown::markdownToHTML(marker_info$markdown, fragment.only = TRUE)))
            )
          })
          
          # Fade in the updated content
          shinyjs::runjs("$('#markerInfoWrapper').fadeIn(200);")
        })
      })
    }
  )
}

