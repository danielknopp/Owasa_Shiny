################################################################################
##                         OWASA Portal                                       ##
##                                                                            ##
##                   Module: Interactive map                                  ##
################################################################################
## Main description:                                                          ##
##                                                                            ##
##  This module displays the interactive map of the Nile Research Portal with ##
##  the respective location of the ground-based precipitation and streamflow  ##
##  stations. If a station is selected by the user, a pop-up window will      ##
##  indicate the station's name, Additionally, the montly time series and     ##
##  mean monthly values will be displayed on the screen.                      ##
##                                                                            ##
##  Authors:                                                                  ##
##                                                                            ##
##    - Oscar M. Baez-Villanueva                                              ##
##    - Juan Miguel Viquez                                                    ##
##                                                                            ##
################################################################################

# UI function of the module
mod_cordex_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
                   tags$div(id = 'cord_data',
                            span(tags$i(h5(textOutput(ns("plot_cord_caption")))),
                                 style="color:#045a8d")),
                   
                   selectInput(ns("Input_variable"), label = "Select variable to display", choices = c("Precipitation", "Temperature"), selected = TRUE),
                            
                   selectInput(ns("Period"), label = "Period (for seasonality plot)", choices = c("2006-2020", "2021-2040",
                                                                           "2041-2060", "2061-2080",
                                                                           "2081-2100"), selected = TRUE),
                   manipulateWidget::combineWidgetsOutput(ns("plot_monthly_period"), width = "100%", height = "500px")
                   
      ),
      mainPanel(leaflet::leafletOutput(ns("cordex_map"), height = "800px", width = "100%"))
    )
  )
  
}


# Server function of the module
mod_cordex_server <- function(id, map) {
  moduleServer(
    id,
    function(input, output, session) {

      output$cordex_map <- leaflet::renderLeaflet(map)
      
      # Reactive object to store the information of the selected station
      selected_cell <- eventReactive(input$cordex_map_draw_new_feature, {
        
        coords <- data.frame(x = input$cordex_map_draw_new_feature$geometry$coordinates[[1]],
                             y = input$cordex_map_draw_new_feature$geometry$coordinates[[2]])  
        
        return(coords)
      })
      
      
      output$plot_monthly_period <- manipulateWidget::renderCombineWidgets({
        
        req(input$cordex_map_draw_new_feature)
        req(input$Input_variable)
        req(input$Period)
        
        get_cordex_monthly_plot(vls_monthly = get_cordex_monthly_values(variable = input$Input_variable,
                                                                       period = input$Period,
                                                                     x = selected_cell()$x, y = selected_cell()$y),
                                vls_annual = get_cordex_annual_values(variable = input$Input_variable,
                                                                             x = selected_cell()$x, y = selected_cell()$y))
        
      })
      
      # Output object that contains the caption of the selected station
      output$plot_cord_caption <- renderText({
        glue::glue("The following window displays precipitation and temperature data for the RCP4.5 and RCP8.5 projections from CORDEX using
        the Regional Climate Model (CCCma-CanRSM4) from the Canadian Center for Climate Modelling and Analysis using the ensemble member r1i1p1.
        Please select a point on the map to visualise the annual P or T and the mean monthly precipitation or temperature (using the drawing tools of the map).")
      })
      
      
    }
  )
}
