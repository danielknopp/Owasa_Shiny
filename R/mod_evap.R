################################################################################
##                         OWASA Portal                                       ##
##                                                                            ##
##                   Module: Evaporation                                      ##
################################################################################
## Main description:                                                          ##
##                                                                            ##
##  This module displays the gridded files of different evaporation           ##
##    products. The users can select a point in the map and they can          ##
##    visualise i) the annual variations and ii) the seasonal distribution.   ##
##                                                                            ##
##  Authors:                                                                  ##
##                                                                            ##
##    - Oscar M. Baez-Villanueva                                              ##
##    - Juan Miguel Viquez                                                    ##
##                                                                            ##
################################################################################

# UI function of the module
mod_evap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("Evap_prod"), label = "Select the E product", 
                    choices = c("ERA5", "ERA5 Land", "GLEAMv3.6"), selected = TRUE),
        tags$div(id = 'e_data',
                 span(tags$i(h4(textOutput(ns("plot_evap_caption")))),
                      style="color:#045a8d"),
                 manipulateWidget::combineWidgetsOutput(ns("plot_E"), width = "100%", height = "600px")
                 
        ),
        
        
        
      ),
      mainPanel(leaflet::leafletOutput(ns("evap_map"),height = "800px", width = "100%"))
    )
  )
  
}


# Server function of the module
mod_evap_server <- function(id, map) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$evap_map <- leaflet::renderLeaflet(map)
      
      # Reactive object to store the information of the selected station
      selected_cell <- eventReactive(input$evap_map_draw_new_feature, {
        
        coords <- data.frame(x = input$evap_map_draw_new_feature$geometry$coordinates[[1]],
                             y = input$evap_map_draw_new_feature$geometry$coordinates[[2]])  
        
        return(coords)
      })
      
      
      output$plot_E <- manipulateWidget::renderCombineWidgets({
        
        req(input$evap_map_draw_new_feature)
        req(input$Evap_prod)
        get_evap_plot(vls = get_evap_values(product = input$Evap_prod,
                                                   x = selected_cell()$x, y = selected_cell()$y))
        
      })
      
      # Output object that contains the caption of the selected station
      output$plot_evap_caption <- renderText({
        glue::glue("To visualise the annual evaporation and the mean monthly evaporation for a desired
                        point, please go to the drawing tools of the interactive map, select the point
                        option, and click on the map!")
      })
      
      
    }
  )
}
