################################################################################
##                         OWASA Portal                                       ##
##                                                                            ##
##                   Module: Soil Moisture                                    ##
################################################################################
## Main description:                                                          ##
##                                                                            ##
##  This module displays the gridded files of different soil moisture         ##
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
mod_sm_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("SM_prod"), label = "Select the SM product", 
                    choices = c("ERA5", "ERA5 Land", "GLEAMv3.6"), selected = TRUE),
        tags$div(id = 'sm_data',
                 span(tags$i(h4(textOutput(ns("plot_sm_caption")))),
                      style="color:#045a8d"),
                 manipulateWidget::combineWidgetsOutput(ns("plot_E"), width = "100%", height = "600px")
                 
        ),
        
        
        
      ),
      mainPanel(leaflet::leafletOutput(ns("sm_map"),height = "925px", width = "100%"))
    )
  )
  
}


# Server function of the module
mod_sm_server <- function(id, map) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$sm_map <- leaflet::renderLeaflet(map)
      
      # Reactive object to store the information of the selected station
      selected_cell <- eventReactive(input$sm_map_draw_new_feature, {
        
        coords <- data.frame(x = input$sm_map_draw_new_feature$geometry$coordinates[[1]],
                             y = input$sm_map_draw_new_feature$geometry$coordinates[[2]])  
        
        return(coords)
      })
      
      output$plot_E <- manipulateWidget::renderCombineWidgets({
        
        req(input$sm_map_draw_new_feature)
        req(input$SM_prod)
        get_sm_plot(vls = OWASA::get_sm_values(product = input$SM_prod,
                                                   x = selected_cell()$x, y = selected_cell()$y))
        
      })
      
      # Output object that contains the caption of the selected station
      output$plot_sm_caption <- renderText({
        glue::glue("To visualise the annual soil moisture and the mean monthly soil moisture for a desired
                        point, please go to the drawing tools of the interactive map, select the point
                        option, and click on the map!")
      })
      
      
    }
  )
}
