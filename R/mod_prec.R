################################################################################
##                         OWASA Portal                                       ##
##                                                                            ##
##                   Module: Precipitation                                    ##
################################################################################
## Main description:                                                          ##
##                                                                            ##
##  This module displays the gridded files of different precipitation         ##
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
mod_prec_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("Prec_prod"), label = "Select the P product", 
                    choices = c("CHIRPSv2", "ERA5", "ERA5 Land", 
                                "MSWEPv2.8", "PERSIANN-CDR", "PERSIANN-CCS-CDR"), selected = TRUE),
        tags$div(id = 'p_data',
                 span(tags$i(h4(textOutput(ns("plot_prec_caption")))),
                      style="color:#045a8d"),
                 manipulateWidget::combineWidgetsOutput(ns("plot_P"), width = "100%", height = "600px")

        ),
        
        
        
      ),
      mainPanel(leaflet::leafletOutput(ns("prec_map"),height = "800px", width = "100%"))
    )
  )
  
}


# Server function of the module
mod_prec_server <- function(id, map) {
  moduleServer(
    id,
    function(input, output, session) {

      output$prec_map <- leaflet::renderLeaflet(map)
      
      # Reactive object to store the information of the selected station
      selected_cell <- eventReactive(input$prec_map_draw_new_feature, {
        
        coords <- data.frame(x = input$prec_map_draw_new_feature$geometry$coordinates[[1]],
                             y = input$prec_map_draw_new_feature$geometry$coordinates[[2]])  
        
        return(coords)
      })
      
      output$plot_P <- manipulateWidget::renderCombineWidgets({

        req(input$prec_map_draw_new_feature)
        req(input$Prec_prod)
        get_prec_plot(vls = get_prec_values(product = input$Prec_prod,
                                                   x = selected_cell()$x, y = selected_cell()$y))
        
      })
      
      # Output object that contains the caption of the selected station
      output$plot_prec_caption <- renderText({
        glue::glue("To visualise the annual precipitation and the mean monthly precipitation for a desired
                        point, please go to the drawing tools of the interactive map, select the point
                        option, and click on the map!")
      })
      
      
    }
  )
}
