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
                   selectInput(ns("Input_Package"), label = "Select the implementation", choices = c("SPEI", "SCI"), selected = TRUE),
                   tags$div(id = 'demo',
                            manipulateWidget::combineWidgetsOutput(ns("plot_daily"), width = "100%", height = "250px"),
                            manipulateWidget::combineWidgetsOutput(ns("plot_monthly"), width = "100%", height = "200px"),
                            fluidRow(
                              column(4, numericInput(ns("H"), label = "Disturbance", value = NULL, min = 0, max =7, step = 0.25)),
                              column(4, numericInput(ns("DaysMOVA"), label = "Days for MA", value = NULL, min = 5, max = 90)),
                              column(4, selectInput(ns("Filter"), label = "Smoother?", choices = c("KF", "MA"), selected = TRUE))
                            ),
                            manipulateWidget::combineWidgetsOutput(ns("plot_smooth"), width = "100%", height = "270px")
                   ),

                   
                   
      ),
      mainPanel(leaflet::leafletOutput(ns("station_map"),height = "925px", width = "100%"))
    )
  )
  
}


# Server function of the module
mod_cordex_server <- function(id, map) {
  moduleServer(
    id,
    function(input, output, session) {
      # output$station_map <- leaflet::renderLeaflet(map)
      # 
      # # Reactive object to store the information of the selected station
      # selected_cell <- eventReactive(input$station_map_draw_new_feature, {
      #   
      #   coords <- data.frame(x = input$station_map_draw_new_feature$geometry$coordinates[[1]],
      #                        y = input$station_map_draw_new_feature$geometry$coordinates[[2]])          
      #   
      #   return(coords)
      # })
      # 
      # # Output object that contains the daily interactive graph. The function
      # output$plot_shape_daily <- dygraphs::renderDygraph({
      #   req(input$station_map_draw_new_feature)
      #   req(input$Input_Package)
      #   get_plot(vls = SPIGamma::get_values(x = selected_cell()$x, y = selected_cell()$y), 
      #            package = input$Input_Package,
      #            parameter = "shape",
      #            scale = "Daily")
      # })
      # 
      # # Output object that contains the daily interactive graph. The function
      # output$plot_daily <- manipulateWidget::renderCombineWidgets({
      #   req(input$station_map_draw_new_feature)
      #   req(input$Input_Package)
      #   get_combined_plot(vls = SPIGamma::get_values(x = selected_cell()$x, y = selected_cell()$y), 
      #                     package = input$Input_Package,
      #                     scale = "Daily")
      #   
      # })
      # 
      # # Output object that contains the monthly interactive graph. The function
      # output$plot_monthly <- manipulateWidget::renderCombineWidgets({
      #   req(input$station_map_draw_new_feature)
      #   req(input$Input_Package)
      #   get_combined_plot(vls = SPIGamma::get_values(x = selected_cell()$x, y = selected_cell()$y), 
      #                     package = input$Input_Package,
      #                     scale = "Monthly")
      #   
      # })
      # 
      # output$plot_smooth <- manipulateWidget::renderCombineWidgets({
      #   req(input$station_map_draw_new_feature)
      #   req(input$Input_Package)
      #   req(input$H)
      #   req(input$DaysMOVA)
      #   get_combined_smoothed_plot(vls = SPIGamma::get_values(x = selected_cell()$x, y = selected_cell()$y), 
      #                              package = input$Input_Package, 
      #                              H = input$H, 
      #                              mova = input$DaysMOVA,
      #                              var = input$Filter)
        
      # })
      
    }
  )
}
