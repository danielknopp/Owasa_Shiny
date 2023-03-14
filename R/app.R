################################################################################
##                         OWASA Portal                                       ##
##                                                                            ##
##                      Main Application                                      ##
################################################################################
## Main description:                                                          ##
##                                                                            ##
##  This file runs the OWASA Portal by combining all the modules:             ##
##     1. about this site (located in inst/extdata/about.md)                  ##
##     2. mod_map.R: Interactive map to visualise the station locations       ##
##                                                                            ##
##  Authors:                                                                  ##
##                                                                            ##
##    - Oscar M. Baez-Villanueva                                              ##
##    - Juan Miguel Viquez                                                    ##
##                                                                            ##
################################################################################

#' Run the Nile Research Portal application
#'
#' @return this functions runs the application
#' @import shiny
#' @export
#'
#' @examples
owasaApp <- function(...) {
  addResourcePath('static', system.file('static', package='OWASA'))
  
  # Get interactive map
  cordex_map  <- get_cordex_map()
  prec_map    <- get_prec_map()
  evap_map    <- get_evap_map()
  sm_map      <- get_sm_map()

  # UI side of the application
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "static/custom.css")
    ),
    navbarPage(
      theme = shinythemes::shinytheme("flatly"), collapsible = TRUE,
      HTML('<a href="#">OWASA</a>'), id = "nav",
      windowTitle = "OWASA",
      # About this site panel
      tabPanel("About this site",
               includeMarkdown(system.file('extdata/about.md', package='OWASA'))),

      # CORDEX panel
      tabPanel("CORDEX projections", id = "Cordex", value = "Cordex",
               div(class="outer", mod_cordex_ui("cordex_map"))),
      
      # Precipitation panel
      tabPanel("Precipitation", id = "P_Map", value = "P_Map",
               div(class="outer", mod_prec_ui("prec_map"))),
      
      # Evaporation panel
      tabPanel("Evaporation", id = "E_Map", value = "E_Map",
               div(class="outer", mod_evap_ui("evap_map"))),
      
      # Soil moisture panel
      tabPanel("Soil moisture", id = "SM_Map", value = "SM_Map",
               div(class="outer", mod_sm_ui("sm_map")))
    )
  )

  # Server side of the application
  server <- function(input, output, session) {
    mod_cordex_server("cordex_map", cordex_map)
    mod_prec_server("prec_map", prec_map)
    mod_evap_server("evap_map", evap_map)
    mod_sm_server("sm_map", sm_map)

  }

  # Function to run the application
  shinyApp(ui, server, ...)
}
