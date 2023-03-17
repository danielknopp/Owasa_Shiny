#' Create the main interactive map of the Nile Basin with the station locations
#'
#' @return Leaflet map widget
#' @export
#'
#' @import leaflet
get_cordex_map <- function() {

  data  <- import_cordex_map_data()
  
  pal_prec <- leaflet::colorNumeric(c("#41B6C4", "#0C2C84"), 
                               raster::getValues(data),
                               na.color = "transparent")
  
  pal_temp <- leaflet::colorNumeric(c("lightgoldenrod1", "indianred2", "firebrick"), 
                               raster::getValues(data),
                               na.color = "transparent")
  
  
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addRasterImage(data[[1]], colors = pal_prec, opacity = 0.5, group = names(data)[1]) %>%
    leaflet::addRasterImage(data[[2]], colors = pal_prec, opacity = 0.5, group = names(data)[2]) %>%
    leaflet::addRasterImage(data[[3]], colors = pal_temp, opacity = 0.5, group = names(data)[3]) %>%
    leaflet::addRasterImage(data[[4]], colors = pal_temp, opacity = 0.5, group = names(data)[4]) %>%
    leaflet::addLayersControl(
      baseGroups = names(data),
      options = leaflet::layersControlOptions(collapsed = FALSE),
    ) %>%
    addLegend(pal=pal_prec, values=raster::values(data[[1:2]]), title = "Precipitation [mm]") %>%
    addLegend(pal=pal_temp, values=raster::values(data[[3:4]]), title = "Temperature [K]") %>%
    # htmlwidgets::onRender("function() {
    #                       $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"> <b> CORDEX projections </b> </label>');
    #                       }")  %>%
    leaflet.extras::addDrawToolbar(targetGroup = "Features",
                                   polylineOptions=FALSE,
                                   rectangleOptions = "Features",
                                   markerOptions=FALSE,
                                   circleOptions=FALSE,
                                   position = "topleft", 
                                   editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = 
                                                                                      leaflet.extras::selectedPathOptions()))
}



#' Import data for CORDEX map
#'
#' @return
#' @export
#'
#' @examples
import_cordex_map_data <- function(){
  
  file_path <- "./data/CORDEX_products/Map"
  
  files <- list.files(file_path, full.names = TRUE)
  names <- tools::file_path_sans_ext(basename(files))
  
  res        <- raster::stack(files)
  names(res) <- names
  
  return(res)
}
