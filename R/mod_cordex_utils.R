#' Create the main interactive map of the Nile Basin with the station locations
#'
#' @return Leaflet map widget
#' @export
#'
#' @import leaflet
get_cordex_map <- function() {

  # data  <- import_data()
  # shape_sci  <- raster::raster(data$sci_shape_daily[[1]])
  # shape_spei <- raster::raster(data$spei_shape_daily[[1]])
  # 
  # shape <- raster::stack(shape_sci, shape_spei)
  # shape[shape > 150] <- 150 #  hack to visualise the colors
  
  # pal <- leaflet::colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), raster::getValues(shape),
  #                     na.color = "transparent")
  
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    # leaflet::addRasterImage(shape[[2]], colors = pal, opacity = 0.5, group = "SPEI") %>%
    # leaflet::addRasterImage(shape[[1]], colors = pal, opacity = 0.5, group = "SCI") %>%
    # leaflet::addLegend(pal = pal, values = raster::getValues(shape), title = "Shape parameter") %>%
    # leaflet::addLayersControl(
    #   baseGroups = c("SPEI", "SCI"),
    #   options = leaflet::layersControlOptions(collapsed = FALSE),
    # ) %>%
    htmlwidgets::onRender("function() {
                          $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"> <b> Layers </b> </label>');
                          }")  %>%
    leaflet.extras::addDrawToolbar(targetGroup = "Features",
                                                              polylineOptions=FALSE,
                                                              rectangleOptions = "Features",
                                                              markerOptions=FALSE,
                                                              circleOptions=FALSE,
                                                              position = "topleft", 
                                                              editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = 
                                                                                                                 leaflet.extras::selectedPathOptions()))
}
