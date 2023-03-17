#' Create the evaporation interactive map of the OWASA portal
#'
#' @return Leaflet map widget
#' @export
#'
#' @import leaflet
get_evap_map <- function() {
  
  data  <- get_evap_map_data()
  
  pal <- leaflet::colorNumeric(c("lightgoldenrod1", "indianred2", "firebrick"), 
                               raster::getValues(data),
                               na.color = "transparent")
  
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addRasterImage(data[[1]], colors = pal, opacity = 0.5, group = names(data)[1]) %>%
    leaflet::addRasterImage(data[[2]], colors = pal, opacity = 0.5, group = names(data)[2]) %>%
    leaflet::addRasterImage(data[[3]], colors = pal, opacity = 0.5, group = names(data)[3]) %>%
    leaflet::addLayersControl(
      baseGroups = names(data),
      options = leaflet::layersControlOptions(collapsed = FALSE),
    ) %>%
    addLegend(pal=pal, values=raster::values(data), title = "Evaporation [mm]") %>%
    # htmlwidgets::onRender("function() {
    #                       $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"> <b> E products </b> </label>');
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


#' Get datasets that will be displayed in the map
#'
#' @return
#' @export
#'
#' @examples
get_evap_map_data <- function(){
  
  file_path <- "./data/E_products/Map/"
  
  files <- list.files(file_path, full.names = TRUE)
  names <- tools::file_path_sans_ext(basename(files))
  
  res        <- raster::stack(files)
  names(res) <- names
  
  return(res)
  
}

#' Get datasets that will be used in the plots
#'
#' @return
#' @export
#'
#' @examples
get_evap_data <- function(product){
  
  an <- paste0("./data/E_products/Annual/", product, ".rds")
  mm <- paste0("./data/E_products/Mean_monthly/", product, ".rds")
  
  an <- readRDS(an)
  mm <- readRDS(mm)
  
  an <- terra::rast(an)
  mm <- terra::rast(mm)
  
  res <- list()
  res$mean_monthly <- mm
  res$annual       <- an
  
  return(res)
  
}

getMonth <- 'function(d){
    var months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
    return months[d];
  }'


#' Extract variables to be used in the plots
#'
#' @return
#' @export
#'
#' @examples
get_evap_values <- function(product, x, y){
  
  
  data  <- get_evap_data(product)
  shp   <- data.frame(x = x, y = y)
  shp   <- terra::vect(shp, geom = c("x", "y"))
  
  vals_monthly_evap  <- terra::extract(data$mean_monthly, shp)[-1]
  vals_annual_evap   <- terra::extract(data$annual, shp)[-1]
  
  result = list(mean_monthly = vals_monthly_evap,
                annual = vals_annual_evap)
  
  return(result)
}

#' Generate the seasonality and annual plots
#'
#' @return
#' @export
#'
#' @examples
get_evap_plot <- function(vls){
  
  # Creating data frame for annual dates
  annual_dates <- as.Date(paste0(names(vls$annual), "-01-01"))
  annual       <- data.frame(vals = as.numeric(vls$annual))
  annual       <- zoo::zoo(annual, annual_dates)
  annual       <- xts::as.xts(annual)
  
  # Creating data frame for annual dates
  monthly <- data.frame(No = 1:length(vls$mean_monthly), vals = as.numeric(vls$mean_monthly))
  
  # Generation of the annual plot
  outplot1 <- annual %>%
    setNames("Annual evaporation") %>%
    dygraphs::dygraph(main = "Annual evaporation") %>%
    dygraphs::dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha=0.1,
                        drawGrid = TRUE, colors="#D8AE5A") %>%
    dygraphs::dyAxis("y", label = paste("Evaporation [mm]")) %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
    dygraphs::dyRangeSelector()
  
  # Generation of the seasonality plot
  outplot2 <- monthly %>%
    setNames("Evaporation seasonality") %>%
    dygraphs::dygraph(main = "Evaporation seasonality") %>%
    dygraphs::dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha=0.1,
                        drawGrid = TRUE, colors="#D8AE5A") %>%
    dygraphs::dyAxis("y", label = paste("Evaporation [mm]")) %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
    dygraphs::dyAxis("x", valueFormatter=htmlwidgets::JS(getMonth), axisLabelFormatter=htmlwidgets::JS(getMonth))
  
  manipulateWidget::combineWidgets(ncol = 1,
                                   outplot1,
                                   outplot2)
  
}



