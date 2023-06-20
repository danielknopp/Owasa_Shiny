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
    leaflet::addLegend(pal=pal_prec, values=raster::values(data[[1:2]]), title = "Precipitation [mm]") %>%
    leaflet::addLegend(pal=pal_temp, values=raster::values(data[[3:4]]), title = "Temperature [K]") %>%
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
  
  file_path <- "data/CORDEX_products/Map"
  
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
get_cordex_annual <- function(variable){
  
  product <- switch(variable,
                    "Temperature" = "temp",
                    "Precipitation" = "prec")
  
  an45 <- paste0("data/CORDEX_products/Annual/y_", product, "_rcp45__2006_2100.rds")
  an85 <- paste0("data/CORDEX_products/Annual/y_", product, "_rcp85__2006_2100.rds")

  
  an45 <- readRDS(an45)
  an85 <- readRDS(an85)
  
  an45 <- terra::rast(an45)
  an85 <- terra::rast(an85)
  
  res <- list()
  res$rcp45 <- an45
  res$rcp85 <- an85
  
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
get_cordex_annual_values <- function(variable, x, y){
  
  
  data  <- get_cordex_annual(variable)
  shp   <- data.frame(x = x, y = y)
  shp   <- terra::vect(shp, geom = c("x", "y"))
  
  vals_45   <- terra::extract(data$rcp45, shp)[-1]
  vals_85   <- terra::extract(data$rcp85, shp)[-1]
  
  result = list(RCP45 = vals_45,
                RCP85 = vals_85,
                variable = variable)
  
  return(result)
}


#' Get datasets that will be used in the plots
#'
#' @return
#' @export
#'
#' @examples
get_cordex_monthly <- function(variable, period){
  
  product <- switch(variable,
                    "Temperature" = "temp",
                    "Precipitation" = "prec")
  
  prd <- switch(period,
                   "2006-2020" = "2006_2020",
                   "2021-2040" = "2021_2040",
                   "2041-2060" = "2041_2060",
                   "2061-2080" = "2061_2080",
                   "2081-2100" = "2081_2100")
  
  an45 <- paste0("data/CORDEX_products/Mean_monthly/mean_", product, "_rcp45_", prd, ".rds")
  an85 <- paste0("data/CORDEX_products/Mean_monthly/mean_", product, "_rcp85_", prd, ".rds")
  
  an45 <- readRDS(an45)
  an85 <- readRDS(an85)
  
  an45 <- terra::rast(an45)
  an85 <- terra::rast(an85)
  
  res <- list()
  res$rcp45 <- an45
  res$rcp85 <- an85
  
  return(res)
  
}


#' Extract variables to be used in the plots
#'
#' @return
#' @export
#'
#' @examples
get_cordex_monthly_values <- function(variable, period, x, y){
  
  
  data  <- get_cordex_monthly(variable, period)
  shp   <- data.frame(x = x, y = y)
  shp   <- terra::vect(shp, geom = c("x", "y"))
  
  vals_45   <- terra::extract(data$rcp45, shp)[-1]
  vals_85   <- terra::extract(data$rcp85, shp)[-1]
  
  result = list(RCP45 = vals_45,
                RCP85 = vals_85,
                variable = variable)
  
  return(result)
}


#' Generate the seasonality and annual plots
#'
#' @return
#' @export
#'
#' @examples
get_cordex_monthly_plot <- function(vls_monthly, vls_annual){
  
  # Creating data frame for annual dates
  annual_dates <- as.Date(paste0(substr(names(vls_annual$RCP45), 2, 5), "-01-01"))
  annual45      <- data.frame(vals = as.numeric(vls_annual$RCP45))
  annual45      <- zoo::zoo(annual45, annual_dates)
  annual45      <- xts::as.xts(annual45)
  
  annual85      <- data.frame(vals = as.numeric(vls_annual$RCP85))
  annual85      <- zoo::zoo(annual85, annual_dates)
  annual85      <- xts::as.xts(annual85)
  
  annual <- cbind(annual45, annual85)
  
  # Creating data frame for monthly dates
  monthly45 <- data.frame(No = 1:length(vls_monthly$RCP45), vals = as.numeric(vls_monthly$RCP45))
  monthly85 <- data.frame(No = 1:length(vls_monthly$RCP85), vals = as.numeric(vls_monthly$RCP85))
  
  unit <-switch(vls_monthly$variable,
                "Precipitation" = "[mm]",
                "Temperature" = "[K]")
  
  monthly <- cbind(monthly45, monthly85$vals)
  
  # Generation of the annual plot
  outplot1 <- annual %>%
    setNames(c("RCP45", "RCP85")) %>%
    dygraphs::dygraph(main = paste("Annual", vls_annual$variable)) %>%
    dygraphs::dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha=0.1,
                        drawGrid = TRUE, colors=c("#D8AE5A", "#D87A5A")) %>%
    dygraphs::dyAxis("y", label = paste(vls_annual$variable, unit)) %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
    dygraphs::dyRangeSelector()
  
  # Generation of the seasonality plot
  outplot2 <- monthly %>%
    setNames(c("RCP45", "RCP85")) %>%
    dygraphs::dygraph(main = paste(vls_monthly$variable, "seasonality")) %>%
    dygraphs::dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha=0.1,
                        drawGrid = TRUE, colors=c("#D8AE5A", "#D87A5A")) %>%
    dygraphs::dyAxis("y", label = paste(vls_monthly$variable, unit)) %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
    dygraphs::dyAxis("x", valueFormatter=htmlwidgets::JS(getMonth), axisLabelFormatter=htmlwidgets::JS(getMonth))
  
  manipulateWidget::combineWidgets(ncol = 1, rowsize = c(1.4, 1),
                                   outplot1, outplot2)
  
}


