#' define the markers of stkeholders to the map
#'
#' @return Leaflet map with stakeholders locations
#' @export
#'
#' @examples
get_partner_map <- function() {
  
  data  <- get_partner_data()
  
  leaflet::leaflet(data) %>%
    leaflet::setView(lng = 37.262384,
                     lat = 18.370775,
                     zoom = 3) %>%
    leaflet::addProviderTiles(providers$Esri.WorldStreetMap) %>%
    leaflet::addMiniMap(
      width = 100,# adjust the width of the mini ma
      height = 100 # adjust the height of the mini map
    ) %>%
    leaflet::addMarkers(
      lng = data$longitud,
      lat = data$latitud,
      clusterOptions = leaflet::markerClusterOptions(),
      popup = paste0("<h4 style='font-size: 14px;'>", data$title, "</h4>"),  # Adjust the font size here"
      group= "pnt")
}




#' Extract the excel data
#'
#' @return
#' @export
#'
#' @examples
get_partner_data <- function() {
  
  file_path <- "data/Partner_Leaflet/Excel/Photo_description.csv"
  gallery_owasa <- read.csv2(file_path)
  # marker_with_photos choose those one which has a photo associated
  markers_with_photos <- gallery_owasa[!is.na(gallery_owasa$URL_Photo) & gallery_owasa$URL_Photo != "", ]
  
  return(markers_with_photos)
  
}

