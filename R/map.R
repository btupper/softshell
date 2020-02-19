#' Draw map of the provided object
#' @export
#' @param x an object to map
draw_map <- function(x) UseMethod("draw_map")

#' Draw map of the provided object
#' @method draw_map default 
#' @export
#' @param x an object to map
draw_map.default <- function(x){
  if (!inherits(x, 'SSCS')) warning("no known draw_map method for ", paste(class(x), collapse = ", "))
}

#' Create a leaflet map with radius-scaled circle markers
#'
#' @method draw_map SSCS 
#' @export
#' @param x list of softshell data elements
#' @param scale_by character name of the varible used to scale the plot symbols by radius
#' @param provider character the tile provder for base map
#' @return leaflet object
draw_map.SSCS <- function(x, 
                     scale_by = c("abundance", "identity", "legal_abundance")[2], 
                     provider = c("Esri.WorldImagery", "OpenStreetMap.Mapnik")[1]){

  threshold <- 50
  if (scale_by[1] == 'identity'){
    xyz <- x$plots %>%
      dplyr::mutate(size = 1)
  } else {
    if (grepl("legal", scale_by[1], fixed = TRUE)){
      m <- colSums(as.matrix(x$abund %>% 
                             dplyr::filter(.data$size > threshold) %>% 
                             dplyr::select(-.data$size)), na.rm = TRUE)
    } else {
      m <- colSums(as.matrix(x$abund %>% 
                             dplyr::select(-.data$size)), na.rm = TRUE)
    }
    m <- m/(sum(m, na.rm = TRUE))
    
    xyz <- x$plots %>%
      dplyr::mutate(size = m)
  }
  
  if (scale_by[1] == 'identity'){
    r <- 10
    col <- "orange"
  } else {
    bottom <- 10
    r <- c(0, max(xyz$size, na.rm = TRUE))
    r <- unname( (xyz$size - r[1])/(r[2]-r[1]) ) * 10 + bottom
    col <- ifelse(r <= bottom, "black", "orange")
  }
  
  leaflet::leaflet(xyz) %>%
    leaflet::addProviderTiles(leaflet::providers[[provider]]) %>%
    leaflet::addCircleMarkers(lng = ~lon, lat = ~lat, 
                              radius = r,
                              color = col,
                              fillColor = col,
                              opacity = 0.7)
}
