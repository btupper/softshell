#' Create a leaflet map with radius-scaled circle markers
#'
#' @export
#' @param x list of softshell data elements
#' @param scale_by character name of the varible used to scale the plot symbols by radius
#' @param provider character the tile provder for base map
#' @return leaflet object
map_sscs <- function(x, 
                     scale_by = c("abundance", "identity")[3], 
                     provider = c("Esri.WorldImagery", "OpenStreetMap.Mapnik")[1]){

  if (is.null(x$plots)) stop("input must have 'plots' element")
  
  if (scale_by[1] == 'identity'){
    xyz <- x$plots %>%
      dplyr::mutate(size = 1)
  } else {
    if (is.null(x$counts)) stop("input must have abund element to plot abundance")
    
    if (scale_by[1] == "fraction") {
      threshold <- 0  # dump spats
    } else {
      threshold <- 49 # dump illegals
    }
    m <- colSums(as.matrix(x$counts %>% 
                             dplyr::filter(size > threshold) %>% 
                             dplyr::select(-size)), na.rm = TRUE)
    m <- m/(sum(m, na.rm = TRUE))
    
    xyz <- x$plots %>%
      dplyr::mutate(size = m)
  }
  
  if (scale_by[1] == 'identity'){
    r <- 1
    col <- "orange"
  } else {
    r <- range(xyz$size)
    r <- unname( (xyz$size + - r[1])/(r[2]-r[1]) )
    col <- ifelse(r == 0, "black", "orange")
  }
  leaflet::leaflet(xyz) %>%
    #leaflet::addTiles() %>%
    leaflet::addProviderTiles(leaflet::providers[[provider]]) %>%
    leaflet::addCircleMarkers(lng = ~lon, lat = ~lat, 
                              radius = r * 10 + 2,
                              color = col,
                              fillColor = col,
                              opacity = 0.7)
  
}
