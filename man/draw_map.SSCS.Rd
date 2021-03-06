% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{draw_map.SSCS}
\alias{draw_map.SSCS}
\title{Create a leaflet map with radius-scaled circle markers}
\usage{
\method{draw_map}{SSCS}(
  x,
  scale_by = c("abundance", "identity", "legal_abundance")[2],
  provider = c("Esri.WorldImagery", "OpenStreetMap.Mapnik")[1]
)
}
\arguments{
\item{x}{list of softshell data elements}

\item{scale_by}{character name of the varible used to scale the plot symbols by radius}

\item{provider}{character the tile provder for base map}
}
\value{
leaflet object
}
\description{
Create a leaflet map with radius-scaled circle markers
}
