#' Compute crop per size class
#' 
#' @export 
#' @param x SSCS class object
#' @param vf table of volume conversion factors
#' @return numeric vector of crop available per bin
compute_crop_by_class <- function(x, vf = volume_factor()){
  
  x$bins$counts/x$survey$plot_count * vf$factor
}


#' Convert a table of counts (by plot) to size bins
#' 
#' @export
#' @param x table of counts
#' @param drop_spat logical, if TRUE drop the 0-10 spat class
#' @return a table of bin size, counts and fractions
counts_to_bins <- function(x, drop_spat = TRUE){
  
  if (drop_spat) x <- x %>% 
      dplyr::filter(size > 0)
  
  m <- rowSums(as.matrix(x %>% dplyr::select(-size)), na.rm = TRUE)
  
  x %>% dplyr::select(size) %>%
    dplyr::mutate(counts = m, 
                  fraction = m/sum(m))
}




#' Generate a list of soft shell clam size classes
#'
#' @export
#' @param prefix the characters with which to optionally prefix the classes
#' @param sep separator to between prefi and class name
#' @return character vector of class names
sscs_classes <- function(prefix = "", sep = "_"){
	sc = c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
			"45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
			"80-84", "85-89", "90-94", "95-99", "100+")
	if (nchar(prefix) > 0) sc = paste(prefix, sc, sep = sep)
	sc
}


#' Retrieve the units for a given variable (if known)
#'
#' @export
#' @param x character, the name of the variable
#' @return the units for the specified variable
sscs_units <- function(x = "crop_legal"){

    if (grepl("n_", x, fixed = TRUE)){
        u = 'counts'
    } else if (grepl("f_", x, fixed = TRUE)){
        u = 'fraction'
    } else {
        u = switch(x,
        lon = 'decimal degrees',
        lat = 'decimal degrees',
        plot_interval = 'feet',
        plot_count = 'count',
        acres = 'acres',
        count = 'count',
        count_legal = 'count',
        countf_legal = 'fraction',
        abundance = 'volume per area',
        abundance_legal = 'volume per area',
        crop = 'volume per area',
        crop_legal = 'volume per area',
        cropf_legal = 'fraction',
        "")
    }

}


#' Gather the stats for a particular SSCS record(s)
#'
#' @export
#' @param x tibble of sscs records
#' @param n the record to gather
#' @param fields the fields to gather
#' @return a tibble of class and value suitable for plotting.
gather_sscs <- function(x, n = 1, fields = sscs_classes(prefix = "f")){

	x %>%
		dplyr::slice(n) %>%
		dplyr::select(fields) %>%
		tidyr::gather("class", "value", fields)
}
