#' Generates S3 class 'SSCS' object form a list of metadata, plot locations, spat and counts
#'
#' @export
#' @param x a 4 element list
#' \itemize{
#'   \item{meta, a list of meta data}
#'   \item{plots, a data.frame (hopefully tibble) of plot locations}
#'   \item{spat, a data.frame (hopefully tibble) of spat counts at each plot}
#'   \item{counts, a data.frame (hopefully tibble) of counts by size class at each plot}
#' }
#' @return a SSCS class object which is a list of 5 elements.  The three inputs
#'   (with meta slightly augmented) and an additional abund element which is the 
#'   counts data frame transformed into abundance (bushels per acre).
SSCS <- function(x = import_report_xls()){
  must_have <- c("meta", "plots", "counts")
  ix <- must_have %in% names(x)
  if (!all(ix)){
    msg <- sprintf("input must have three elements: %s (missing %s)",
                   paste(must_have, collapse = ", "),
                   paste(must_have[!ix], collapse = ", "))
    stop(msg)
  }
  must_inherit <- c(meta = "list", plots = "data.frame", counts = "data.frame")
  for (n in names(must_inherit)){
    stopifnot(inherits(x[[n]], must_inherit[n]))
  }
  
  vf <-  volume_factor()
  area <- x$meta[['area']]
  plot_counts <- x$meta$plot_count
  plotnames <- colnames(x$counts)[-1]
  abundance <- function(x, vf = NULL){ 
    x * vf$factor
  }
  x$abund <- x$counts %>%
    dplyr::mutate_at(plotnames, abundance, vf = vf)
  x$meta$class_abundance <- rowSums(x$counts %>% 
                                    dplyr::select(-.data$size) %>%
                                    as.matrix(), na.rm = TRUE)
  x$meta$class_abundance <- (x$meta$class_abundance/ x$meta[['plot_count']]) * vf$factor
  names(x$meta$class_abundance) <- x$counts$size
  x$meta$abundance <- sum(x$meta$class_abundance, na.rm = TRUE)
  ix <- x$counts$size >= 50
  x$meta$legal_abundance <- sum(x$meta$class_abundance[ix], na.rm = TRUE)
  
  class(x) <- c("SSCS", class(x))
  x
}

#' @method print SSCS    
#' @export 
print.SSCS <- function(x){
  cat(sprintf("Softshell Survey data: %s, %s, %s", 
              format(x$meta$date, "%Y-%m-%d"),
              x$meta$town, x$meta$site), "\n")
  cat(sprintf("N plots: %i, Plot intervals: %i' x %i', Area: %0.1f acres", 
              x$meta$plot_count, x$meta$plot_interval[1],
              x$meta$plot_interval[2], x$meta$area), "\n")
  cat(sprintf("Abundance: %0.1f bushels per acre", 
              x$meta$abundance), "\n")
  cat(sprintf("Legally sized abundance (> 50mm), %0.1f bushels per acre", 
              x$meta$legal_abundance), "\n")
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
               area = 'acres',
               count = 'count',
               count_legal = 'count',
               countf_legal = 'fraction',
               abundance = 'bushels per acre',
               abundance_legal = 'bushels per acre',
               legal_abundaance = 'bushels per acre',
               crop = 'bushels',
               crop_legal = 'bushels',
               cropf_legal = 'fraction',
               "")
  }
  
}