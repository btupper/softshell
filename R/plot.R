#' Plot softshell bins as a histogram
#' 
#' @export
#' @param x a SSCS object
#' @param title either NULL or character.  If NULL the title is generated automatically
#' @return ggplot object
plot_bins <- function(x = SSCS(), 
                      title = NULL){
  
  
  if (length(what) == 1){
    wh = tolower(what[1])
    x$bins$fraction <- x$bins$fraction * 100
    ylabel <- switch(wh,
                     "counts" = 'Counts',
                     "fraction" = "Fraction of Counts (%)",
                     "density"  = "Crop Density (bu/acre)",
                     "crop"     = "Crop (bu)")
    if (is.null(title)) title <- sprintf("%s: %s, %s", 
                                         x$survey$town[1], 
                                         x$survey$site[1], 
                                         format(x$survey$date[1], "%Y-%m-%d"))
    gg <-
      ggplot2::ggplot(x$bins) +
      ggplot2::geom_col(ggplot2::aes(x = factor(.data$size), y = .data[[wh]])) +
      ggplot2::labs(x = "Size Class (mm)", y = ylabel) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      ggplot2::scale_x_discrete(breaks = x$bins$size, labels= sscs_classes())
    
    if (wh == "fraction") gg <- gg + ggplot2::ylim(0,100)
    
    gg
  } else {
    gg <- softshell::plot_bins(x, what = what[1]) + 
      softshell::plot_bins(x, what = what[2], title = " ") + 
      patchwork::plot_layout(ncol = 2)
  }
  gg
}

#' Create a barplot of either count or count fraction by size class
#'
#' @export
#' @param x x tibble of one sscs record
#' @param what either 'count' or 'fraction'
#' @return ggplot2 object
barplot_sscs <- function(x,  what = c("counts", "fraction")[2]){
  wh = tolower(what[1])
  ylabel = switch(wh,
                  'counts' = "Counts",
                  "Fraction of Counts")
  
  y <- gather_sscs(x, fields = fields) %>%
    dplyr::mutate(value = ifelse(is.na(.data$value), 0, .data$value),
                  cum = cumsum(.data$value),
                  loc = seq(from = 10, length = length(fields), by = 5))
  
  gg <-
    ggplot2::ggplot(y) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$class, y = .data$cum), stat = 'identity', alpha = 0.4) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$class, y = .data$value), stat = 'identity') +
    ggplot2::labs(x = "Size Class", y = ylabel) +
    ggplot2::ggtitle(sprintf("%s: %s, %s", x$town[1], x$site[1], format(x$date[1], "%Y-%m-%d")),
                     subtitle = sprintf("Area: %0.1f acres, Crop: %0.1f bu,  Crop legal: %0.2f bu",
                                        x$area[1], x$crop[1], x$crop_legal[1])) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggplot2::scale_x_discrete(limits = fields, labels= sscs_classes())
  if (wh != "count") gg = gg + ggplot2::ylim(0,1)
  gg
}