#' Plot softshell abundances as a histogram
#' 
#' @method plot SSCS  
#' @export
#' @param x a SSCS object
#' @param ylim either NULL (autoselect) or a specified range
#' @param title either NULL or character.  If NULL the title is generated automatically
#' @return ggplot object
plot.SSCS <- function(x = SSCS(), 
                      ylim = NULL, 
                      title = NULL){
  
  if (is.null(ylim)) ylim <- c(0, max(pretty(x$meta$class_abundance)))
  if (is.null(title)) title <- sprintf("%s: %s", 
                                       format(x$meta$date[1], "%Y-%m-%d"),
                                       x$meta$site[1])
  subtitle <- sprintf("Area: %0.1f acres, Abund: %0.1f bu/acre, Abund legal: %0.1f bu/acre",
                      x$meta$area[1], x$meta$abundance[1], x$meta$legal_abundance[1])
  klass <- as.numeric(names(x$meta$class_abundance))
  abund <- dplyr::tibble(size = factor(klass, levels = as.character(klass)) ,
                         abundance = unname(x$meta$class_abundance),
                         legal = klass >= 50) %>%
    dplyr::group_by(.data$legal)
    
  gg <-
    ggplot2::ggplot(data = abund, 
                    mapping = ggplot2::aes(x = .data$size, 
                                           y = .data$abundance,
                                           fill = .data$legal))  +
    ggplot2::scale_y_continuous(limit = ylim) + 
    ggplot2::scale_x_discrete(labels = sscs_classes()) + 
    ggplot2::scale_fill_manual(values=c("grey75", "grey45")) + 
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(x = "Size Class (mm)", y = "Abundance (bu/acre)") +
    ggplot2::ggtitle(title,
                     subtitle = subtitle ) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90, 
                                                              hjust = 1, 
                                                              vjust = 0.5))
    
  gg
}