#' Export the template
#' 
#' @export
#' @param filename syring, the filename (including path) to save the template to.
#' @return logical (invisibly) with TRUE meaning success
export_template <- function(filename = "softshell_template.xls"){
  
  ok <- file.copy(system.file("examples/softshell-template.xls", package = "softshell"),
                  filename,
                  overwrite = TRUE, 
                  recursive = TRUE)
  invisible(ok)
}


#' Export the example
#' 
#' @export
#' @param filename syring, the filename (including path) to save the example to.
#' @return logical (invisibly) with TRUE meaning success
export_example <- function(filename = "softshell_example.xls"){
  
  ok <- file.copy(system.file("examples/softshell-example.xls", package = "softshell"),
                  filename,
                  overwrite = TRUE, 
                  recursive = TRUE)
  invisible(ok)
}


#' Retrieve the volume factor by size class
#' 
#' @export
#' @return table (tibble) of size_class and vol_factor
volume_factor <- function(){
  dplyr::tibble(
    size = seq.int(10, 100, by = 5),
    factor = c(0.19, 0.56, 1.02, 3.08, 
                   3.44, 5.30, 7.72, 10.88, 
                   14.69, 19.34, 24.93, 31.44, 
                   37.07, 47.81, 57.58, 68.93, 
                   81.49, 95.53, 110.70)
  )
}

#' Plot the volume factor by size class
#' 
#' @export
#' @param x the volume factor table
#' @return ggplot2 plot object
plot_volume_factor <- function(x = volume_factor()){
  
  ggplot2::ggplot(data = x, ggplot2::aes(x = .data$size, y = .data$factor)) + 
    ggplot2::geom_point() + 
    ggplot2::labs(title = "Size class to volume fraction", 
                  x= "Size Class (mm)", 
                  y = "Scaling Factor")
  
}