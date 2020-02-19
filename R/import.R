#' Import a user generated single-sheet XLS softshell survey file
#' 
#' @export
#' @param filename string, the name of the file to read
#' @param sheet character or index, the name of the sheet within the file
#'   By default read the first encountered.
#' @return list of 
#' \itemize{
#' \item{meta list of metadata}
#'   \describe{
#'     \item{date}{Date, survey date}
#'     \item{town}{character, survey town}
#'     \item{site}{character, survey site within town}
#'     \item{crew}{character, crew names}
#'     \item{plot_interval}{two element numeric, (shore-normal, along-shore)}
#'     \item{plot_size}{generally character,  1'x2'}
#'     \item{plot_count}{numeric, the number of plots sampled}
#'     \item{area}{numeric, aggregate area surveyed in acres}
#'     \item{class_abundance}{named numeric vector, of abundance by class size}
#'     \item{abundance}{numeric, total bushels per acre}
#'     \item{legal_abundance}{numeric, legally sized bushels per acre}
#'   }
#' \item{plots tibble of plot locations}
#' \item{counts tibble of counts at each plot}
#' \item{abund tibble of abundance (bushels per acre) by size class at each plot}
#' }
import_report_xls <- function(
  filename = system.file("examples/softshell-example.xls", package = "softshell"),
  sheet = 1){
  
  z <- suppressMessages(readxl::read_excel(filename, sheet = sheet))
  m <- as.matrix(z)
  N <- nrow(m)
  x <- (z %>% dplyr::select(1))[[1]]
  ix <- c("[survey]" = 1,
          sapply(c("[plots]","[counts]"),
               function(flag) grep(flag, x, fixed = TRUE))
          )
  isurvey <- seq(from = ix[1]+1, to = ix[2]-1, by = 1)
  meta <- as.list((z %>% dplyr::select(2))[[1]][isurvey])
  names(meta) <- as.list(x[isurvey])
  meta[['date']] <- as.Date(meta$date)
  if ("plot_interval" %in% names(meta)){
    p <- gsub("'", "", meta[['plot_interval']], fixed = TRUE)
    p <- strsplit(p, '[x, ]')[[1]]
    if (length(p) == 1) p <- c(p,p)
    if (length(p > 2)){
      p <- suppressWarnings(as.numeric(p))
      p <- p[!is.na(p)]
      if (length(p) == 1) {
        p <- c(p,p)
      } else {
        p <- p[1:2]
      }
    }
    meta[['plot_interval']] <- p
  } else {
    meta[['plot_interval']] <- c(50,50)
  }
  
  as_num <- c("plot_interval", "plot_count")
  for (n in as_num) if (n %in% names(meta)) meta[[n]] <- as.numeric(meta[[n]])
  
  plots <- suppressMessages(readxl::read_excel(filename,
                                               range = cellranger::cell_rows(c(ix[2]+2,ix[3])),
                                               col_names = TRUE,
                                               sheet = sheet))
  plots <- t(as.matrix(plots))
  plots <- dplyr::tibble(station = rownames(plots)[-1],
                         lat = as.numeric(plots[-1,1]),
                         lon = as.numeric(plots[-1,2]))
  
  meta[['area']] <- (meta$plot_interval[1] * meta$plot_interval[2]) / 43500 * meta$plot_count
  
  counts <- suppressMessages(readxl::read_excel(filename,
                                                range = cellranger::cell_rows(c(ix[3]+3,N+1)),
                                                col_names = m[ix[3]+1,],
                                                col_types = "numeric",
                                                sheet = sheet))
  spat <- counts %>% 
    dplyr::filter(size <= 0)
  counts <- counts %>% 
    dplyr::filter(size > 0)
  
  x <- sscs(x = list(meta = meta, plots = plots, spat = spat, counts = counts))
  x
}


