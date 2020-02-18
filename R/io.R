#' Write a sscs dataset
#'
#' @export
#' @param x a sscs dataset
#' @param filename the path and name of the destination file
write_sscs <- function(x, filename = "softshell.sscs"){

  catn <- function(x, name, append = TRUE, ...){
    cat(paste0(name, "="), x[[name]], "\n", sep = "", append = append, ...)
  }
  con <- file(filename, open = "wt")

  if ("survey" %in% names(x)){
    cat('[survey]\n', file = con)
    cat("date=", format(x$survey$date, '%Y-%m-%d'), "\n", sep = "", file = con, append = TRUE)
    catn(x$survey, "town", file = con)
    catn(x$survey, "site", file = con)
    cat("crew=", paste(x$survey$crew, sep = " "), "\n", sep = "", file = con, append = TRUE)
    catn(x$survey, "lon", file = con)
    catn(x$survey, "lat", file = con)
    catn(x$survey, "plot_interval", file = con)
    catn(x$survey, "plot_size", file = con)
    catn(x$survey, "plot_count", file = con)
    catn(x$survey, "area", file = con)
    catn(x$survey, "count", file = con)
    catn(x$survey, "count_legal", file = con)
    catn(x$survey, "countf_legal", file = con)
    catn(x$survey, "abundance", file = con)
    catn(x$survey, "abundance_legal", file = con)
    catn(x$survey, "crop", file = con)
    catn(x$survey, "crop_legal", file = con)
    catn(x$survey, "cropf_legal", file = con)
  }
  
  if ("bins" %in% names(x)){
    cat('[bins]\n', file = con)
    readr::write_csv(x$counts, con, col_names = TRUE, append = TRUE)
  }
  
  close(con)
  invisible(x)
}


#' Read a sscs format text file
#' 
#' @export
#' @param filename the name of the file to read
#' @return sscs class object
read_sscs <- function(
  filename = system.file("softshell.sscs", package = "softshell")){
  
  if (!file.exists(filename[1])) stop("file not found:", filename[1])
  
  S <- readLines(filename)
  ix_survey <- grep("[survey]", S, fixed = TRUE)
  if (length(ix_survey) == 0) stop("file not sscs format")
  ix_bins <- grep("[bins]", S, fixed = TRUE)
  ix_plots <- grep("[plots]", S, fixed = TRUE)
  
  if (length(ix_bins) > 0){
    ix <- seq(from = ix_survey+1, to = ix_bins[1] - 1)
  } else {
    ix <- seq(from = ix_survey+1, to = length(S))
  }
  ss <- strsplit(S[ix], "=", fixed = TRUE)
  survey <- lapply(ss, "[[", 2)
  names(survey) <- sapply(ss, "[[",1)
  nm <- names(survey)
  if ("date" %in% nm) survey$date <- as.Date(survey$date)
  nums <- c("lon", "lat",
            "plot_count",
            "area",
            "count",
            "count_legal",
            "countf_legal",
            "abundance",
            "abundance_legal",
            "crop",
            "crop_legal",
            "cropf_legal")
  for (n in nums) {
    if (n %in% nm) survey[[n]] <- as.numeric(survey[[n]])
  }
  if (length(ix_bins) > 0){
    if (length(ix_plots) > 0){
      ix <- seq(from = ix_bins + 1, to = ix_plots[1] - 1)
    } else {
      ix <- seq(from = ix_bins + 1, to = length(S))
    }
    bins <- readr::read_csv(paste(S[ix], collapse = "\n"))
  } else {
    bins <- NULL
  }
  
  x <- list(
    filename = filename,
    survey = survey,
    bins = bins,
    plots = NULL,
    counts = NULL
  )
  
  class(x) <- append(class(x), 'SSCS')
  x
  
}
