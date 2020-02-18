#' Import Soft Shell Clam Survey data from a simple compound CSV
#' 
#' @export
#' @param filename the name of the file to import
#' @return SSCS S3 object (a list of sscs data)
#' Comprised of up to 5 items: 
#' \itemize{
#' \item{filename}
#' \item{survey, a tibble of surveydata - required}
#' \item{bins, a tibble of counts (and fractions) by size bin - optional}
#' \item{plots, a tibble of plot locations - optional}
#' \item{counts, a tibble of plot counts - optional}
#' }
import_csv_report <- function(
  filename = system.file("examples/softshell.csv", package = "softshell")){
  
  if (!file.exists(filename[1])) stop("file not found:", filename[1])
  bins   <- NULL
  counts <- NULL
  plots  <- NULL
  S <- readLines(filename, warn = FALSE)
  ix_survey <- grep("[survey]", S, fixed = TRUE)
  if (length(ix_survey) == 0) stop("file not sscs CSV format")
  ix_bins <- grep("[bins]", S, fixed = TRUE)
  ix_plots <- grep("[plots]", S, fixed = TRUE)
  ix_counts <- grep("[counts]", S, fixed = TRUE)
  index <- c(
    "survey" = ix_survey[1],
    "bins"   = ix_bins[1],
    "plots"  = ix_plots[1],
    "counts" = ix_counts[1]
  )
  index <- sort(index)
  
  # survey (aka survey)
  if (length(index) == 1){
    ix <- seq(from = index['survey'] + 1, to = length(S)) 
  } else {
    ix <- seq(from = index['survey'] + 1, to = index[[2]] - 1) 
  }
  
  ss <- strsplit(S[ix], ",", fixed = TRUE)
  names(ss) <- nmss <- sapply(ss, "[[", 1)
  survey <- list()
  nm <- 'date'
  ix <- which(nmss == nm)
  if (length(ix) > 0) survey[[nm]] <- as.Date(ss[[ix]][[2]])
  nm <- 'town'
  ix <- which(nmss == nm)
  if (length(ix) > 0) survey[[nm]] <- ss[[ix]][[2]]
  nm <- 'site'
  ix <- which(nmss == nm)
  if (length(ix) > 0) survey[[nm]] <- ss[[ix]][[2]]
  nm <- 'crew'
  ix <- which(nmss == nm)
  if (length(ix) > 0) {
    crew <-  ss[[ix]][-1]
    nc <- nchar(crew)
    survey[[nm]] <- paste(crew[nc > 0], collapse = " ")
  }
  nm <- 'lon'
  ix <- which(nmss == nm)
  if (length(ix) > 0) survey[[nm]] <- as.numeric(ss[[ix]][[2]])
  nm <- 'lat'
  ix <- which(nmss == nm)
  if (length(ix) > 0) survey[[nm]] <- as.numeric(ss[[ix]][[2]])
  nm <- 'plot_count'
  ix <- which(nmss == nm)
  if (length(ix) > 0) survey[[nm]] <- as.numeric(ss[[ix]][[2]])
  nm <- 'plot_interval'
  ix <- which(nmss == nm)
  if (length(ix) > 0) survey[[nm]] <- ss[[ix]][[2]]
  nm <- 'plot_size'
  ix <- which(nmss == nm)
  if (length(ix) > 0) survey[[nm]] <- ss[[ix]][[2]]

  if ("plots" %in% names(index)){
    ss <- strsplit(S[seq_len(3) + index["plots"]], ",", fixed = TRUE) 
    s <- do.call(cbind, ss) 
    nms <- s[1,]
    s <- s[-1,]
    colnames(s) <- nms
    plots <- dplyr::as_tibble(s) %>%
      dplyr::mutate(lon = as.numeric(lon),
                    lat = as.numeric(lat))
  }

  if ("counts" %in% names(index)){
    counts <- readr::read_csv(paste(S[seq_len(21) + index["counts"]], collapse = "\n"))
    bins <- counts_to_bins(counts)
  }

  x <- list(filename = filename,
            survey = survey,
            bins = bins,
            plots = plots,
            counts = counts)
  class(x) <- append(class(x), 'SSCS')
  x

}


#' Import a user generated single-sheet XLS softshell survey file
#' 
#' @export
#' @param filename string, the name of the file to read
#' @return list of 
#' \itemize{
#' \item{meta list of metadata}
#' \item{plots tibble of plot locations}
#' \item{counts tibble of counts at each plot}
#' \item{abund tibble of abundance (bushels per acre) by size class at each plot}
#' }
import_xls_report <- function(
  filename = system.file("examples/softshell-example.xls", package = "softshell")){
  
  z <- suppressMessages(readxl::read_excel(filename))
  m <- as.matrix(z)
  N <- nrow(z)
  x <- (z %>% dplyr::select(1))[[1]]
  ix <- sapply(c("[survey]","[plots]","[counts]"),
               function(flag) grep(flag, x, fixed = TRUE))
  isurvey <- seq(from = ix[1]+1, to = ix[2]-1, by = 1)
  meta <- as.list((z %>% dplyr::select(2))[[1]][isurvey])
  names(meta) <- as.list(x[isurvey])
  meta[['date']] <- as.Date(meta$date)
  as_num <- c("plot_interval", "plot_count")
  for (n in as_num) if (n %in% names(meta)) meta[[n]] <- as.numeric(meta[[n]])
  
  plots <- suppressMessages(readxl::read_excel(filename,
                                               range = cellranger::cell_rows(c(ix[2]+2,ix[3])),
                                               col_names = TRUE))
  plots <- t(as.matrix(plots))
  plots <- dplyr::tibble(station = rownames(plots)[-1],
                         lat = as.numeric(plots[-1,1]),
                         lon = as.numeric(plots[-1,2]))
  
  #meta[['lon']] <- mean(plots$lon, na.rm = TRUE)
  #meta[['lat']] <- mean(plots$lat, na.rm = TRUE)
  meta[['area']] <- meta$plot_interval^2 / 43500 * meta$plot_count
  
  counts <- suppressMessages(readxl::read_excel(filename,
                                               range = cellranger::cell_rows(c(ix[3]+4,N+1)),
                                               col_names = m[ix[3]+1,],
                                               col_types = "numeric"))
  vf <-  volume_factor()
  area <- meta[['area']]
  plot_counts <- meta$plot_count
  plotnames <- colnames(counts)[-1]
  abundance <- function(x, vf = NULL, area = NULL){ 
    #cat(str(x), "\n")
    x * vf$factor
  }
  abund <- counts %>%
    dplyr::mutate_at(plotnames, abundance, vf = vf, area = area)

  #summa <- rowSums(counts %>% dplyr::select(-.data$size), na.rm = TRUE)
  #abund <- vf$factor * summa / meta$plot_count
  #abund <- vf %>%
  #  dplyr::select(-.data$factor) %>%
  #  dplyr::mutate(abund = abund)

  meta$class_abundance <- rowMeans(abund %>% 
                                     dplyr::select(-size) %>%
                                     as.matrix(), na.rm = TRUE)
  names(meta$class_abundance) <- counts$size
  
  meta$abundance <- sum(rowMeans(abund %>% 
                                   dplyr::select(-size) %>%
                                   as.matrix(), na.rm = TRUE), na.rm = TRUE)
  meta$legal_abundance <- sum(rowMeans(abund %>% 
                                 dplyr::filter(.data$size >= 50) %>%
                                 dplyr::select(-size) %>%
                                 as.matrix(), na.rm = TRUE), na.rm = TRUE)
  
  list(meta = meta, plots = plots, counts = counts, abund = abund)
}

#' Import Soft Shell Clam Survey data from the "Report" sheet of an excel document
#'
#' @param filename the name of the file to import
#' @return SSCS S3 object (a list of sscs data)
#' Comprised of up to 5 items: 
#' \itemize{
#' \item{filename}
#' \item{survey, a tibble of surveydata - required}
#' \item{bins, a tibble of counts (and fractions) by size bin - optional}
#' \item{plots, a tibble of plot locations - optional}
#' \item{counts, a tibble of plot counts - optional}
#' }
#' surveydata is comprised of the following 
#' \itemize{
#'  \item{date survey date}
#'  \item{town survey town}
#'  \item{site survey site name}
#'  \item{lon  decimal degrees longitude (west is negative)}
#'  \item{lat  dcimal degrees latitude}
#'  \item{plot_size sample plot size, typically 1' x 2'}
#'  \item{plot_interval distance between sample plots}
#'  \item{plot_count number of sample plots}
#'  \item{area survey coverage in acres}
#'  \item{count count of softshell clams collected}
#'  \item{count_legal count of soft shell clams of legal harvest size}
#'  \item{countf_legal fraction of soft shell clams of legal harvest size}
#'  \item{abundance abundance of soft shell clams in sample area}
#'  \item{abundance_legal abundance of soft shell clams of legal size in sample area}
#'  \item{crop extrapolated abundance of soft shell clams at site}
#'  \item{crop_legal extrapolated abundance of soft shell clams of leagl size at site}
#'  \item{cropf_legal extrapolated fractional abundance of soft shell clams of leagl size at site}

#' }
import_dmr_report <- function(
  filename = system.file("Yarmouth-Lanes-Is-2019-09-21.xls", package = "softshell")){

	# gather the header data from reading the entire sheet as character
	ncol = 7
	skip = 2
	stupid_excel_date_fix = 2

	x = readxl::read_excel(filename,
	                       sheet = 'Report',
	                       skip = 2,
	                       col_types = rep("text", ncol),
	                       col_names = LETTERS[seq_len(ncol)])
	date_row <- grep("DATE", x$A, fixed = TRUE)
	town_row <- grep("TOWN", x$A, fixed = TRUE)
	flat_row <- grep("FLAT", x$A, fixed = TRUE)
	crew_row <- grep("CREW", x$A, fixed = TRUE)
  hdr_row  <- grep("Length", x$A, fixed = TRUE)
	survey <- list(
		date = as.Date(as.numeric(x$B[date_row]),
		               origin = as.Date("1900-01-01")) -
		       stupid_excel_date_fix,
		town = x$B[town_row],
		site = x$B[flat_row],
		crew = x$B[crew_row],
		lon  = NA_real_,
		lat  = NA_real_,
		plot_size = x$F[town_row],
		plot_interval = as.numeric(x$F[flat_row]),
		plot_count = as.numeric(x$F[crew_row]),
		area = as.numeric(x$F[date_row]),
		count = as.numeric(x$G[hdr_row + 1]),
		count_legal = as.numeric(x$G[hdr_row + 2]),
		countf_legal = as.numeric(x$G[hdr_row + 3]),
		abundance = as.numeric(x$G[hdr_row + 5]),
		abundance_legal = as.numeric(x$G[hdr_row + 6]),
		crop = as.numeric(x$G[hdr_row + 8]),
		crop_legal = as.numeric(x$G[hdr_row + 9]),
		cropf_legal = as.numeric(x$G[hdr_row + 10])
		)

	# gather the count data
	d = readxl::read_excel(filename, range = "Report!A10:C28",
		col_names = c("size", "count", "fraction"),
		col_types = c("text", "numeric", "numeric"))

  x <- list(filename = filename,
       survey = survey,
       bins = d,
       plots = NULL,
       counts = NULL)
  class(x) <- append(class(x), 'SSCS')
  x
}


#' Read a soft shell clam survey table stored in CSV format
#'
#' @param filename the name of the file to read
#' @return tibble of sscs data
read_sscs <- function(filename = sscs_path("Yarmouth-sscs.csv")){
	readr::read_csv(filename)
}
