#' Soft Shell Clam Survey Data from Maine Department of Marine Resources
#'
#
#' Maine Department of Marine Resources [DMR](https://www.maine.gov/dmr/)
#' publishes reports on soft shell clam surveys.  Methods provide access to example data
#' from [Yarmouth, Maine](https://yarmouth.me.us), as well as plotting and mapping
#' tools.
#'
#' @name softshell
#' @aliases softshell softshell-package
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @importFrom utils glob2rx
#' @importFrom stats quantile
#' @importFrom rlang .data
NULL

#' Soft Shell Clam Survey Data from Maine Department of Marine Resources [DMR](https://www.maine.gov/dmr/).
#'
#' A dataset containing the time, location, and results from quadrat based surveys.
#'
#' @format A data frame with 55 variables:
#' \describe{
#'   \item{date}{survey date}
#'   \item{town}{survey town}
#'   \item{site}{survey site name}
#'   \item{lon}{decimal degrees longitude (west is negative)}
#'   \item{lat}{decimal degrees latitude}
#'   \item{plot_size}{sample plot size, typically 1' x 2'}
#'   \item{plot_interval}{distance between sample plots}
#'   \item{plot_count}{number of sample plots}
#'   \item{area}{size of area in acres}
#'   \item{count}{count of softshell clams collected}
#'   \item{count_legal}{count of soft shell clams of legal harvest size}
#'   \item{countf_legal}{fraction of soft shell clams of legal harvest size}
#'   \item{abundance}{abundance of soft shell clams in sample area}
#'   \item{abundance_legal}{abundance of soft shell clams of legal size in sample area}
#'   \item{crop}{extrapolated abundance of soft shell clams at site}
#'   \item{crop_legal}{extrapolated abundance of soft shell clams of leagl size at site}
#'   \item{cropf_legal}{extrapolated fractional abundance of soft shell clams of leagl size at site}
#'   \item{n_10-14}{counts in the size class 10-14 mm}
#'   \item{n_15-19}{counts in the size class}
#'   \item{n_20-24}{counts in the size class}
#'   \item{n_25-29}{counts in the size class}
#'   \item{n_30-34}{counts in the size class}
#'   \item{n_35-39}{counts in the size class}
#'   \item{n_40-44}{counts in the size class}
#'   \item{n_45-49}{counts in the size class}
#'   \item{n_50-54}{counts in the size class}
#'   \item{n_55-59}{counts in the size class}
#'   \item{n_60-64}{counts in the size class}
#'   \item{n_65-69}{counts in the size class}
#'   \item{n_70-74}{counts in the size class}
#'   \item{n_75-79}{counts in the size class}
#'   \item{n_80-84}{counts in the size class}
#'   \item{n_85-89}{counts in the size class}
#'   \item{n_90-94}{counts in the size class}
#'   \item{n_95-99}{counts in the size class}
#'   \item{n_100}{counts in the size class <= 100mm}
#'   \item{f_10-14}{fraction in the size class 10-14 mm}
#'   \item{f_15-19}{fraction in the size class}
#'   \item{f_20-24}{fraction in the size class}
#'   \item{f_25-29}{fraction in the size class}
#'   \item{f_30-34}{fraction in the size class}
#'   \item{f_35-39}{fraction in the size class}
#'   \item{f_40-44}{fraction in the size class}
#'   \item{f_45-49}{fraction in the size class}
#'   \item{f_50-54}{fraction in the size class}
#'   \item{f_55-59}{fraction in the size class}
#'   \item{f_60-64}{fraction in the size class}
#'   \item{f_65-69}{fraction in the size class}
#'   \item{f_70-74}{fraction in the size class}
#'   \item{f_75-79}{fraction in the size class}
#'   \item{f_80-84}{fraction in the size class}
#'   \item{f_85-89}{fraction in the size class}
#'   \item{f_90-94}{fraction in the size class}
#'   \item{f_95-99}{fraction in the size class}
#'   \item{f_100}{fraction in the size class <= 100mm}
#' }
#'
#' @source \url{https://www.maine.gov/dmr/}
"sscs"