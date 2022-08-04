#' MAFs for 21 genes and 5 time points
#'
#' A dataset containing 5 longitudinal sampling time points and the associated MAF values for 21 genes.
#'
#' @format A data frame with 101 rows and 3 variables:
#' \describe{
#'   \item{Gene}{gene symbol}
#'   \item{time_from_baseline}{time passed from first sampling time point, in days}
#'   \item{MAF}{mutant allele frequency}
#' }
"mafs"

#' Time of clinical events
#'
#' A dataset containing the type and timing of clinical events.
#'
#' @format A data frame with 2 rows and 2 variables:
#' \describe{
#'   \item{Event}{type of event, ie. Progression or Death}
#'   \item{Days_from_baseline}{time passed from first sampling time point, in days}
#' }
"clinical_events"

#' Time of CT scans
#'
#' A dataset containing the times of performed CT scans.
#'
#' @format A data frame with 3 rows and 2 variables:
#' \describe{
#'   \item{Scan_num}{order of the scan}
#'   \item{Days_from_baseline}{time passed from first sampling time point, in days}
#' }
"scans"

#' Treatment data
#'
#' A dataset containing the time frames of treatment and the drugs used.
#'
#' @format A data frame with 2 rows and 3 variables:
#' \describe{
#'   \item{Treatment_drug}{name of the drug}
#'   \item{start_from_baseline}{treatment start from first sampling time point, in days}
#'   \item{stop_from_baseline}{treatment end from first sampling time point, in days}
#' }
"treatment"
