#' Calculate the mean value per time point
#'
#' @param data The `data.frame` to be used for the calculation.
#'
#' It should contain a column with the values to be summarized, and the corresponding time points denoting when the measurement was taken.
#'
#' Zero-value entries are ignored and are completely removed from the calculation, unless every value corresponding to the time point is zero.
#'
#' @param time_col `string` Name of the column containing the time data.
#' @param val_col `string` Name of the column containing the values to summarize.
#'
#' @return A `tibble` containing the mean value for each individual time point. The data frame contains two columns - the original `time_col` column and the calculated mean value column in the format `Mean_[val_col]`.
#' @export
#'
#' @examples
#' x <- data.frame(
#'          days_from_baseline = c(0, 0, 0, 27, 27),
#'          MAF = c(0, 0.009, 0.007, 0.012, 0.032))
#'
#' mean_val_per_timepoint(x, 'days_from_baseline', 'MAF')
#'
mean_val_per_timepoint <- function(data, time_col, val_col) {
  mean_data <- data %>%
    dplyr::mutate(cleaned_val = ifelse(get(val_col) == 0, NA, get(val_col))) %>%
    dplyr::select(-val_col) %>%
    dplyr::group_by(get(time_col)) %>%
    dplyr::summarize(mean_val = mean(as.numeric(cleaned_val), na.rm = T)) %>%
    dplyr::mutate(mean_val = ifelse(is.nan(mean_val), 0, mean_val)) %>%
    purrr::set_names(time_col, paste0('Mean_', val_col))

  return(mean_data)
}
