#' Calculate the mean value per time point
#'
#' @param data A data frame containing the time points and values.
#' @param time_col Name of the column containing the time points.
#' @param val_col Name of the column containing the values to summarize.
#'
#' @return A data frame
#' @export
#'
#' @examples
mean_val_per_timepoint = function(data, time_col, val_col) {
  mean_data = data %>%
    dplyr::mutate(cleaned_val = ifelse(get(val_col) == 0, NA, get(val_col))) %>%
    dplyr::select(-val_col) %>%
    dplyr::group_by(get(time_col)) %>%
    dplyr::summarize(mean_val = mean(cleaned_val, na.rm = T)) %>%
    dplyr::mutate(mean_val = ifelse(is.nan(mean_val), 0, mean_val)) %>%
    purrr::set_names(time_col, paste0('Mean_', val_col))

  return(mean_data)
}
