#' Title
#'
#' @param data
#' @param x_val
#' @param y_val
#' @param col_val
#' @param col_legend
#' @param mean_line
#' @param x_title
#' @param y_title
#' @param treatments
#' @param scans
#' @param progression
#' @param treatment_start
#' @param treatment_end
#' @param treatment_drug
#' @param scan_date
#' @param event_date
#'
#' @return
#' @export
#'
#' @examples
gglongi = function(data, x_val, y_val, col_val, col_legend = T,
                   mean_line = T, x_title = x_val, y_title = y_val,
                   treatments = NA, scans = NA, progression = NA,
                   treatment_start, treatment_end, treatment_drug,
                   scan_date, event_date) {
  p = ggplot2::ggplot() +
    ggplot2::geom_point(data = dplyr::filter(data, get(y_val) > 0),
                        ggplot2::aes_string(x = x_val, y = y_val, color = col_val)) +
    ggplot2::geom_point(data = dplyr::filter(data, get(y_val) == 0),
                        pch = 21, color = 'black', fill = 'white',
                        ggplot2::aes_string(x = x_val, y = y_val)) +
    ggpubr::theme_pubr() +
    ggplot2::xlab(x_title) +
    ggplot2::ylab(y_title)

  if (col_legend == F) {
    p = p +
      ggplot2::theme(legend.position = 'none')
  }

  if (mean_line == T) {
    calculate_means = mean_val_per_timepoint(data, x_val, y_val)

    p = p +
      ggplot2::geom_line(data = calculate_means, ggplot2::aes_string(x = x_val, y = paste0('Mean_', y_val)))
  }

  if (!is.null(nrow(treatments))) {
    p = p +
      ggplot2::geom_rect(data = treatments, ggplot2::aes_string(xmin = treatment_start,
                                                       xmax = treatment_end,
                                       ymin = 0, ymax = 1,
                                       fill = treatment_drug), alpha = 0.3)
  }

  if (!is.null(nrow(scans))) {
    p = p +
      ggplot2::geom_vline(data = scans, ggplot2::aes_string(xintercept = scan_date),
                          colour = 'black', linetype = "dashed")
  }

  if (!is.null(nrow(progression))) {
    p = p +
      ggplot2::geom_vline(data = progression, ggplot2::aes_string(xintercept = event_date), lwd = 2,
                 colour = ifelse(progression$Event == 'Death', 'black', 'indianred4'))
  }
  return (p)

}
