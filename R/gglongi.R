#' Visualize longitudinal data
#'
#' @param data The `data.frame` containing the data to be used for the visualization
#' @param x_val A `string` referring to the name of the column to be visualized on the X axis.
#' @param y_val A `string` referring to the name of the column to be visualized on the Y axis.
#' @param col_val A `string` referring to the name of the column to be used to color the points on the plot.
#' @param col_legend A `boolean` value. If `TRUE`, the color legend will be displayed.
#' @param mean_line A `boolean` value. If `TRUE`, the mean values per time point will be calculated and added as a continuous line.
#' @param x_title A `string` to be used as label for the X axis.
#' @param y_title A `string` to be used as label for the X axis.
#' @param treatments A `data.frame` containing treatment information. It should have 3 columns:
#' - a column containing the name of the drug
#' - a column containing the time information for the start of treatment
#' - a column containing the time information for the end of treatment
#' @param scans A `data.frame` containing information about scan dates. It should contain a column denoting the time for each scan.
#' @param events A `data.frame` containing information on clinical events. It should contain the type of event and the associated timing for the event.
#' @param treatment_start A `string` referring to the name of the column containing the treatment start data.
#' @param treatment_end A `string` referring to the name of the column containing the treatment end data.
#' @param treatment_drug A `string` referring to the name of the column containing the treatment drug.
#' @param scan_date A `string` referring to the name of the column containing the scan date in the `scans` data frame.
#' @param event_date A `string` referring to the name of the column containing the event date in the `events` data frame.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' mafs <- data.frame(time_from_baseline = c(0, 0, 0, 27, 27),
#'                    MAF = c(0, 0.009, 0.007, 0.012, 0.032),
#'                    Gene = c('BRAF', 'BRAF', 'BRAF', 'TP53', 'TP53'))
#'
#' treatment = data.frame(Treatment_drug = c('Drug 1', 'Drug 2'),
#'                        start_from_baseline = c(0, 307),
#'                        stop_from_baseline = c(224, 538))
#'
#' scans = data.frame(Scan_num = c(1, 2, 3),
#'                    Days_from_baseline = c(12, 247, 499))
#'
#' clinical_events = data.frame(Event = c('Progression', 'Death'),
#'                              Days_from_baseline = c(273, 620))
#'
#' gglongi(mafs, x_val = "time_from_baseline", y_val = "MAF", col_val = "Gene",
#'         col_legend = T, mean_line = T, x_title = "Days from baseline sample",
#'         y_title = "Mutant allele frequency", treatments = treatment, scans = scans,
#'         events = clinical_events, treatment_start = "start_from_baseline",
#'         treatment_end = "stop_from_baseline", treatment_drug = "Treatment_drug",
#'         scan_date = "Days_from_baseline", event_date = "Days_from_baseline")
#'
gglongi = function(data, x_val, y_val, col_val, col_legend = T,
                   mean_line = T, x_title = x_val, y_title = y_val,
                   treatments = NA, scans = NA, events = NA,
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
                                       ymin = 0, ymax = max(dplyr::select(data, y_val)) * 1.2,
                                       fill = treatment_drug), alpha = 0.3)
  }

  if (!is.null(nrow(scans))) {
    p = p +
      ggplot2::geom_vline(data = scans, ggplot2::aes_string(xintercept = scan_date),
                          colour = 'black', linetype = "dashed")
  }

  if (!is.null(nrow(events))) {
    p = p +
      ggplot2::geom_vline(data = events, ggplot2::aes_string(xintercept = event_date), lwd = 2,
                          colour = ifelse(events$Event == 'Death', 'black', 'indianred4'))
  }
  return (p)

}

