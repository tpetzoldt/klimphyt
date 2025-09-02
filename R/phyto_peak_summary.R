#' Summarize Phytoplankton Peaks
#'
#' This function takes a time-series of phytoplankton data with identified peaks
#' and calculates key summary metrics for each peak, such as its start time,
#' duration, maximum value, and skewness. It offers an optional mode to use linear
#' interpolation for more precise start, end, and integrated values.
#'
#' @param phyto_series A data frame containing the phytoplankton time series.
#'   It must include columns for \code{date} and \code{bv} (biovolume).
#' @param peak_obj An object (e.g., a data frame or list) that identifies
#'   individual peaks in the time series, typically containing a \code{peakid}
#'   column.
#' @param cutoff_ratio A numeric value to define the minimum value of a peak
#' related to its maximum.
#' @param max_peak_len A numeric value to filter out and suppress
#'   abnormally long peaks.
#' @param use_interpolation A logical value. If \code{TRUE}, the function uses
#'   linear interpolation to more accurately estimate the peak's start time, end
#'   time, and integrated value. If \code{FALSE} (default), it uses a simpler,
#'   non-interpolated method based on the first and last data points above a
#'   threshold.
#'
#' @return A data frame summarizing each identified peak with columns for:
#'   \itemize{
#'     \item \code{date}: The first date of the peak.
#'     \item \code{Nr}: The unique peak ID.
#'     \item \code{Tstart}: The start time of the peak.
#'     \item \code{end}: The end time of the peak.
#'     \item \code{Ymax}: The maximum biovolume of the peak.
#'     \item \code{Dpeak}: The duration of the peak.
#'     \item \code{Skew}: A measure of the peak's skewness.
#'     \item \code{Fint}: The integrated area of the peak (only available with interpolation).
#'     \item \code{Year}: The year of the peak.
#'   }
#' @export
#'
#' @importFrom dplyr mutate group_by summarize left_join ungroup
#' @importFrom lubridate year
#' @importFrom rlang .data
#'
phyto_peak_summary <- function(phyto_series, peak_obj, cutoff_ratio = 0.1,
                               max_peak_len=365, use_interpolation = FALSE) {

  phyto_series <- phyto_series |>
    mutate(peakid = peak_obj$peakid)

  tbl_max <- phyto_series |>
    group_by(peakid) |>
    summarize(maxpeak = max(bv), tmax = date[which.max(bv)], .groups = "drop")

  peak_summary <- phyto_series |>
    left_join(tbl_max, by = "peakid") |>
    mutate(
      is_peak = bv > cutoff_ratio * maxpeak,
      tmax = as.numeric(tmax),
      t = as.numeric(date)
    )

  if (use_interpolation) {
    # Method 1: Use linear interpolation for more accurate start/end and Fint
    peak_summary <- peak_summary |>
      dplyr::filter(is_peak) |>
      group_by(peakid) |>
      mutate(threshold = cutoff_ratio * maxpeak[1]) |>
      # Use `do()` to apply calc_duration to each group
      # A better way would be using summarize and list columns
      do({
        result <- calc_peak_duration(x = .data$t, y = .data$bv, threshold = .data$threshold[1])
        data.frame(
          date = .data$date[1],
          Nr = .data$peakid[1],
          Tstart = result$start,
          end = result$end,
          Ymax = .data$maxpeak[1],
          Dpeak = result$end - result$start,
          Skew = (.data$tmax[1] - result$start) / (result$end - result$start),
          Fint = result$Fint
        )
      }) |>
      ungroup()

  } else {
    # Method 2: Use the original, simpler logic
    peak_summary <- peak_summary |>
      dplyr::filter(is_peak) |>
      group_by(peakid) |>
      summarize(
        date = date[1],
        Nr = peakid[1],
        Tstart = min(t),
        end = max(t),
        Ymax = maxpeak[1],
        Dpeak = end - Tstart,
        Skew = (tmax[1] - Tstart) / Dpeak,
        .groups = "drop"
      )
  }

  peak_summary <- peak_summary |>
    mutate(Year = year(date), Tstart = Tstart %% 365.25) |>
    dlpyr::filter(Dpeak < max_peak_len) # workaround to suppress extremely long peaks

  return(peak_summary)
}
