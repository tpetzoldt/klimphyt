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
#'   time, and integrated value (experimental!).
#'    If \code{FALSE} (default), it uses a simpler,
#'   non-interpolated method based on the first and last data points above a
#'   threshold.
#' @param min_peakwidth If \code{use_interpolation = TRUE}, minimum witch of
#'   peaks consisting of a single value only.
#'
#' @return A data frame summarizing each identified peak with columns for:
#'   \itemize{
#'     \item \code{peakid, Nr}: The unique peak ID.
#'     \item \code{date}: The first date of the peak.
#'     \item \code{Year}: The year of the peak.
#'     \item \code{Tstart}: The start time of the peak.
#'     \item \code{Tend}: The end time of the peak.
#'     \item \code{Ymax}: The maximum biovolume of the peak.
#'     \item \code{Dpeak}: The duration of the peak.
#'     \item \code{Skew}: A measure of the peak's skewness.
#'     \item \code{Fint}: The integrated area of the peak (only available with interpolation).
#'   }
#' @export
#'
#' @importFrom lubridate year
#' @importFrom rlang .data
#'
phyto_peak_summary <- function(phyto_series, peak_obj, cutoff_ratio = 0.1,
                               max_peak_len=365, use_interpolation = FALSE,
                               min_peakwidth = 3) {

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
    peak_summary <- peak_summary |>
      dplyr::filter(is_peak) |>
      group_by(peakid) |>
      # Use `summarize` with a list-column to capture the results
      summarize(
        date = date[1],
        Nr = peakid[1],
        Ymax = maxpeak[1],
        tmax = tmax[1],
        # Apply the helper function to each group
        peak_data = list(calc_peak_data(
          df = pick(t, bv, maxpeak, tmax),
          cutoff_ratio = cutoff_ratio,
          min_peakwidth = min_peakwidth
        )),
        .groups = "drop"
      ) |>
      # Unnest the list-column into a wide format
      tidyr::unnest_wider(peak_data)
  } else {
    # Method 2: Use the original, simpler logic
    peak_summary <- peak_summary |>
      dplyr::filter(is_peak) |>
      group_by(peakid) |>
      summarize(
        date = date[1],
        Nr = peakid[1],
        Tstart = min(t),
        Tend = max(t),
        Ymax = maxpeak[1],
        Dpeak = Tend - Tstart,
        Skew = (tmax[1] - Tstart) / Dpeak,
        # Calculate Fint using the trapezoidal rule
        Fint = (function(y, x) {
          sum((x[-1] - x[-length(x)]) * (y[-1] + y[-length(y)])) / 2
        })(.data$bv, .data$t),
        .groups = "drop"
      )
  }

  peak_summary <- peak_summary |>
    mutate(Year = year(date), Tstart = Tstart %% 365.25) |>
    select(peakid, Nr, date, Year, Tstart, Tend, Ymax, Dpeak, Skew, Fint)  |>
    dplyr::filter(Dpeak < max_peak_len) # workaround to suppress extremely long peaks

  return(peak_summary)
}

## -----------------------------------------------------------------------------
## Internal helper to perform the peak calculation on a single peak group
## -----------------------------------------------------------------------------
calc_peak_data <- function(df, cutoff_ratio, min_peakwidth) {
  # Handles the empty group case
  if (nrow(df) == 0) {
    return(data.frame(
      Tstart = NA, Tend = NA, Dpeak = NA, Skew = NA, Fint = NA
    ))
  }

  result <- calc_peak_duration(x = df$t, y = df$bv, threshold = cutoff_ratio * df$maxpeak[1])

  # Edge case logic if peak consists of a single value
  if (result$start == result$end) {
    result$Fint <- 0
    result$start <- result$start - min_peakwidth/2
    result$end <- result$end + min_peakwidth/2
  }

  return(data.frame(
    Tstart = result$start,
    Tend = result$end,
    Dpeak = result$end - result$start,
    Skew = (df$tmax[1] - result$start) / (result$end - result$start),
    Fint = result$Fint
  ))
}
