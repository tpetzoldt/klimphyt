#' Calculate Peak Duration and Integrated Value
#'
#' This function calculates the duration and the integrated area above a specified
#' threshold of an identified peak in a time series data set. It uses linear
#' interpolation to find the start and end points of the peak and then applies numerical
#' integration (trapezoidal rule) to compute the total area under the curve above
#' the threshold.
#'
#' @param x A numeric vector of x-coordinates (e.g., time or day of year).
#' @param y A numeric vector of y-coordinates (e.g., abundance or value).
#' @param threshold A numeric value defining the minimum height for a point to be
#'   considered part of the peak.
#'
#' @return A list containing three numeric values:
#' \itemize{
#'   \item \strong{start}: The interpolated start time of the peak.
#'   \item \strong{end}: The interpolated end time of the peak.
#'   \item \strong{Fint}: The integrated area under the curve above the threshold.
#' }
#' @export
#'
#' @examples
#' # Create sample data
#' set.seed(42)
#' x_vals <- 1:20
#' y_vals <- c(rep(0, 5), runif(10, 0.5, 2.5), rep(0, 5))
#' y_vals[c(6, 10, 15)] <- c(0.8, 1.2, 0.9)
#'
#' # Define the threshold
#' threshold <- 0.1
#'
#' # Run the function
#' result <- calc_peak_duration(x_vals, y_vals, threshold)
#'
#' print(result)
calc_peak_duration <- function(x, y, threshold) {

  # Find the indices of the first and last points above the threshold
  peak_indices <- which(y > threshold)

  # Handle case where no peak exists
  if (length(peak_indices) == 0) {
    return(list(
      start = NA,
      end = NA,
      Fint = 0
    ))
  }

  I1 <- min(peak_indices)
  I2 <- max(peak_indices)

  # Linear interpolation for start and end points
  # Handles edge cases at the start of the data
  if (I1 > 1) {
    start <- lin_int(x[I1 - 1], y[I1 - 1], x[I1], y[I1], x_target = threshold)
  } else {
    start <- x[I1] # If peak starts at the first data point
  }

  # Handles edge cases at the end of the data
  if (I2 < length(x)) {
    end <- lin_int(x[I2], y[I2], x[I2 + 1], y[I2 + 1], x_target = threshold)
  } else {
    end <- x[I2] # If peak ends at the last data point
  }

  # Numerical integration (Trapezoidal rule)
  res <- 0

  # Area of the first triangle (or trapezoid) at the start of the peak
  res <- res + num_int(threshold, y[I1], x[I1] - start)

  # Loop to sum the areas of the trapezoids between the peak start and end
  for (i in I1:(I2 - 1)) {
    res <- res + num_int(y[i], y[i + 1], x[i + 1] - x[i])
  }

  # Area of the last triangle (or trapezoid) at the end of the peak
  res <- res + num_int(y[I2], threshold, end - x[I2])

  return(list(
    start = start,
    end = end,
    Fint = res
  ))
}

# Helper function for linear interpolation
# Note special handling of x1=x2 that returns the minimum y not the average
lin_int <- function(x1, y1, x2, y2, x_target) {
  # Avoid division by zero
  if (x1 == x2) {
    y_target <- y1
    if (y2 < y_target) y_target <- y2
  } else {
    y_target <- (x_target - x1) * (y2 - y1) / (x2 - x1) + y1
  }
  return(y_target)
}

# Helper function for numerical integration (Trapezoidal rule)
num_int <- function(y1, y2, dx) {
  return(dx * (y1 + y2) / 2)
}
