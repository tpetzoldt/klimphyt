#' Plot Peak Patterns of Ecological Time Series Data
#'
#' This function visualizes ecological or time-series data as a series of filled
#' triangles, resembling a "peak pattern" plot. It replicates the functionality
#' of an old Pascal program with modern R packages like `ggplot2`.
#' The function supports both predefined and custom transformations.
#'
#' @param df A data frame containing the plot data with columns:
#'   \code{Nr}, \code{Year}, \code{Tstart}, \code{Ymax}, \code{Dpeak}, and
#'   \code{Skew}.
#' @param y_transform A character string specifying a predefined transformation
#'   or a custom function.
#'   \itemize{
#'     \item **Beginner Mode:** Use a character string: `"cbrt"` (cube root, default),
#'     `"log"` (natural logarithm), or `"sqrt"` (square root).
#'     \item **Expert Mode:** Provide a custom function that takes one numeric
#'     argument, e.g., `function(y) y^0.5`.
#'   }
#' @param n_months An integer specifying the number of months to display on the
#'   x-axis. The function will generate breaks and labels accordingly.
#' @param title A character string for the main plot title.
#' @param subtitle A character string for the plot subtitle.
#' @param xlab A character string for the x-axis label.
#' @param ylab A character string for the y-axis label.
#' @return A `ggplot2` object that can be printed, saved, or further modified.
#'
#' @references
#'
#' Petzoldt, T. (1996). Möglichkeiten zur Vorhersage von
#'    Phytoplanktonmassenentwicklungen von der statischen Betrachtungsweise zur
#'    Kurzfristprognose. Dissertation. TU Dresden,
#'    Fakultät Forst-, Geo- und Hydrowissenschaften.
#' Rolinski, S., Horn, H., Petzoldt, T., & Paul, L. (2007).
#'    Identifying cardinal dates in phytoplankton time series to enable the
#'    analysis of long-term trends. Oecologia, 153(4), 997–1008.
#'    https://doi.org/10.1007/s00442-007-0783-2
#'
#'
#' @examples
#'
#' # Create example data for demonstration
#' set.seed(42)
#' example_df <- data.frame(
#'   Nr = 1:4,
#'   Year = c(1975, 1975, 1976, 1976),
#'   Tstart = c(71.2, 154.4, 293.6, 71),
#'   Ymax = c(2.78, 0.5, 0.76, 2.65),
#'   Dpeak = c(57.5, 52.8, 84.2, 108.6),
#'   Skew = c(0.57, 0.64, 0.49, 0.37)
#' )
#'
#' # Example 1: Using a predefined transformation (cube root)
#' plot_cbrt <- peak_patterns(
#'   df = example_df,
#'   y_transform = "cbrt",
#'   n_months = 18,
#'   title = "Peak Patterns Plot",
#'   subtitle = "Using Cube Root Transformation",
#'   xlab = "Time (Months)",
#'   ylab = "Year"
#' )
#' print(plot_cbrt)
#'
#' # Example 2: Using a custom function
#' plot_expert <- peak_patterns(
#'   df = example_df,
#'   y_transform = function(y) log10(y),
#'   n_months = 18,
#'   title = "Peak Patterns Plot",
#'   subtitle = "Using Custom Log10 Transformation",
#'   xlab = "Time (Months)",
#'   ylab = "Year"
#' )
#' print(plot_expert)
#'
#'
#' @export
#'
peak_patterns <- function(df, y_transform = "cbrt", n_months = 12,
                          title = "", subtitle="", xlab="Months", ylab="Year") {

  # Determine the transformation function based on the input
  if (is.character(y_transform)) {
    # Beginner mode: Use predefined transformations
    transform_func <- switch(
      y_transform,
      "cbrt" = function(y) y^(1/3),
      "log" = function(y) log(y),
      "sqrt" = function(y) sqrt(y),
      stop("Invalid transformation specified.
           Please use 'cbrt', 'log', 'sqrt', or provide a custom function.")
    )
  } else if (is.function(y_transform)) {
    # Expert mode: Use the provided custom function
    transform_func <- y_transform
  } else {
    stop("Invalid 'y_transform' argument. Must be a character string or a function.")
  }

  # Perform the transformation and normalization.
  df_transformed <- df |>
    mutate(
      Ymax_transformed = transform_func(Ymax)
    )

  max_ymax <- max(df_transformed$Ymax_transformed)

  df_final <- df_transformed |>
    mutate(
      Ymax_rel = Ymax_transformed / max_ymax
    )

  # Define the months for the axis labels
  x_breaks <- seq(0, length.out = n_months, by = 365/12)
  x_months <- rep(c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), 3)[1:length(x_breaks)]

  # Generate the plot. `ggplot` allows us to define the plot elements.
  p <- ggplot() +
    # Draw the triangles (the `CharBox` logic)
    geom_polygon(
      data = df_final,
      aes(
        x = Tstart + Dpeak * Skew,
        y = Year + Ymax_rel, # Use the transformed, relative Ymax
        group = Nr
      ),
      fill = "white",
      color = "black"
    ) +
    geom_polygon(
      data = df_final,
      aes(
        x = Tstart,
        y = Year,
        group = Nr
      ),
      fill = "white",
      color = "black"
    ) +
    geom_polygon(
      data = df_final,
      aes(
        x = Tstart + Dpeak,
        y = Year,
        group = Nr
      ),
      fill = "white",
      color = "black"
    ) +
    # The logic is for 3 distinct points. We'll use a single geom_polygon call
    # with the correct vertices defined.
    geom_polygon(
      data = data.frame(
        x = c(df_final$Tstart, df_final$Tstart + df_final$Dpeak * df_final$Skew,
              df_final$Tstart + df_final$Dpeak),
        y = c(df_final$Year, df_final$Year - df_final$Ymax_rel, df_final$Year),
        id = rep(df_final$Nr, 3)
      ),
      aes(x = x, y = y, group = id),
      fill = "cyan",
      color = "black"
    ) +

    # Draw the horizontal grid lines for years
    geom_hline(yintercept = unique(df$Year), color = "gray", linetype = "dotted") +

    # Draw the vertical grid lines for the months
    geom_vline(xintercept = x_breaks, color = "gray", linetype = "dotted") +

    # Set the scales and labels
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +

    # Invert the y-axis
    scale_y_reverse(breaks = 1975:1989) +

    # Set the x-axis breaks and labels for the months
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_months, # Repeat "J" for the last month of the previous year
      sec.axis = sec_axis(~., breaks = x_breaks, labels = x_months)
    ) +

    # Apply a minimalist theme
    theme_minimal() #+

    # Ensure a fixed aspect ratio for accurate representation
    #coord_fixed(ratio=20)
  return(p)
}
