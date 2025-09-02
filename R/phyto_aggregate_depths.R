#' Aggregate Phytoplankton Data by Depth
#'
#' This function aggregates phytoplankton data across different depths to calculate
#' a vertical mean for each phytoplankton group on a given date. It uses
#' depth-weighted averaging, which is a standard method in aquatic ecology
#' to represent the average abundance in the water column.
#'
#' @param phyto_grouped A data frame of phytoplankton data, typically
#'   pre-processed and grouped. It must contain the columns \code{date},
#'   \code{group}, \code{depth}, and \code{bv} (biovolume).
#' @param levels A data frame with columns \code{date} and \code{level},
#'   where \code{level} represents the water level in meters above ground.
#'   If meters above sea level are given, the base level (ground) needs to
#'   be subtracted first.
#' @param top Numeric. The top depth of the water column for vertical averaging
#'   (e.g., the surface). Default is 0.
#' @param bot Character or Numeric. The bottom depth of the water column.
#'   If set to \code{"max"} (default), it uses the maximum observed depth in the
#'   data. Otherwise, a numeric value can be specified.
#' @return A data frame with the vertically averaged biovolume (\code{bv}) for
#'   each \code{date} and \code{group}.
#' @importFrom dplyr left_join group_by summarise
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @export
phyto_aggregate_depths <- function(phyto_grouped, levels, top=0, bot="max") {

  # Join the phytoplankton data with water level information
  phyto_levels <- left_join(phyto_grouped, levels, by="date")

  # Define the weighting function (a simple placeholder)
  weight.column <- function(z, level) {z}

  # if bot = "max", set bot to maximum level
  if (bot == "max") {
    bot <- max(phyto_levels$depth, na.rm=TRUE)
  }

  ret <- phyto_levels |>
    ## TODO: Check for missing data and print a warning.
    na.omit() |>
    group_by(.data$date, .data$group) |>
    summarise(bottom = min(bot, max(.data$level)),
              bv = vertmean(.data$depth, .data$bv, level = .data$level[1], top = 0, bot = bottom, weight.column),
              .groups = "drop")
  return(ret)
}
