#' Aggregate Phytoplankton Data by Group
#'
#' This function aggregates phytoplankton data by `group`, ensuring that all
#' depths present for a given date are accounted for, even if a specific group
#' has a missing value at that depth. It fills these missing values with zero
#' before aggregation to avoid introducing bias in the depth-weighted mean.
#'
#' @param phyto A data frame containing phytoplankton data. The function requires
#'   the following columns:
#'   \itemize{
#'     \item \strong{date}: A column representing the date of observation.
#'     \item \strong{depth}: A column representing the sampling depth.
#'     \item \strong{group}: A column for the phytoplankton group/species name.
#'     \item \strong{bv}: A column with the biovolume or abundance value to be aggregated.
#'   }
#' @param fill_missing Logical value, if \code{TRUE} (default) missing values
#'   in the vertical profile are set to zero to avoid weighting bias.
#'   This is especially important for rarer species.
#'
#' @return A data frame with the aggregated biovolume (\code{bv}) for each
#'   \code{date}, \code{depth}, and \code{group}.
#'
#' @export
phyto_aggregate_groups <- function(phyto, fill_missing = TRUE) {

  ## Fill in potentially missing depths for a species to avoid bias in the
  ## depth-weighted mean for each date.
  ## "Missing depth" is a depth for which no value exists for a specific species
  ## but which is present for other species on the same date.
  ## This is especially important for rarer species.

  if (fill_missing) {
    phyto_completed <- phyto |>
      reconstruct_and_fill_missing(
        keys = c("date", "depth", "group"),
        completion_col = "depth",
        date_col = "date",
        fill_cols = "bv",
        fill_value = 0
      )
  } else {
    phyto_completed <- phyto
  }

  phyto_sum <- phyto_completed |>
    group_by(date, depth, group) |>
    summarise(bv = sum(bv), .groups = "drop")

  return(phyto_sum)
}
