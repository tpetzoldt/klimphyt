#' Fill Non-Detected Phytoplankton Observations with Zero
#'
#' This function ensures a complete time series for specified phytoplankton groups
#' by filling in missing observation dates with a biovolume (bv) of zero. It is
#' useful for preparing data for time-series analysis where a complete record
#' is required.
#'
#' @param phyto A data frame containing phytoplankton data. It must include
#'   \code{date}, \code{group}, and \code{bv} columns.
#' @param groups A character vector specifying the group names for which non-detected
#'   dates should be filled with zero biovolume.
#'
#' @return A data frame with all dates for the specified groups included, and
#'   \code{NA} values in the \code{bv} column replaced with zero.
#' @export
#'
#' @importFrom dplyr left_join mutate
#' @importFrom rlang .data
#'
#' @examples
#' # Create sample data with a missing observation
#' phyto_data <- data.frame(
#'   date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15")),
#'   group = c("GroupA", "GroupA", "GroupA"),
#'   bv = c(10, NA, 25)
#' )
#'
#' # Fill in the NA value with zero
#' filled_data <- phyto_fill_nonedetects(phyto_data, "GroupA")
#' print(filled_data)
#' 
#' # Create sample data for multiple groups with a missing observation for GroupB
#' phyto_data_multi <- data.frame(
#'   date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-01", "2023-01-08")),
#'   group = c("GroupA", "GroupA", "GroupB", "GroupC"),
#'   bv = c(10, 20, 5, 15)
#' )
#' 
#' # Fill non-detects for GroupB
#' filled_multi <- phyto_fill_nonedetects(phyto_data_multi, c("GroupA", "GroupB", "GroupC"))
#' print(filled_multi)
#'
phyto_fill_nonedetects <- function(phyto, groups) {
  
  # Argument checking
  stopifnot(
    "phyto must be a data frame" = is.data.frame(phyto),
    "groups must be a character vector" = is.character(groups)
  )
  required_cols <- c("date", "group", "bv")
  if (!all(required_cols %in% names(phyto))) {
    stop("Input data frame `phyto` must contain columns: ",
         paste(required_cols, collapse = ", "))
  }
  
  ## select group(s) and
  ## fill nonedetect dates for these group(s) with zero biovolume
  sampling_dates <- expand.grid(date = unique(phyto$date),
                                group = groups)
  
  filled_phyto <- dplyr::left_join(sampling_dates, phyto, by = c("date", "group")) |>
    dplyr::mutate(bv = ifelse(is.na(bv), 0, bv))
    
  return(filled_phyto)
}