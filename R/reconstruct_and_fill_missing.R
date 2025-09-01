library(dplyr)
library(tidyr)
library(rlang)
library(purrr) # Required for map and bind_rows (or use dplyr::bind_rows)

#' Reconstruct and Fill Missing Combinations
#'
#'
#' Reconstruct missing rows in a data frame for specific combinations of key
#' columns. It is primarily intended for multivariate time series data, where
#' for each date and for each combination of columns present on that date,
#' all values for a specified completion column observed on that date will be
#' present, and missing combinations are filled with a user-defined value.
#' The set of values used for the completion column is determined individually
#' for each date, based on the values present in the input data for that date.
#'
#' @param data A data frame of observations, including columns to be filled.
#' @param keys A character vector of column names that uniquely identify each
#'   observation (e.g., `c("date", "depth", "species")`).
#'   These columns define the combinations to check for completeness.
#' @param completion_col The name of the column within `keys` whose values
#'   should be made complete (e.g., `"depth"`). The set of values used for
#'   completion depends on the `date_col`.
#' @param date_col The name of the column within `keys` that represents the
#'   date or primary grouping (e.g., `"date"`). The set of values
#'   for `completion_col` is determined based on this column.
#' @param fill_value The value for filling missing entries in `fill_cols`.
#'   Defaults to `0`. Can be `NA`.
#' @param fill_cols A character vector of column names in `data` that should
#'   be filled with `fill_value` where combinations are missing. These columns
#'   should not be in `keys`.
#'
#' @return A data frame with missing combinations reconstructed and specified
#'   columns filled with `fill_value`, considering only completion values observed
#'   on each specific date for entities present on that date. The output is
#'   arranged by the `keys` provided in the input.
#'
#' @examples
#'
#' # --- Example Usage ---
#'
#' set.seed(234)
#' tbl <- expand.grid(
#'   date = c("2025-01-10", "2025-02-12", "2025-03-17"),
#'   depth = 1:5,
#'   species = LETTERS[1:5]) |>
#'   mutate(value = sample(0:2, n(), replace = TRUE)) |>
#'   mutate(value2 = value * 10 + sample(0:9, n(), replace = TRUE)) |>
#'   ## Filter out some data, including original 0s in 'value' and some combinations
#'   filter(value != 0) |> # Remove original 0s in 'value'
#'   filter(!(date == "2025-02-12" & species == "B")) |> # Remove a species completely for a date
#'   filter(!(date == "2025-03-17" & species == "C")) |> # Remove another species completely for a date
#'   filter(!(date == "2025-01-10" & depth > 3)) # Remove depths > 3 for a date, and any species/value combos with them
#'
#' # Data before reconstruction (shows missing rows and original values)
#' print("--- Original Data (filtered) ---")
#' print(tbl)
#'
#'
#' keys <- c("date", "depth", "species")
#' date_col <- "date"
#' completion_col <- "depth"
#'
#'
#' # --- Test Cases ---
#'
#' # 1. Fill all with NA (default)
#' print("--- Reconstructed (default fill = 0) ---")
#' reconstructed_default <- reconstruct_and_fill_missing(
#'   tbl, keys, completion_col, date_col)
#' print(reconstructed_default, n = 40)
#'
#' # Verify depths for 2025-01-10 (should be 1, 2, 3)
#' print("Depths for 2025-01-10 (default fill):")
#' reconstructed_default |> filter(date == "2025-01-10") |> distinct(depth) |> print()
#'
#'
#' # 2. Fill with 0
#' fill_cols <- "value2" # Specify which columns to fill
#' print("--- Reconstructed (fill = NA) ---")
#' reconstructed_0 <- reconstruct_and_fill_missing(
#'   tbl, keys, completion_col, date_col, fill_cols, fill_value = 0
#' )
#' print(reconstructed_0, n = 40)
#'
#' # Verify depths for 2025-01-10 (should be 1, 2, 3)
#' print("Depths for 2025-01-10 (NA fill):")
#' reconstructed_0 |> filter(date == "2025-01-10") |> distinct(depth) |> print()
#'
#' # 3. Fill with a specific value (e.g., -999)
#' fill_cols <- c("value", "value2") # Specify which columns to fill
#' print("--- Reconstructed (fill = -999) ---")
#' reconstructed_custom <- reconstruct_and_fill_missing(
#'   tbl, keys, completion_col, date_col, fill_cols, fill_value = -999
#' )
#' print(reconstructed_custom, n = 40)
#'
#'
#' @export
reconstruct_and_fill_missing <- function(data, keys, completion_col, date_col,
                                         fill_value = NA,
                                         fill_cols = setdiff(names(data), keys)
                                         ) {

  ## check arguments
  if (!is.data.frame(data))
    stop("Input 'data' must be a data frame.")
  if (!is.character(keys) || length(keys) < 2)
    stop("'keys' must be a character vector of at least length 2.")
  if (!all(keys %in% colnames(data)))
    stop("All 'keys' must be column names in 'data'.")
  if (!is.character(completion_col) || length(completion_col) != 1)
    stop("'completion_col' must be a single character string.")
  if (!(completion_col %in% keys))
    stop(paste0("The 'completion_col' ('", completion_col, "') must be one of the 'keys'."))
  if (!is.character(date_col) || length(date_col) != 1 || !(date_col %in% keys)) {
    stop("'date_col' must be a single character string and one of the 'keys'.")
  }
  if (date_col == completion_col) {
    stop("'date_col' and 'completion_col' must be different.")
  }
  if (!is.character(fill_cols) || length(fill_cols) == 0)
    stop("'fill_cols' must be a character vector of column names to fill.")
  if (!all(fill_cols %in% colnames(data))) {
    stop(paste0("All 'fill_cols' must be column names in 'data'. Missing: ",
                paste(setdiff(fill_cols, colnames(data)), collapse = ", ")))
  }
  if (any(fill_cols %in% keys)) {
    warning("Columns in 'fill_cols' are also in 'keys'. Filling key columns might lead to unexpected results.")
  }

  ## --- Setup ---
  original_keys <- keys # Capture original keys for arranging
  # The columns defining the entities within each date group (all keys except date and completion_col)
  inner_entity_cols_orig <- setdiff(original_keys, c(date_col, completion_col))

  ## Handle the case where there are no entity columns besides date and completion_col
  is_dummy_needed <- length(inner_entity_cols_orig) == 0
  dummy_col_name <- ".dummy_col" # Define dummy column name

  keys_for_processing <- original_keys
  inner_entity_cols_for_processing <- inner_entity_cols_orig

  ## Add dummy column to data and if needed
  if (is_dummy_needed) {
    data <- data |> dplyr::mutate(!!sym(dummy_col_name) := 1)
    keys_for_processing <- c(original_keys, dummy_col_name) # Add dummy to keys for join
    inner_entity_cols_for_processing <- dummy_col_name # Inner entity is the dummy column
  }

  ## --- Generate Target Grid ---
  ## Group data by the date column
  data_grouped_by_date <- data |>
    group_by(!!sym(date_col)) |>
    group_split() # returns a list of tibbles

  ## Process each date group to create its target grid
  target_grid_list <- map(data_grouped_by_date, \(.x){
    current_date_value <- unique(.x[[date_col]])

    ## Get unique values of the completion column (e.g., depths)
    ## for THIS specific date
    completion_values_for_date <- unique(.x[[completion_col]])

    ## Get unique combinations of inner entity columns (e.g., species or dummy)
    ## for THIS specific date
    inner_entities_for_date <- .x |>
      select(all_of(inner_entity_cols_for_processing)) |>
      distinct()

    ## Create the grid for this date
    ## crossing combines the unique values/combinations for this date
    tidyr::crossing(
      tibble(!!sym(date_col) := current_date_value), # current date value(s)
      tibble(!!sym(completion_col) := completion_values_for_date),
      inner_entities_for_date # Data frame of inner entity combinations
    )
  })

  ## Combine the target grids from all dates
  target_grid <- bind_rows(target_grid_list)

  # --- Join and Fill ---
  # Perform a left join with the potentially modified original data
  # The join keys are the keys_for_processing (includes dummy if added)
  reconstructed_data <- target_grid |>
    left_join(data, by = keys_for_processing) |>
    mutate(across(all_of(fill_cols), \(.x) if_else(is.na(.x), fill_value, .x))) |>
    ## arrange using the original keys
    arrange(!!!syms(original_keys)) |>
    ## Remove the dummy column if available
    select(-any_of(dummy_col_name))

  return(reconstructed_data)
}
