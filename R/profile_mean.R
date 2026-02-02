#' Vertically Weighted Mean of Concentrations in Lake Profiles
#'
#' Calculate depth-weighted or volume-weighted vertical mean values of
#' concentrations, temperature, or other variables measured at different depths
#' in water bodies. This function provides safeguards against missing data and
#' supports different layer discretization schemes.
#'
#' **Note: experimental, needs thorough testing.**
#'
#' @param depth Numeric vector of sampled depths (m). Values will be sorted
#'   automatically.
#' @param value Numeric vector of measurements corresponding to \code{depth}
#'   (e.g., concentration, temperature, oxygen). Must be the same length as
#'   \code{depth}.
#' @param zmax Numeric scalar specifying the maximum depth (m) for integration
#'   or averaging. Only measurements at depths <= \code{zmax} are included.
#' @param weight Character string specifying the weighting method:
#'   \describe{
#'     \item{\code{"depth"}}{Simple depth-weighted averaging (default)}
#'     \item{\code{"volume"}}{Volume-weighted averaging based on lake morphometry}
#'   }
#' @param scheme Character string specifying the layer discretization scheme:
#'   \describe{
#'     \item{\code{"midpoint"}}{Layer boundaries at midpoints between sampling
#'       depths (default)}
#'     \item{\code{"rectangle"}}{Layer boundaries at actual sampling depths}
#'   }
#' @param vol Hypsographic volume function (optional). Required when
#'   \code{weight = "volume"}.
#'   Should be a function that takes a vector of depths and returns corresponding
#'   volumes (m³). Ignored when \code{weight = "depth"}.
#' @param total Logical. If \code{TRUE}, returns the integrated total value over
#'   the water column instead of the weighted mean. Default is \code{FALSE}.
#'
#' @return
#' Numeric value representing either:
#' \itemize{
#'   \item The weighted mean value from surface (0 m) to \code{zmax} when
#'     \code{total = FALSE}
#'   \item The integrated total value from surface to \code{zmax} when
#'     \code{total = TRUE}
#'   \item \code{NA_real_} if input data is insufficient or invalid
#' }
#'
#' @details
#' The function automatically handles:
#' \itemize{
#'   \item **Missing values**: NA values in \code{depth} or \code{value} are removed
#'   \item **Sorting**: Depths are sorted in ascending order
#'   \item **Depth filtering**: Only depths <= \code{zmax} are used
#' }
#'
#' **Layer discretization schemes:**
#' \itemize{
#'   \item \strong{Midpoint scheme}: Creates layers with boundaries at the midpoints
#'     between consecutive sampling depths, with the first layer starting at 0 and
#'     the last extending to \code{zmax}
#'   \item \strong{Rectangle scheme}: Uses the actual sampling depths as layer
#'     boundaries, with the last layer extending to \code{zmax}
#' }
#'
#' **Weighting methods:**
#' \itemize{
#'   \item \strong{Depth weighting}: Each layer contributes proportionally to its
#'     thickness
#'   \item \strong{Volume weighting}: Each layer contributes proportionally to its
#'     volume, accounting for lake morphometry via the hypsographic function
#' }
#'
#' @author Sv.Sh.
#'
#' @examples
#' # Example data
#' depth <- c(0, 1, 3.5, 5, 7, 10, 10.5, 11.5)
#' conc <- c(3.7, 4.2, 6.1, 8.9, 7.8, 9.7, 11.4, 11.4)
#' zmax <- 12
#'
#' # Depth-weighted mean with midpoint scheme
#' profile_mean(
#'   depth, conc, zmax,
#'   weight = "depth",
#'   scheme = "midpoint"
#' )
#'
#' # Depth-weighted mean with rectangle scheme
#' profile_mean(
#'   depth, conc, zmax,
#'   weight = "depth",
#'   scheme = "rectangle"
#' )
#'
#' # Volume-weighted mean (requires hypsographic function)
#' # Define a volume function for a specific water body
#' hypsographic_volume_function <- function(z) {
#'   zz <- 12 - z  # water column height
#'   175947 * zz^2 + 2686 * zz^3  # volume in m³
#' }
#'
#' profile_mean(
#'   depth, conc, zmax,
#'   weight = "volume",
#'   scheme = "midpoint",
#'   vol = hypsographic_volume_function
#' )
#'
#' # Get integrated total instead of mean
#' profile_mean(
#'   depth, conc, zmax,
#'   weight = "depth",
#'   scheme = "midpoint",
#'   total = TRUE
#' )
#'
#' @seealso \code{\link{vertmean}} for an alternative implementation with
#'   customizable top and bottom depths
#'
#' @export
profile_mean <- function(
    depth,
    value,
    zmax,
    weight = c("depth", "volume"),
    scheme = c("midpoint", "rectangle"),
    vol = NULL,
    total = FALSE
) {
  weight <- match.arg(weight)
  scheme <- match.arg(scheme)

  # --- Safeguards
  if (length(zmax) != 1 || is.na(zmax)) return(NA_real_)

  ok <- !is.na(depth) & !is.na(value)
  depth <- depth[ok]
  value <- value[ok]
  if (length(depth) < 1) return(NA_real_)

  ord <- order(depth)
  depth <- depth[ord]
  value <- value[ord]

  idx <- depth <= zmax
  depth <- depth[idx]
  value <- value[idx]
  if (length(depth) < 1) return(NA_real_)

  # --- Layer boundaries
  if (scheme == "rectangle") {
    z <- c(depth, zmax)
    w <- diff(z)
    val <- value
  } else {
    z <- c(0, (depth[-1] + depth[-length(depth)]) / 2, zmax)
    w <- diff(z)
    val <- value
  }

  # --- Weights
  if (weight == "depth") {
    ww <- w
  } else {
    if (is.null(vol))
      stop("Volumetric weighted mean requires a 'vol' function.")
    v <- vol(z)
    ww <- abs(diff(v))
  }

  res <- sum(val * ww)
  if (total) return(res)
  res / sum(ww)
}
