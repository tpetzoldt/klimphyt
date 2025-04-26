#' @name vertmean
#' @title Vertical Volume Weighted Mean of Matter Concentrations in Water Bodies
#'
#' @description
#' Calculate vertical mean values which respect to depths of different
#' layers or lake morphometry.
#'
#' @details
#' This function calculates the vertical volumetric weighted mean of a variable
#' (e.g., concentration, temperature) within a specified depth range of a water body.
#' It uses a provided hypsographic function (`vol`) to determine the volume of water
#' at different depths, allowing for weighting the variable by the volume of the
#' corresponding layer.
#'
#' The function first selects the data points (`depth` and `variable`) that fall within
#' the specified `top` and `bot` depths. It then defines layers for volume calculation
#' using the `top`, `bot`, and the midpoints between consecutive selected `depth` values.
#' The `vol` function is called to calculate the cumulative volume at these layer
#' boundaries. The volume of each layer is then determined from the differences in
#' cumulative volume.
#'
#' The weighted mean is calculated as the sum of (variable value * layer volume)
#' divided by the total volume of the specified layer. If `total` is set to `TRUE`,
#' the function returns the total sum (integrated value) instead of the mean.
#'
#' Argument validation is performed at the beginning of the function to ensure
#' that the inputs are of the expected types and structures, and that the
#' `top` depth is not greater than the `bot` depth. Warnings are issued if no
#' data points are found within the specified depth range or if the total volume
#' of the layer is zero.
#'
#' @param depth sorted vector of sampled depths.
#' @param variable measurements corresponding to \code{depth} (concentration,
#'   temperature, \dots).
#' @param level surface water level (above ground or above sea level (m
#'   a.s.l.), depending on bathymetric function used.
#' @param top top water level of requested layer over which to average or
#'   integrate.
#' @param bot bottom water level of requested layer over which to average or
#'   intgrate.
#' @param vol hypsographic function to be used (e.g. \code{vol.depth}). This
#'   function must accept two arguments: a depth or vector of depths, and the
#'   \code{level} argument, and it must return the cumulative volume from the
#'   surface down to the specified depth(s).
#' @param total if \code{TRUE} the total sum over the water body is
#'   returned (integrated value), instead of the volumetric mean.
#'
#' @return
#' Volumetric average respectively total value (for total =\code{TRUE}) for a
#' given quantity (concentration, energy, temperature) in the requested
#' layer between depths \code{top} and \code{bottom}. Returns `NaN` if no data
#' points are within the specified depth range or if the total volume is zero.
#'
#' @author Thomas Petzoldt
#'
#' @examples
#'
#' ## define a bathymetric formula for a given lake or basin
#' ## z:     water depth  (m below surface)
#' ## zz:    water column (m above ground)
#' ## level: total water depth (m above ground or above reference level)
#' weight.vol <- function(z, level) {
#'   zz  <- level - z
#'   if (any(zz < 0)) stop("depth > maximum depth")
#'   vol <- 175947 * zz^2 + 2686 * zz^3 # m^3
#' }
#'
#' ## area is first derivative (not directly used in vertmean, but related)
#' area <- function(z, level) {
#'   zz  <- level - z
#'   A   <-   0.5 * 175947 * zz + 1/3 * 2686 * zz^2 # m^2
#' }
#'
#' ## dummy formula for depth-weighted averaging
#' ## (water column, instead of bathymetric curve)
#'
#' weight.column <- function(z, level) {z}
#'
#' ## Plot of lake volume (bathymetric curve)
#' par(mfrow = c(1, 2))
#' z <- 0:12
#' V <- weight.vol(z, 12)
#' plot(V, z, type = "l", ylim = c(12, 0), xlab = "Volume (m3)",
#'   ylab = "Depth (m)")
#' polygon(c(V, 0), c(z, 0), col = "cyan")
#'
#' ## Test Data
#' level <- 12
#' depth <- c(0, 1, 3.5, 5, 7, 10, 10.5, 11.5)
#' pconc <- c(3.7, 4.2, 6.1, 8.9, 7.8, 9.7, 11.4, 11.4)
#'
#' ## Plot test data
#' plot(pconc, depth, xlim=range(c(0, pconc)), ylim=c(12,0), type="n",
#'   xlab="P concentration (mu g / L)", ylab="Depth (m)")
#' segments(rep(0, 13), depth, pconc, depth, lwd=3)
#'
#' ## simple means
#' m <- mean(pconc[depth <= 4])
#' lines(c(m, m), c(0, 4), col="blue", lwd=2)
#'
#' m <- mean(pconc[depth >= 4])
#' lines(c(m, m), c(4, 12), col="blue", lwd=2)
#'
#' ## depth weighted
#' m <- vertmean(depth, pconc, level, top=0, bot=4, weight.column)
#' lines(c(m, m), c(0, 4), col="red", lwd=2)
#'
#' m <- vertmean(depth, pconc, level, top=4, bot=12, weight.column)
#' lines(c(m, m), c(4, 12), col="red", lwd=2)
#'
#' ## volume weighted
#' m <- vertmean(depth, pconc, level, top=0, bot=4, weight.vol)
#' lines(c(m, m), c(0, 4), col="green", lwd=2)
#'
#' m <- vertmean(depth, pconc, level, top=4, bot=12, weight.vol)
#' lines(c(m, m), c(4, 12), col="green", lwd=2)
#'
#' m <- vertmean(depth, pconc, level, top=4, bot=12, weight.vol)
#' lines(c(m, m), c(4, 12), col="green", lwd=2)
#'
#' legend("topright", col=c("blue", "red", "green"), lwd=2, cex=0.7,
#'   legend=c("non weighted", "depth weighted", "volume weighted"))
#'
#' ## total sum over the whole water column
#' vertmean(depth, pconc, level, top=0, bot=12, weight.vol, total=TRUE)
#'
#' @keywords misc
#' @export
vertmean <- function(depth, variable, level, top, bot, vol, total = FALSE) {

  # --- Argument Validation ---
  if (!is.numeric(depth) || !is.vector(depth)) {
    stop("Argument 'depth' must be a numeric vector.")
  }
  if (!is.numeric(variable) || !is.vector(variable)) {
    stop("Argument 'variable' must be a numeric vector.")
  }
  if (length(depth) != length(variable)) {
    stop("Arguments 'depth' and 'variable' must have the same length.")
  }
  if (!is.numeric(level) || length(level) != 1) {
    stop("Argument 'level' must be a single numeric value.")
  }
  if (!is.numeric(top) || length(top) != 1) {
    stop("Argument 'top' must be a single numeric value.")
  }
  if (!is.numeric(bot) || length(bot) != 1) {
    stop("Argument 'bot' must be a single numeric value.")
  }
  if (top > bot) {
    stop("Argument 'top' cannot be greater than 'bot'.")
  }
  if (!is.function(vol)) {
    stop("Argument 'vol' must be a function.")
  }
  if (!is.logical(total) || length(total) != 1) {
    stop("Argument 'total' must be a single logical value (TRUE or FALSE).")
  }

  # --- Function Logic ---

  idepth <- which(depth >= top & depth <= bot)

  # Check if the depth range includes any data points
  if (length(idepth) == 0) {
    warning("No data points found within the specified depth range [top, bot]. Returning NaN.")
    return(NaN)
  }

  ndepth <- c(depth[idepth])
  nvari  <- c(variable[idepth])

  # Construct the depths for volume calculation, including top and bottom
  # and the midpoints of the selected depths.
   mdepth <- unique(sort(c(top, bot,
                           if(length(ndepth) > 1) (ndepth[-1] +
                              ndepth[-length(ndepth)])/2 else numeric(0))))

   # Filter mdepth to be within the [top, bot] range and ensure top and bot are included
   mdepth <- mdepth[mdepth >= top & mdepth <= bot]
   if(!(top %in% mdepth)) mdepth <- c(top, mdepth)
   if(!(bot %in% mdepth)) mdepth <- c(mdepth, bot)
   mdepth <- unique(sort(mdepth))


  # Calculate volume at each mdepth.
  # Wrap in tryCatch to handle potential errors in vol function
  nvol <- tryCatch({
    vol(mdepth, level)
  }, error = function(e) {
    stop("Error encountered when calling the 'vol' function: ", e$message)
  })

  # Check if nvol is a numeric vector of the expected length
  if (!is.numeric(nvol) || !is.vector(nvol) || length(nvol) != length(mdepth)) {
      stop("The 'vol' function must return a numeric vector with the same length as the input depth vector.")
  }

  dvol <- abs(diff(nvol))

  # Check if the length of dvol matches the length of nvari. This should be true based on the construction of mdepth.
  if (length(dvol) != length(nvari)) {
      # This indicates an unexpected issue with the calculation of mdepth or subsequent steps.
      # This could happen if the unique() and sort() operations on mdepth removed points
      # in a way that breaks the intended correspondence with nvari.
      # Let's add a more specific check or reconsider mdepth construction if needed.
      # Given the original logic implies length(dvol) == length(nvari),
      # a mismatch here suggests a problem with how layers were implicitly expected to align with nvari.
      # Let's assume the original logic's intent for now and flag this.
      stop("Internal error: Length of volume differences (", length(dvol), ") does not match the number of variable measurements (", length(nvari), ").")
  }

  ret <- sum(nvari * dvol)


  if (total) {
    return(ret) # return sum instead of mean value
  } else {
    total_volume <- sum(dvol)
    if (total_volume == 0) {
        warning("Total volume within the specified range is zero, cannot compute weighted mean. Returning NaN.")
        return(NaN)
    }
    return(ret / total_volume)
  }
}
