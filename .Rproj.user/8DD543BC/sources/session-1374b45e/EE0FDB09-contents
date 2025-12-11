#' Calculate convective thermal resistance
#'
#' @param h Convection coefficient [W/(m^2·K)]
#' @param A Heat exchange surface area [m^2]
#' @return Convective thermal resistance [K/W]
#' @export
# R_conv = 1 / (h * A)
# A: total heat exchange surface [m^2]
resistance_conv <- function(h, A) {
  if (any(!is.finite(h) | h <= 0)) stop("'h' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(A) | A <= 0)) stop("'A' must be > 0 and finite.", call. = FALSE)
  1 / (h * A)
}
