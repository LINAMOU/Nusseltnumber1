#' Calculate convection coefficient h
#'
#' @param Nu Nusselt number
#' @param k Thermal conductivity [W/(m·K)]
#' @param Dh Hydraulic diameter [m]
#' @return Convection coefficient h [W/(m^2·K)]
#' @export
# h = Nu * k / Dh
# k: fluid thermal conductivity [W/(m·K)]
# Dh: hydraulic diameter [m]
h_conv <- function(Nu, k, Dh) {
  if (any(!is.finite(Nu) | Nu <= 0)) stop("'Nu' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(k)  | k  <= 0)) stop("'k' must be > 0 and finite.",  call. = FALSE)
  if (any(!is.finite(Dh) | Dh <= 0)) stop("'Dh' must be > 0 and finite.", call. = FALSE)
  Nu * k / Dh
}
