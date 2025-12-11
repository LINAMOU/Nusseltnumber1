#' Estimate CPU temperature
#'
#' @param Q Dissipated power [W]
#' @param T_amb Ambient temperature [°C]
#' @param R_conv Convective thermal resistance [K/W]
#' @param R_cond Conduction thermal resistance [K/W], default 0.1
#' @return CPU temperature [°C]
#' @export
# T_cpu = T_amb + Q * (R_cond + R_conv)
# Q: dissipated power [W]
# T_amb: ambient temperature [°C]
# R_cond: solid conduction resistance CPU->fins [K/W] (default 0.1)
cpu_temperature <- function(Q, T_amb, R_conv, R_cond = 0.1) {
  if (any(!is.finite(Q) | Q < 0)) stop("'Q' must be >= 0 and finite.", call. = FALSE)
  if (!is.finite(T_amb)) stop("'T_amb' must be finite.", call. = FALSE)
  if (any(!is.finite(R_conv) | R_conv <= 0)) stop("'R_conv' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(R_cond) | R_cond <= 0)) stop("'R_cond' must be > 0 and finite.", call. = FALSE)
  T_amb + Q * (R_cond + R_conv)
}
