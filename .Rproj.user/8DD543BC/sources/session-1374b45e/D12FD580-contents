#' Plot CPU temperature vs dissipated power
#'
#' @param Q_vec Vector of dissipated power values [W]
#' @param T_amb Ambient temperature [°C]
#' @param R_conv Convective thermal resistance [K/W]
#' @param R_cond Conduction thermal resistance [K/W], default 0.1
#' @param main Plot title
#' @return Data frame with Q and T_cpu
#' @export
# Plot T_cpu as a function of Q
# Also returns the (Q, T_cpu) data frame
plot_cpu_temp <- function(Q_vec, T_amb, R_conv, R_cond = 0.1,
                          main = "CPU Temperature vs Dissipated Power") {
  if (any(!is.finite(Q_vec) | Q_vec < 0)) stop("'Q_vec' must be >= 0 and finite.", call. = FALSE)
  if (!is.finite(T_amb)) stop("'T_amb' must be finite.", call. = FALSE)
  if (any(!is.finite(R_conv) | R_conv <= 0)) stop("'R_conv' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(R_cond) | R_cond <= 0)) stop("'R_cond' must be > 0 and finite.", call. = FALSE)

  T_cpu <- cpu_temperature(Q_vec, T_amb, R_conv, R_cond)
  plot(Q_vec, T_cpu, type = "l", lwd = 2, col = "steelblue",
       xlab = "Dissipated Power Q [W]", ylab = "CPU Temperature [°C]",
       main = main)
  grid()
  invisible(data.frame(Q = Q_vec, T_cpu = T_cpu))
}
