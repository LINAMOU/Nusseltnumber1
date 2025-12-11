#' Compare Dittus–Boelter and Gnielinski correlations
#'
#' @param Re_vec Vector of Reynolds numbers
#' @param Pr Prandtl number
#' @param k Thermal conductivity [W/(m·K)]
#' @param Dh Hydraulic diameter [m]
#' @param A Heat exchange surface area [m^2]
#' @param heating TRUE if wall is heated
#' @return List with Nu and h values for both correlations
#' @export
# Compare Dittus–Boelter vs Gnielinski (Nu and h) over a range of Re
compare_corr <- function(Re_vec, Pr, k, Dh, A, heating = TRUE) {
  if (any(!is.finite(Re_vec) | Re_vec <= 0)) stop("'Re_vec' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(Pr) | Pr <= 0)) stop("'Pr' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(k)  | k  <= 0)) stop("'k' must be > 0 and finite.",  call. = FALSE)
  if (any(!is.finite(Dh) | Dh <= 0)) stop("'Dh' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(A)  | A  <= 0)) stop("'A' must be > 0 and finite.",  call. = FALSE)

  Nu_d <- nusselt_dittus_boelter(Re_vec, Pr, heating = heating)
  Nu_g <- nusselt_gnielinski(Re_vec, Pr)
  h_d  <- h_conv(Nu_d, k, Dh)
  h_g  <- h_conv(Nu_g, k, Dh)

  matplot(Re_vec, cbind(Nu_d, Nu_g), type = "l", lwd = 2, lty = 1,
          col = c("firebrick", "forestgreen"), xlab = "Reynolds", ylab = "Nusselt",
          main = "Correlation Comparison (Nu vs Re)")
  legend("topleft", legend = c("Dittus–Boelter", "Gnielinski"),
         col = c("firebrick", "forestgreen"), lty = 1, lwd = 2, bty = "n")
  grid()

  # Second plot for h
  dev.new()
  matplot(Re_vec, cbind(h_d, h_g), type = "l", lwd = 2, lty = 1,
          col = c("firebrick", "forestgreen"), xlab = "Reynolds", ylab = "h [W/(m^2·K)]",
          main = "Correlation Comparison (h vs Re)")
  legend("topleft", legend = c("Dittus–Boelter", "Gnielinski"),
         col = c("firebrick", "forestgreen"), lty = 1, lwd = 2, bty = "n")
  grid()

  invisible(list(Re = Re_vec,
                 Nu = data.frame(dittus = Nu_d, gnielinski = Nu_g),
                 h  = data.frame(dittus = h_d,  gnielinski = h_g)))
}
