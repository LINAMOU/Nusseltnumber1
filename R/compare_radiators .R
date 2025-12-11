#' Compare multiple radiator designs
#'
#' @param radiators Data frame with columns A (surface), Dh (hydraulic diameter), label
#' @param Pr Prandtl number
#' @param k Thermal conductivity [W/(m·K)]
#' @param Re_vec Vector of Reynolds numbers (optional)
#' @param v_vec Vector of air velocities (optional)
#' @param nu Kinematic viscosity [m^2/s] (needed if v_vec is used)
#' @param L_char Characteristic length [m] (needed if v_vec is used)
#' @param correlation Correlation to use ("dittus" or "gnielinski")
#' @param heating TRUE if wall is heated
#' @return List of results per radiator
#' @export
# Compare multiple radiators (A and Dh) as a function of Re or v
# radiators: data.frame with columns A, Dh, label
# Provide either Re_vec, or v_vec + nu + L_char to compute Re = v * L_char / nu
compare_radiators <- function(radiators, Pr, k,
                              Re_vec = NULL, v_vec = NULL, nu = NULL, L_char = NULL,
                              correlation = c("dittus", "gnielinski"), heating = TRUE) {
  correlation <- match.arg(correlation)

  if (!all(c("A", "Dh", "label") %in% names(radiators))) {
    stop("radiators must contain columns: A, Dh, label", call. = FALSE)
  }
  if (any(!is.finite(radiators$A)  | radiators$A  <= 0)) stop("'A' must be > 0 and finite.",  call. = FALSE)
  if (any(!is.finite(radiators$Dh) | radiators$Dh <= 0)) stop("'Dh' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(Pr) | Pr <= 0)) stop("'Pr' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(k)  | k  <= 0)) stop("'k' must be > 0 and finite.",  call. = FALSE)

  # Determine Re
  if (is.null(Re_vec)) {
    if (is.null(v_vec) || is.null(nu) || is.null(L_char)) {
      stop("Provide either Re_vec, or v_vec, nu and L_char to compute Re.", call. = FALSE)
    }
    if (any(!is.finite(v_vec) | v_vec <= 0)) stop("'v_vec' must be > 0 and finite.", call. = FALSE)
    if (any(!is.finite(nu)    | nu    <= 0)) stop("'nu' must be > 0 and finite.", call. = FALSE)
    if (any(!is.finite(L_char)| L_char<= 0)) stop("'L_char' must be > 0 and finite.", call. = FALSE)
    Re_vec <- v_vec * L_char / nu
  }
  if (any(!is.finite(Re_vec) | Re_vec <= 0)) stop("'Re_vec' must be > 0 and finite.", call. = FALSE)

  # Calculations per radiator
  results <- lapply(seq_len(nrow(radiators)), function(i) {
    A  <- radiators$A[i]
    Dh <- radiators$Dh[i]
    lab <- radiators$label[i]

    Nu <- switch(correlation,
                 dittus     = nusselt_dittus_boelter(Re_vec, Pr, heating = heating),
                 gnielinski = nusselt_gnielinski(Re_vec, Pr))
    h  <- h_conv(Nu, k, Dh)
    R  <- resistance_conv(h, A)
    list(label = lab, Re = Re_vec, h = h, R = R)
  })

  # Plot h vs Re
  cols <- grDevices::rainbow(nrow(radiators))
  matplot(Re_vec, do.call(cbind, lapply(results, function(x) x$h)),
          type = "l", lwd = 2, lty = 1, col = cols,
          xlab = "Reynolds", ylab = "h [W/(m^2·K)]",
          main = sprintf("Radiator Comparison (%s)", correlation))
  legend("topleft", legend = radiators$label, col = cols, lty = 1, lwd = 2, bty = "n")
  grid()

  invisible(results)
}
