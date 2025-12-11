#' Calculate Nusselt number with Gnielinski correlation
#'
#' @param Re Reynolds number
#' @param Pr Prandtl number
#' @param f Friction factor (optional). If NULL, smooth pipe approximation is used.
#' @return Nusselt number
#' @export
# Gnielinski correlation for turbulent duct flow:
# Nu = (f/8) * (Re - 1000) * Pr / [1 + 12.7 * sqrt(f/8) * (Pr^(2/3) - 1)]
# f (smooth pipe) ≈ (0.79*ln(Re) - 1.64)^(-2) if Re >= 3000
nusselt_gnielinski <- function(Re, Pr, f = NULL) {
  if (any(!is.finite(Re) | Re <= 0)) stop("'Re' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(Pr) | Pr <= 0)) stop("'Pr' must be > 0 and finite.", call. = FALSE)
  if (is.null(f)) {
    if (any(Re < 3000)) warning("Gnielinski is intended for Re >= 3000. Re < 3000 detected.")
    f <- (0.79 * log(Re) - 1.64)^(-2)
  } else {
    if (any(!is.finite(f) | f <= 0)) stop("'f' must be > 0 and finite.", call. = FALSE)
  }
  (f / 8) * (Re - 1000) * Pr / (1 + 12.7 * sqrt(f / 8) * (Pr^(2/3) - 1))
}
