#' Calculate Nusselt number with Dittus–Boelter correlation
#'
#' @param Re Reynolds number
#' @param Pr Prandtl number
#' @param heating TRUE if wall is heated, FALSE if cooled
#' @return Nusselt number
#' @export
# Nu = 0.023 * Re^0.8 * Pr^n
# n = 0.4 if the wall is heated, n = 0.3 if the wall is cooled
nusselt_dittus_boelter <- function(Re, Pr, heating = TRUE) {
  if (any(!is.finite(Re) | Re <= 0)) stop("'Re' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(Pr) | Pr <= 0)) stop("'Pr' must be > 0 and finite.", call. = FALSE)
  n <- if (isTRUE(heating)) 0.4 else 0.3
  0.023 * (Re^0.8) * (Pr^n)
}
