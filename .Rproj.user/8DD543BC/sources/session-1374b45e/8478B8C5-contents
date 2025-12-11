#' Full calculation of Nu, h and R_conv
#'
#' @param Re Reynolds number
#' @param Pr Prandtl number
#' @param k Thermal conductivity [W/(m·K)]
#' @param Dh Hydraulic diameter [m]
#' @param A Heat exchange surface area [m^2]
#' @param correlation Correlation to use ("dittus" or "gnielinski")
#' @param heating TRUE if wall is heated
#' @param f Friction factor (optional for Gnielinski)
#' @return List with Nu, h, R_conv
#' @export
# Pipeline: returns Nu, h and R_conv for a chosen correlation
# correlation: "dittus" or "gnielinski"
calc_Nusselt <- function(Re, Pr, k, Dh, A,
                         correlation = c("dittus", "gnielinski"),
                         heating = TRUE, f = NULL) {
  correlation <- match.arg(correlation)
  if (any(!is.finite(Re) | Re <= 0)) stop("'Re' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(Pr) | Pr <= 0)) stop("'Pr' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(k)  | k  <= 0)) stop("'k' must be > 0 and finite.",  call. = FALSE)
  if (any(!is.finite(Dh) | Dh <= 0)) stop("'Dh' must be > 0 and finite.", call. = FALSE)
  if (any(!is.finite(A)  | A  <= 0)) stop("'A' must be > 0 and finite.",  call. = FALSE)

  Nu <- switch(correlation,
               dittus     = nusselt_dittus_boelter(Re, Pr, heating = heating),
               gnielinski = nusselt_gnielinski(Re, Pr, f = f))
  h  <- h_conv(Nu, k, Dh)
  R  <- resistance_conv(h, A)

  list(Nu = Nu, h = h, R_conv = R,
       input = list(Re = Re, Pr = Pr, k = k, Dh = Dh, A = A,
                    correlation = correlation, heating = heating, f = f))
}
