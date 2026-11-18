#' Heteroscedasticity diagnostics for a regression model
#'
#' Runs the studentized Breusch-Pagan test and computes coefficient tests with
#' HC3 heteroscedasticity-consistent standard errors.
#'
#' @param model An object of class \code{lm}, for example the \code{model}
#'   element returned by \code{\link{timss_regresi}}.
#' @return A list with \code{bp} (a named vector with the test statistic,
#'   degrees of freedom and p-value) and \code{robust} (a coefficient table
#'   with HC3 standard errors).
#' @examples
#' d <- timss_nilai_rata(timss_contoh)
#' d <- timss_kode_kosong(d, c("BSBG04", "BSBG07"))
#' hasil <- timss_regresi(d, "SAINS", c("BSBG04", "BSBG07", "BSBS28A"))
#' timss_diagnostik(hasil$model)$bp
#' @export
timss_diagnostik <- function(model) {
  if (!inherits(model, "lm")) stop("'model' must be an lm object.", call. = FALSE)
  bp <- lmtest::bptest(model)
  robust <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC3"))
  list(bp = c(statistik = unname(bp$statistic),
              db = unname(bp$parameter),
              p = unname(bp$p.value)),
       robust = robust)
}
