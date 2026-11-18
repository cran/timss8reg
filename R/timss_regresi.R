#' Multiple linear regression with standardized coefficients and VIF
#'
#' Fits an ordinary least squares regression after removing rows with missing
#' values, and returns the model together with standardized coefficients,
#' correlations of every predictor with the response and variance inflation
#' factors. Sampling weights are not used.
#'
#' @param data Data frame.
#' @param y Name of the response variable.
#' @param x Character vector with at least two predictor names.
#' @return A list with elements \code{model} (an \code{lm} object), \code{data}
#'   (the complete cases used), \code{n} (number of rows used),
#'   \code{beta_terstandar}, \code{korelasi} and \code{vif}.
#' @examples
#' d <- timss_nilai_rata(timss_contoh)
#' d <- timss_kode_kosong(d, c("BSBG04", "BSBG07"))
#' hasil <- timss_regresi(d, "SAINS", c("BSBG04", "BSBG07", "BSBS28A"))
#' summary(hasil$model)
#' hasil$vif
#' @export
timss_regresi <- function(data, y = "SAINS", x) {
  stopifnot(is.data.frame(data))
  if (length(x) < 2L) stop("Provide at least two predictors in 'x'.", call. = FALSE)
  hilang <- setdiff(c(y, x), names(data))
  if (length(hilang) > 0L) {
    stop("Variables not found: ", paste(hilang, collapse = ", "), call. = FALSE)
  }
  d <- stats::na.omit(data[, c(y, x), drop = FALSE])
  model <- stats::lm(stats::reformulate(x, response = y), data = d)
  sx <- vapply(d[x], stats::sd, numeric(1))
  beta <- stats::coef(model)[x] * sx / stats::sd(d[[y]])
  korelasi <- sort(stats::cor(d[x], d[[y]])[, 1L], decreasing = TRUE)
  vif <- vapply(x, function(v) {
    r2 <- summary(stats::lm(stats::reformulate(setdiff(x, v), response = v),
                            data = d))$r.squared
    1 / (1 - r2)
  }, numeric(1))
  list(model = model, data = d, n = nrow(d),
       beta_terstandar = beta, korelasi = korelasi, vif = vif)
}
