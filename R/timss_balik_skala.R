#' Reverse the direction of a response scale
#'
#' TIMSS codes agreement items as 1 = agree a lot to 4 = disagree a lot and
#' frequency items in a similar decreasing order. This function reverses the
#' scale (new value = \code{maks + 1 - value}) so that a larger number means
#' more agreement or higher frequency.
#'
#' @param data Data frame.
#' @param variabel Character vector of numeric variables to reverse.
#' @param maks Highest code of the scale, for example 4 for a four-point scale.
#' @return \code{data} with the selected variables reversed.
#' @examples
#' d <- timss_balik_skala(timss_contoh, c("BSBS28A", "BSBS28C"), maks = 4)
#' head(cbind(timss_contoh$BSBS28A, d$BSBS28A))
#' @export
timss_balik_skala <- function(data, variabel, maks) {
  stopifnot(is.data.frame(data), is.numeric(maks), length(maks) == 1L)
  hilang <- setdiff(variabel, names(data))
  if (length(hilang) > 0L) {
    stop("Variables not found: ", paste(hilang, collapse = ", "), call. = FALSE)
  }
  for (v in variabel) data[[v]] <- (maks + 1) - data[[v]]
  data
}
