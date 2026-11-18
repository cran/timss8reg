#' Convert missing-value codes to NA
#'
#' Converts the selected variables to numeric and replaces the codes that TIMSS
#' uses for omitted or invalid answers (for example 9, 99, 999) by \code{NA}.
#'
#' @param data Data frame.
#' @param variabel Character vector of variable names to convert.
#' @param kode Numeric vector of codes to be treated as missing.
#' @return \code{data} with the selected variables converted.
#' @examples
#' d <- timss_kode_kosong(timss_contoh, c("BSBG04", "BSBG07"))
#' sum(is.na(d$BSBG04))
#' @export
timss_kode_kosong <- function(data, variabel, kode = c(9, 99, 999, 9999)) {
  stopifnot(is.data.frame(data))
  hilang <- setdiff(variabel, names(data))
  if (length(hilang) > 0L) {
    stop("Variables not found: ", paste(hilang, collapse = ", "), call. = FALSE)
  }
  for (v in variabel) {
    x <- suppressWarnings(as.numeric(as.character(data[[v]])))
    x[x %in% kode] <- NA_real_
    data[[v]] <- x
  }
  data
}
