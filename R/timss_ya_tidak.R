#' Recode yes/no items to 1/0
#'
#' TIMSS codes yes/no items as 1 = yes and 2 = no. This function recodes them
#' to 1 = yes and 0 = no, which is convenient for regression.
#'
#' @param data Data frame.
#' @param variabel Character vector of numeric variables coded 1 and 2.
#' @return \code{data} with the selected variables recoded.
#' @examples
#' d <- timss_ya_tidak(timss_contoh, "BSBG05A")
#' table(d$BSBG05A)
#' @export
timss_ya_tidak <- function(data, variabel) {
  stopifnot(is.data.frame(data))
  hilang <- setdiff(variabel, names(data))
  if (length(hilang) > 0L) {
    stop("Variables not found: ", paste(hilang, collapse = ", "), call. = FALSE)
  }
  for (v in variabel) data[[v]] <- 2 - data[[v]]
  data
}
