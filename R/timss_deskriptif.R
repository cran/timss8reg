#' Descriptive statistics table
#'
#' Computes the number of valid values, mean, standard deviation, minimum,
#' maximum and percentage of missing values of numeric variables.
#'
#' @param data Data frame.
#' @param variabel Character vector of variable names. Non-numeric variables
#'   are skipped.
#' @return A data frame with one row per numeric variable.
#' @examples
#' timss_deskriptif(timss_contoh, c("BSBG04", "BSBS28A"))
#' @export
timss_deskriptif <- function(data, variabel = names(data)) {
  stopifnot(is.data.frame(data))
  variabel <- intersect(variabel, names(data))
  num <- variabel[vapply(data[variabel], is.numeric, logical(1))]
  if (length(num) == 0L) stop("No numeric variables found.", call. = FALSE)
  hasil <- lapply(num, function(v) {
    x <- data[[v]]
    data.frame(variabel = v,
               n = sum(!is.na(x)),
               rata2 = mean(x, na.rm = TRUE),
               sd = stats::sd(x, na.rm = TRUE),
               min = min(x, na.rm = TRUE),
               maks = max(x, na.rm = TRUE),
               persen_na = 100 * mean(is.na(x)),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, hasil)
}
