#' Average of the plausible values
#'
#' Adds a column with the row mean of the plausible values of one subject.
#' Averaging is a simplification; a full analysis uses every plausible value
#' separately and combines the results.
#'
#' @param data Data frame that contains the plausible value columns.
#' @param awalan Prefix of the plausible value columns, for example
#'   \code{"BSSSCI"} (science) or \code{"BSMMAT"} (mathematics).
#' @param jumlah Number of plausible values. Default 5.
#' @param nama Name of the new column.
#' @return \code{data} with one extra column.
#' @examples
#' d <- timss_nilai_rata(timss_contoh)
#' summary(d$SAINS)
#' @export
timss_nilai_rata <- function(data, awalan = "BSSSCI", jumlah = 5L, nama = "SAINS") {
  stopifnot(is.data.frame(data))
  pv <- sprintf("%s%02d", awalan, seq_len(jumlah))
  hilang <- setdiff(pv, names(data))
  if (length(hilang) > 0L) {
    stop("Plausible value columns not found: ", paste(hilang, collapse = ", "), call. = FALSE)
  }
  data[[nama]] <- rowMeans(data[, pv, drop = FALSE], na.rm = TRUE)
  data
}
