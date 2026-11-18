#' Select usable predictor variables
#'
#' Keeps the variables whose share of missing values is below a threshold and
#' that have more than one distinct value.
#'
#' @param data Data frame.
#' @param variabel Character vector of candidate variable names.
#' @param batas_na Largest allowed proportion of missing values (exclusive).
#' @return A list with elements \code{terpilih} (kept variables),
#'   \code{dibuang} (dropped variables) and \code{proporsi_na} (proportion of
#'   missing values of every candidate found in \code{data}).
#' @examples
#' d <- timss_kode_kosong(timss_contoh, c("BSBG04", "BSBG07"))
#' timss_pilih_variabel(d, c("BSBG04", "BSBG07", "BSBS28A"))$terpilih
#' @export
timss_pilih_variabel <- function(data, variabel, batas_na = 0.25) {
  stopifnot(is.data.frame(data), is.numeric(batas_na))
  ada <- intersect(variabel, names(data))
  prop_na <- vapply(data[ada], function(v) mean(is.na(v)), numeric(1))
  ragam <- vapply(data[ada], function(v) length(unique(v[!is.na(v)])), numeric(1))
  layak <- prop_na < batas_na & ragam > 1
  list(terpilih = ada[layak],
       dibuang = setdiff(variabel, ada[layak]),
       proporsi_na = prop_na)
}
