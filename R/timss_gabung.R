#' Merge the student questionnaire and achievement files
#'
#' Merges the student questionnaire data (\code{bsg} file) and the student
#' achievement data (\code{bsa} file) by country code and student identifier.
#' Only columns of the achievement file that are not in the questionnaire file
#' are added.
#'
#' @param siswa Data frame of the student questionnaire.
#' @param nilai Data frame of the student achievement file.
#' @param kunci Character vector of key variables present in both data frames.
#' @return A data frame with one row per student present in both files.
#' @examples
#' siswa <- timss_contoh[, c("IDCNTRY", "IDSTUD", "BSBG04")]
#' nilai <- timss_contoh[, c("IDCNTRY", "IDSTUD", "BSSSCI01")]
#' dim(timss_gabung(siswa, nilai))
#' @export
timss_gabung <- function(siswa, nilai, kunci = c("IDCNTRY", "IDSTUD")) {
  stopifnot(is.data.frame(siswa), is.data.frame(nilai))
  if (!all(kunci %in% names(siswa)) || !all(kunci %in% names(nilai))) {
    stop("The key variables must exist in both data frames.", call. = FALSE)
  }
  tambahan <- setdiff(names(nilai), names(siswa))
  merge(siswa, nilai[, c(kunci, tambahan), drop = FALSE], by = kunci)
}
