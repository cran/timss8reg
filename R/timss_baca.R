#' Read a TIMSS data file in R format
#'
#' Reads one TIMSS International Database file stored in R format
#' (\code{.RData}), converts all variable names to upper case and removes the
#' SPSS value labels (class \code{haven_labelled}) that otherwise stop
#' functions such as \code{\link[base]{merge}} from working.
#'
#' @param folder Folder that contains the TIMSS \code{.RData} files.
#' @param prefix File prefix, for example \code{"bsg"} (student questionnaire)
#'   or \code{"bsa"} (student achievement).
#' @param negara Three-letter country code used in the file name, for example
#'   \code{"bhr"}.
#' @param siklus Cycle suffix of the file name. Default \code{"m8"} (TIMSS 2023).
#' @return A data frame with upper-case variable names.
#' @examples
#' dir_tmp <- tempfile("timss")
#' dir.create(dir_tmp)
#' bsgxxxm8 <- timss_contoh
#' save(bsgxxxm8, file = file.path(dir_tmp, "bsgxxxm8.RData"))
#' d <- timss_baca(dir_tmp, "bsg", "xxx")
#' dim(d)
#' unlink(dir_tmp, recursive = TRUE)
#' @export
timss_baca <- function(folder, prefix, negara, siklus = "m8") {
  stopifnot(is.character(folder), length(folder) == 1L)
  pola <- paste0("^", prefix, negara, siklus, "(\\.rdata|\\.rda)?$")
  f <- list.files(folder, pattern = pola, full.names = TRUE, ignore.case = TRUE)
  if (length(f) == 0L) {
    stop("File not found: ", prefix, negara, siklus, " in ", folder, call. = FALSE)
  }
  e <- new.env()
  nm <- load(f[1L], envir = e)
  d <- as.data.frame(get(nm[1L], envir = e), stringsAsFactors = FALSE)
  names(d) <- toupper(names(d))
  d[] <- lapply(d, function(v) {
    if (inherits(v, "haven_labelled")) attributes(v) <- NULL
    v
  })
  d
}
