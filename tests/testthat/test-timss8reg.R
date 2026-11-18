test_that("timss_baca reads a file and strips labels", {
  dir_tmp <- tempfile("timss"); dir.create(dir_tmp)
  on.exit(unlink(dir_tmp, recursive = TRUE))
  bsgxxxm8 <- timss_contoh
  names(bsgxxxm8) <- tolower(names(bsgxxxm8))
  bsgxxxm8$bsbg01 <- structure(bsgxxxm8$bsbg01, label = "sex",
                               class = c("haven_labelled", "vctrs_vctr", "integer"))
  save(bsgxxxm8, file = file.path(dir_tmp, "bsgxxxm8.RData"))
  d <- timss_baca(dir_tmp, "bsg", "xxx")
  expect_true("BSBG01" %in% names(d))
  expect_null(attributes(d$BSBG01))
  expect_error(timss_baca(dir_tmp, "bsa", "xxx"), "File not found")
})

test_that("timss_gabung merges by keys", {
  siswa <- timss_contoh[, c("IDCNTRY", "IDSTUD", "BSBG04")]
  nilai <- timss_contoh[, c("IDCNTRY", "IDSTUD", "BSSSCI01")]
  d <- timss_gabung(siswa, nilai)
  expect_equal(nrow(d), 300L)
  expect_true("BSSSCI01" %in% names(d))
  expect_error(timss_gabung(siswa, nilai, kunci = "XX"), "key variables")
})

test_that("timss_nilai_rata averages plausible values", {
  d <- timss_nilai_rata(timss_contoh)
  pv <- sprintf("BSSSCI%02d", 1:5)
  expect_equal(d$SAINS[1], mean(unlist(timss_contoh[1, pv])))
  expect_error(timss_nilai_rata(timss_contoh, awalan = "ZZZ"), "not found")
})

test_that("recoding helpers work", {
  d <- timss_kode_kosong(timss_contoh, "BSBG04")
  expect_true(anyNA(d$BSBG04))
  expect_false(any(d$BSBG04 == 9, na.rm = TRUE))
  d2 <- timss_balik_skala(timss_contoh, "BSBS28A", maks = 4)
  expect_equal(d2$BSBS28A, 5 - timss_contoh$BSBS28A)
  d3 <- timss_ya_tidak(timss_contoh, "BSBG05A")
  expect_true(all(d3$BSBG05A %in% c(0, 1)))
})

test_that("variable selection and descriptives work", {
  d <- timss_kode_kosong(timss_contoh, c("BSBG04", "BSBG07"))
  s <- timss_pilih_variabel(d, c("BSBG04", "BSBS28A", "NOPE"), batas_na = 0.01)
  expect_equal(s$terpilih, "BSBS28A")
  expect_true(all(c("BSBG04", "NOPE") %in% s$dibuang))
  ds <- timss_deskriptif(d, c("BSBG04", "BSBS28A"))
  expect_equal(nrow(ds), 2L)
  expect_gt(ds$persen_na[1], 0)
})

test_that("regression and diagnostics work", {
  d <- timss_nilai_rata(timss_contoh)
  d <- timss_kode_kosong(d, c("BSBG04", "BSBG07"))
  h <- timss_regresi(d, "SAINS", c("BSBG04", "BSBG07", "BSBS28A"))
  expect_s3_class(h$model, "lm")
  expect_length(h$vif, 3L)
  expect_true(all(h$vif >= 1))
  expect_equal(h$n, nrow(h$data))
  expect_error(timss_regresi(d, "SAINS", "BSBG04"), "at least two")
  dg <- timss_diagnostik(h$model)
  expect_named(dg$bp, c("statistik", "db", "p"))
  expect_error(timss_diagnostik(1), "lm object")
})
