# timss8reg

Helper functions for regression analysis of TIMSS grade 8 student data
(student questionnaire and achievement files in R format).

```r
library(timss8reg)
siswa <- timss_baca("C:/path/to/R Data", "bsg", "bhr")
nilai <- timss_baca("C:/path/to/R Data", "bsa", "bhr")
dat   <- timss_gabung(siswa, nilai)
dat   <- timss_nilai_rata(dat)
```

The package does not contain real TIMSS data. Download the data from <https://timss2023.org/data/>.
