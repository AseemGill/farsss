library(testthat)
library(farsss)
devtools::install_github("https://github.com/AseemGill/farsss")
expect_that(farsss:make_filename(2015),is_a("character"))
