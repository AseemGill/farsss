library(testthat)
devtools::install_github("https://github.com/AseemGill/farsss/tree/master/inst/extdata")
expect_that(make_filename(2015), is_a("character"))
