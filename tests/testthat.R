library(testthat)
# devtools::install_github("https://github.com/AseemGill/farsss")
expect_that(make_filename(2015), is_a("character"))
