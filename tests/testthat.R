library(testthat)
test_that("make_filename",expect_equal(farsss::make_filename(2015),"inst/extdata/accident_2015.csv.bz2",auth_token="c2bf2f93035ee5705a8a108939a0bc5ca7985875"))
