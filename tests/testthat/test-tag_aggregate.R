test_that("tag_aggregate() returns a tibble dataframe", {
  expect_s3_class(tag_aggregate(tbl_ex_parsed_dat
  ), c("tbl_df", "tbl", "data.frame"))
})


test_that("tag_aggregate() returns a tibble dataframe", {
  expect_s3_class(tag_aggregate(tbl_ex_parsed_xlsx
  ), c("tbl_df", "tbl", "data.frame"))
})
