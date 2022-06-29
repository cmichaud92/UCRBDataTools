test_that("tag_rm_test_tags() returns a tibble dataframe", {
  expect_s3_class(tag_rm_test_tags(tbl_ex_parsed_dat
  ), c("tbl_df", "tbl", "data.frame"))
})


test_that("tag_rm_test_tags() returns a tibble dataframe", {
  expect_s3_class(tag_rm_test_tags(tbl_ex_parsed_xlsx
  ), c("tbl_df", "tbl", "data.frame"))
})
