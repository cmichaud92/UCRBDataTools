test_that("tag_parse_xlsx() returns a tibble dataframe", {
  expect_s3_class(tag_parse_xlsx(
    system.file("extdata",
                "2022_CASTLE_BUTTE_PIA_1.xlsx",
                package = "UCRBDataTools")
  ), c("tbl_df", "tbl", "data.frame"))
})
