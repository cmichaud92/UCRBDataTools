test_that("tag_parse_dat() returns a tibble dataframe", {
  expect_s3_class(tag_parse_dat(
    system.file("extdata",
                "GreenRiverTusherDiversion_Tags.dat",
                package = "UCRBDataTools")
  ), c("tbl_df", "tbl", "data.frame"))
})
