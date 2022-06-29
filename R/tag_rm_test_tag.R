#' Title Remove test-tags
#'
#' @param .data A data.frame or tibble
#' @param .tag_prefix The first 3 characters of the hex string
#' @param .tag_col The column containing the hex strings
#'
#' @return A tibble or data.frame
#' @export
#'
#' @examples
#' tag_rm_test_tags(.data = tbl_ex_parsed_dat,
#'                 .tag_prefix = "3E7",
#'                 .tag_col = cd_hex)
#' tag_rm_test_tags(.data = tbl_ex_parsed_xlsx,
#'                 .tag_prefix = "3E7",
#'                 .tag_col = cd_hex)
tag_rm_test_tags <- function(.data, .tag_prefix = "3E7", .tag_col = cd_hex) {
  .data |>
    dplyr::filter(!grepl(paste0("^",
                                .tag_prefix), {{.tag_col}}))
}

