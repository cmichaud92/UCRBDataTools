#' Title Check data types during processing to insure consistency
#'
#' @param .data A data frame who's name contains either "site", "fish", or "water"
#' @param .table_type The type of table to be checked either "site", "fish", or "water"
#'
#' @return A message or error
#' @export
#'
#' @examples
dp_type_ck <- function(.data, .table_type) {
  if (grepl(.table_type, deparse(substitute(.data)))) {

    if (.table_type == "site" ) {
      tmplt_dp_standard$site %>%
        bind_rows(.data)
      message("PASS: Site table data types match the template")

    } else if (.table_type == "fish") {
      tmplt_dp_standard$fish %>%
        bind_rows(.data)
      message("PASS: Fish table data types match the template")

    } else if (.table_type == "water") {
      tmplt_dp_standard$water %>%
        bind_rows(.data)
      message("PASS: Water table data types match the template")

    } else {
      stop(".table_type not recognized")
    }
  } else {
    stop("Dataset name must include .table_type")
  }
}
