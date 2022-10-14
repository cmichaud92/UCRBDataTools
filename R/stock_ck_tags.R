#' Check for duplicated or missing values in pittag column of the stocking submission dataset
#'
#' @param .data A data frame
#' @param .tag_col The field containing pittag codes
#'
#' @return A message or warning
#' @export
#'
#' @examples
stock_ck_tags <- function(.data, .tag_col) {

  tags <- .data |>
    pull({{.tag_col}})

  if (sum(is.na(tags)) > 0) {
    warning("FAIL: `PIT TAG 134` field holds missing values")
  } else {
    message("PASS: No missing pit tag values within the dataset.")
  }

  if (sum(duplicated(tags[!is.na(tags)])) > 0) {
    warning("FAIL: `PIT TAG 134` field holds duplicated values")
  } else {
    message("PASS: No duplicated pit tag values within the dataset.")
  }
}
