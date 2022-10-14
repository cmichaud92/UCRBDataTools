#' Test non-required fields for missing values in stocking data submissions
#'
#' @param .data A stocking submission dataset
#'
#' @return Test results
#' @export
#'
#' @examples
stock_other_na <- function(.data) {

  not_rqd_stock <- tmplt_streams_stock[["not_rqd_stocking datasheet"]]

  out <- matrix(nrow = 1, ncol = length(not_rqd_stock))
  colnames(out) <- not_rqd_stock

  for (j in seq_along(not_rqd_stock)) {
    out[, j] <- sum(is.na(.data[not_rqd_stock[j]]))
  }

  extra_NAs <- tibble::as_tibble(out) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "non_required_field", values_to = "n_missing_values") |>
    dplyr::mutate(outcome = ifelse(n_missing_values > 0, "CHECK", "PASS"))

  if (colSums(extra_NAs[, 2]) == 0) {
    message("Congradulations!!! No missing values in non-required columns!")
  } else {
    warning ("This dataset contains missing values in some non-required fields! Please double check your submission is complete!")
    print(extra_NAs)
  }

}
