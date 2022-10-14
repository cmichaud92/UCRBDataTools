#' Test required fields for missing values in stocking data submissions
#'
#' @param .data A stocking submission dataset
#'
#' @return Test results
#' @export
#'
#' @examples
stock_rqd_na <- function(.data) {

  rqd_stock <- tmplt_streams_stock[["rqd_stocking datasheet"]]

  out <- matrix(nrow = 1, ncol = length(rqd_stock))
  colnames(out) <- rqd_stock

  for (j in seq_along(rqd_stock)) {
    out[, j] <- sum(is.na(.data[rqd_stock[j]]))
  }

  extra_NAs <- tibble::as_tibble(out) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "required_field", values_to = "n_missing_values") |>
    dplyr::mutate(outcome = ifelse(n_missing_values > 0, "FAIL", "PASS"))

  if (colSums(extra_NAs[, 2]) == 0) {
    message("Congradulations!!! No missing values in required columns!")
  } else {
    warning ("This dataset contains missing values in required fields! Please make corrections before uploading data!")
    print(extra_NAs)
  }
}

