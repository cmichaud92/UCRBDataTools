#' Check for NA values in required fields
#'
#' @param .data A data frame containing either site data or fish data
#' @param .table A string designating the table type ("site" or "fish")
#'
#' @return A message or warning and table
#' @export
#'
#' @examples
field_rqd_na <- function(.data, .table) {

  if (nrow(.data > 0)) {

    if (grepl("fish", .table, ignore.case = TRUE)) {
      rqd_vec <- c("key_a", "sample_number", "rec_num", "loc_rmi_enc", "ts_enc", "n_fish")

    } else if (grepl("site", .table, ignore.case = TRUE)) {
      rqd_vec <- c("key_a", "sample_number", "id_study_event", "id_gear",
                   "ts_start", "loc_start_rmi", "loc_end_rmi", "el_sec")
    }

    rqd_fields <- rqd_vec

    out <- matrix(nrow = 1, ncol = length(rqd_fields))
    colnames(out) <- rqd_fields

    for (j in seq_along(rqd_fields)) {
      out[, j] <- sum(is.na(.data[rqd_fields[j]]))
    }

    extra_NAs <- tibble::as_tibble(out) |>
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "required_field",
                          values_to = "n_missing_values") |>
      dplyr::mutate(outcome = ifelse(n_missing_values > 0, "FAIL", "PASS"))

    if (colSums(extra_NAs[, 2]) == 0) {
      print(extra_NAs)
      message(paste("Congradulations!!!", .table, "table contains no missing values in required columns!"))
    } else {
      warning (paste(.table, "dataset contains missing values in required fields! Please make corrections before uploading data!"))
      print(extra_NAs)
    }

  } else {
    warning("Dataset contains 0 records!")
  }

}
