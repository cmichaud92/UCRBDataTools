#' Check STReaMS submissions for missing values in required fields
#'
#' @param .data An imported STReaMS data submission
#' @param .table The type of table ("site", "rare" or "ntf")
#'
#' @return a table and message
#' @export
#'
#' @examples
STReaMS_rqd_na <- function(.data, .table) {

  if (nrow(.data > 0)) {

    if (grepl("rare", .table, ignore.case = TRUE)) {
      rqd_vec <- tmplt_streams_field$`rqd_Rare Fish datasheet`

    } else if (grepl("site", .table, ignore.case = TRUE)) {
      rqd_vec <- tmplt_streams_field$`rqd_Sample - Site-effort datasheet`

    } else if (grepl("ntf", .table, ignore.case = TRUE)) {
      rqd_vec <- tmplt_streams_field$`rqd_Non-tagged Fish datasheet`
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
