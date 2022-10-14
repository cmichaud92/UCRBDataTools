#' Test ranges for important numeric fields on stocking submissions
#'
#' @param .data A stocking submission dataset
#'
#' @return Test results
#' @export
#'
#' @examples
stock_ck_ranges <- function(.data) {
  .data |>
    dplyr::group_by(`STOCKING NUMBER`, `STOCK DATE`, SPECIES, `STOCK RIVER`, `STOCK RMI`) |>
    dplyr::summarise(number_obs = n(),
                     dplyr::across(c(LENGTH, WEIGHT, `RIVER TEMP`, `PH RIVER`, `TANK TEMP`, `PH TANK`),
                                   list(max = ~max(., na.rm = TRUE),
                                        min = ~min(., na.rm = TRUE),
                                        mean = ~mean(., na.rm = TRUE))))
}
