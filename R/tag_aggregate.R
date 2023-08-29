#' Title Aggregate ppit tage detections to specified time interval
#'
#' @param .data A parsed pit tag file
#' @param .interval Time interval to aggregate to
#'
#' @return A data frame
#' @export
#'
#' @examples
#' tag_aggregate(tbl_ex_parsed_xlsx, .interval = "hour")
#' tag_aggregate(tbl_ex_parsed_dat, .interval = "day")
tag_aggregate <- function(.data,
                          .interval = "min") {

  stopifnot(is.data.frame(.data),
            .interval %in% c("min", "hour", "day"),
            c("cd_pit134_hex", "source_file") %in% names(.data))

  if (.interval == "min") {
    .data |>
      dplyr::mutate(interval = format(ts_enc, '%Y-%m-%d %H:%M')) |>
      dplyr::group_by(cd_pit134_hex, interval, source_file) |>
      dplyr::summarise(ts_enc = dplyr::first(ts_enc),
                       n_detections = dplyr::n(),
                       .groups = "drop") |>
      dplyr::select(-interval)

  } else if (.interval == "hour") {
    .data |>
      dplyr::mutate(interval = format(ts_enc, '%Y-%m-%d %H')) |>
      dplyr::group_by(cd_pit134_hex, interval, source_file) |>
      dplyr::summarise(ts_enc = dplyr::first(ts_enc),
                       n_detections = dplyr::n(),
                       .groups = "drop") |>
      dplyr::select(-interval)

  } else if (.interval == "day") {
    .data |>
      dplyr::mutate(interval = format(ts_enc, '%Y-%m-%d')) |>
      dplyr::group_by(cd_pit134_hex, interval, source_file) |>
      dplyr::summarise(ts_enc = dplyr::first(ts_enc),
                       n_detections = dplyr::n(),
                       .groups = "drop") |>
      dplyr::select(-interval)

  } else {
    stop("interval is not of compatible 'type', please use 'min', 'hour', 'day'")
  }
}
