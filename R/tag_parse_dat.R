#' Title Parse Perminant PIA download (.dat) files
#'
#' @param .path File path including file name
#'
#' @return A data frame
#' @export
#'
#' @examples
#' tag_parse_dat(.path = system.file("extdata",
#'                                   "GreenRiverTusherDiversion_Tags.dat",
#'                                   package = "UCRBDataTools"))
tag_parse_dat <- function(.path) {
  readr::read_csv(.path,
                  skip = 4,
                  col_names = FALSE) |>
    dplyr::mutate(source_file = basename(.path),
                  ts_dnload = lubridate::ymd_hms(X1),
                  .keep = "unused") |>
    dplyr::select(idx = X2,
                  ts_dnload,
                  X3, source_file) |>
    tidyr::separate(X3, into = c(NA, "id_reader", "id_antenna", "date", "time", "cd_hex"), sep = " ") |>
    dplyr::mutate(ts_scan = lubridate::mdy_hms(paste(date, time)),
                  .keep = "unused")
}
