#' Read and parse raw tag manager download file in .xlsx format
#'
#' @param .path Path to and file name of target excel file
#' @param .sheet Which sheet to read
#'
#' @return A tibble or data.frame
#' @export
#'
#' @examples
#' tag_parse_xlsx(system.file("extdata",
#'                            "2022_CASTLE_BUTTE_PIA_1.xlsx",
#'                            package = "UCRBDataTools"))
tag_parse_xlsx <- function(.path, .sheet = 1) {
  readxl::read_xlsx(path = .path,
                    sheet = .sheet,
                    col_types = c(rep("text", 8), rep("numeric", 2), "text", rep("numeric", 2), "text")) |>
    dplyr::rename_with(~tolower(gsub(' |,', '_', .))) |>
    rbind(tagmgr_tmplt) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.))) |>
    dplyr::mutate(source_file = basename(.path),
                  ts_scan = lubridate::mdy_hms(paste(scan_date, scan_time)),
                  ts_dnload = lubridate::mdy_hms(paste(download_date, download_time)),
                  .keep = "unused") |>
    dplyr::select(ts_scan,
                  ts_dnload,
                  id_reader = reader_id,
                  id_antenna = antenna_id,
                  cd_hex = `hex_tag_id`,
                  cd_dec = dec_tag_id,
                  temperature_c,
                  signal_mv,
                  is_duplicate,
                  latitude,
                  longitude,
                  file_name,
                  source_file)

}
