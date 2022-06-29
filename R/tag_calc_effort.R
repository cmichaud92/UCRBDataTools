#' Title Calculate the effort in hours a portable submersible (PS) antenna was
#' operational
#'
#' @param .data A PS file downloaded using Biomark TagManager and parsed with
#' UCRBDataTools:: tag_parse_xlsx or tag_parse_txt
#' @param .tag_prefix The "test tag" prefix
#'
#' @return A data frame
#' @export
#'
#' @examples
#' tag_calc_effort(.data = tbl_ex_parsed_xlsx,
#'                 .tag_prefix = "3E7")
tag_calc_effort <- function(.data, .tag_prefix = '^3E7') {
  .data |>
    dplyr::filter(grepl(.tag_prefix, cd_hex)) |>
    UCRBDataTools::tag_aggregate(.interval = "day") |>
    dplyr::group_by(cd_hex, id_antenna, id_reader, source_file) |>
    dplyr::summarise(effort_hrs = sum(n_detections),
                     .groups = "drop")
}
