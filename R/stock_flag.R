#' Apply QC flags to stocking dataset
#'
#' @param .data A stocking dataset formatted to STReaMS spec
#'
#' @return A dataframe and message or warning
#' @export
#'
#' @examples
stock_flag <- function(.data) {

  `%!in%` <- Negate(`%in%`)

  out <- .data |>
    dplyr::mutate(date_flg = ifelse(is.na(`STOCK DATE`) |
                                      lubridate::year(lubridate::ymd_hms(`STOCK DATE`)) != `STOCK YEAR`, "FLAG", ""),
                  pit_flg = ifelse(grepl('^[A-Fa-f0-9]{3}\\.?[A-Fa-f0-9]{10}$', `PIT TAG 134`), "", "FLAG"),
                  spp_flg = ifelse(SPECIES %in% vec_spp$end, "", "FLAG"),
                  bio_flg = ifelse(`PROJECT BIOLOGIST` %in% dimension_streams$vec_person, "", "FLAG"),
                  rvr_flg = ifelse(`STOCK RIVER` %in% dimension_streams$vec_rvr, "", "FLAG"),
                  len_flg = dplyr::case_when(!is.na(LENGTH) &
                                               !between(LENGTH, 100, 550) ~ "FLAG",
                                             TRUE ~ ""),
                  wt_flg = dplyr::case_when(!is.na(WEIGHT) &
                                              !between(WEIGHT, 40, 750) ~ "FLAG",
                                            TRUE ~ ""),
                  temp_flg = dplyr::case_when(!is.na(`RIVER TEMP`) &
                                                !between(`RIVER TEMP`, 6, 30) |
                                                !is.na(`TANK TEMP`) &
                                                !between(`TANK TEMP`, 6, 30) ~ "FLAG",
                                              TRUE ~ ""),
                  pH_flg = dplyr::case_when(!is.na(`PH TANK`) &
                                              !between(`PH TANK`, 6, 9) |
                                              !is.na(`PH RIVER`) &
                                              !between(`PH RIVER`, 6, 9) ~ "FLAG",
                                            TRUE ~ ""),
                  harvest_flg = ifelse(`HARVEST TYPE` %in% c("A", "P"), "", "FLAG"),
                  release_flg = ifelse(`RELEASE TYPE` %in% c("S", "H"), "", "FLAG"),
                  agency_org_flg = ifelse(AGENCY %!in% dimension_streams$vec_org |
                                            `SOURCE ORG` %!in% dimension_streams$vec_org, "FLAG", "")) |>
    dplyr::filter(dplyr::if_any(dplyr::matches('_flg$'), ~grepl('FLAG', .)))

  if (dim(out)[1] == 0) {
    message("Congradulations, no flags!!!")
  } else {
    warning(paste0(nrow(out)," flaged records encountered, check output, make corrections and retest"))
    return(out)
  }
}
