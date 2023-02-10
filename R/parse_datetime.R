#' Parse datetimes from incorrectly formatted values
#'
#' @param .x A datetime stored as a character string
#'
#' @return A POSIXct datetime
#' @export
#'
#' @examples
parse_datetime <- function(.x) {

  tmp_dtm <- gsub('[ ]+', ' ', trimws(.x))

  if (stringr::str_length(tmp_dtm) == 16 |
      (stringr::str_length(tmp_dtm) == 19 &
       grepl('( PM)|( AM)', tmp_dtm, ignore.case = TRUE))) {

    dtm <- lubridate::ymd_hm(tmp_dtm)

  } else if (stringr::str_length(tmp_dtm) == 19 |
             (stringr::str_length(tmp_dtm) == 22 &
              grepl('( PM)|( AM)', tmp_dtm, ignore.case = TRUE))) {

    dtm <- lubridate::ymd_hms(tmp_dtm)

  } else if (stringr::str_length(tmp_dtm) == 10) {

    dtm <- lubridate::ymd_hms(paste(tmp_dtm, "00:00:00"))

  } else {

    warning("string failed to parse")
    dtm <- NA
  }

  return(dtm)

}
