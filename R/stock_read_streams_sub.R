#' Import a STReaMS stocking data submission
#'
#' @param .file_path A string or vector containing path to target datasets
#'
#' @return A list
#' @export
#'
#' @examples
stock_read_streams_sub <- function(.file_path) {

  # Create empty lists for names/datatypes and data
  tmp_dat <- list()
  datatype <- list()

  for (i in seq_along(.file_path)) {

    #fetch datatypes and names
    datatype[[i]] <- sapply(readxl::read_xlsx(path = .file_path[i],
                                              sheet = "stocking datasheet"),
                            class)

    # Fetch complete data set from AGENCY for YEAR
    tmp_dat[[i]] <- readxl::read_xlsx(path = .file_path[i],
                                      sheet = "stocking datasheet",
                                      col_types = "text"
    ) |>

      dplyr::mutate(FILENAME = basename(.file_path[i]),                           # add file name and xlsx row number
                    idx_xlsx = row_number() + 1,
                    dplyr::across(c(`TEMPERED TIME`, `STOCK YEAR`, `UTM ZONE`,
                                    LENGTH, WEIGHT, `YEAR CLASS`), as.integer),
                    dplyr::across(c(`UTM X`, `UTM Y`, `STOCK RMI`,
                                    dplyr::matches('^PH | TEMP$')), as.numeric),
                    dplyr::across(`STOCK DATE`, ~as.Date(as.numeric(.),
                                                         origin = "1899-12-30")))     # to data for qc purposes
  }

  # Create stock dataframe
  dat <- dplyr::bind_rows(tmp_dat) |>
    dplyr::mutate(idx = dplyr::row_number())

  data_pkg <- list(dat = dat,
                   datatype = datatype)
  return(data_pkg)
}
