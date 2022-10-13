#' Import a STReaMS stocking data submission
#'
#' @param .file_path A string or vector containing path to target datasets
#'
#' @return A list
#' @export
#'
#' @examples
read_streams_stock_sub <- function(.file_path) {

  # Create empty lists for names/datatypes and data
  tmp_dat <- list()
  datatype <- list()

  for (i in seq_along(.file_path)) {

    #fetch datatypes and names
    datatype[[i]] <- sapply(read_xlsx(path = .file_path[i],
                                      sheet = "stocking datasheet"),
                            class)

    # Fetch complete data set from AGENCY for YEAR
    tmp_dat[[i]] <- read_xlsx(path = .file_path[i],
                              sheet = "stocking datasheet") %>%

      mutate(FILENAME = basename(.file_path[i]),                                        # add file name and xlsx row number
             idx_xlsx = row_number() + 1,
             across(`REC NUM`, as.character),
             across(`TEMPERED TIME`, as.integer),
             across(c(`UTM X`, `UTM Y`), as.numeric))                                # to data for qc purposes
  }

  # Create stock dataframe
  dat <- bind_rows(tmp_dat) %>%
    mutate(across(c(`STOCK DATE`, `REC NUM`), as.character),
           idx = row_number())

  data_pkg <- list(dat = dat,
                   datatype = datatype)
  return(data_pkg)
}
