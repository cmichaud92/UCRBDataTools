#' Save transformed field data to the STReaMS upload template
#'
#' @param .site_data A data frame containing spec'd site data
#' @param .rare_data A data frame containing spec'd rare fish data
#' @param .rare_data_2 A data frame containing spec'd rare fish data overflow records
#' @param .ntf_data A data frame containing spec'd nontagged fish data
#' @param .year Year data was collected (numeric)
#' @param .study The study underwhich the data was collected
#' @param .agency The agency managing the study
#' @param .output_path Path to data destination directory
#' @param .overwrite Overwrite existing data if it exists?
#'
#' @return Nothing, creates an xlsx file in destination directory
#' @export
#'
#' @examples
field_xlsx_wkbk <- function(.site_data,
                            .rare_data = NULL,
                            .rare_data_2 = NULL,
                            .ntf_data = NULL,
                            .year,
                            .study,
                            .agency,
                            .output_path,
                            .overwrite) {

    # Set Default options (only work on cols where data is present)
    options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")            # Sets default date format
    options("openxlsx.numFmt" = "0.0")                                    # Sets default numeric to 1 decimal place

    # Set style formats for specific formatting requirements
    n0 <- openxlsx::createStyle(numFmt = "0")                                        # Creates a style object for interger
    n1<- openxlsx::createStyle(numFmt = "0.0")                                       # One decimal place
    n4 <- openxlsx::createStyle(numFmt = "0.0000")                                   # Four decimal places
    t <- openxlsx::createStyle(numFmt = "@")                                         # Text formatting
    tm <- openxlsx::createStyle(numFmt = "yyyy-mm-dd hh:mm:ss")

    # Rare Fish fmt
    r_n0 <- c(8, 25)
    r_n1 <- c(7, 12:14)
    r_n4 <- c(26, 27)
    r_t <- c(1:6, 10, 11, 15:24, 28, 29)
    r_tm <- c(9)

    #Site-effort
    s_n0 <- c(16, 19, 21)
    s_n1 <- c(9, 10, 17, 24:26)
    s_n4 <- c(22, 23)
    s_t <- c(1:8, 13:15, 18, 20, 27)
    s_tm <- c(11, 12)

    # Nontagged fish
    n_n0 <- c(1, 2, 8, 12, 40)
    n_n1 <- c(7, 14:16)
    n_n4 <- c(41, 42)
    n_t <- c(3:6, 10, 11, 13, 17:39, 43)
    n_tm <- c(9)

    # Create a blank workbook
    B <- openxlsx::createWorkbook()


    # Add site data
    openxlsx::addWorksheet(wb = B, sheetName = "Sample - Site-effort datasheet")    # Adds a blank sheet and names it
    openxlsx::writeData(wb = B, sheet = 1, x = .site_data)

    openxlsx::addStyle(B, 1, style = n0,                                             # Associates it with col 16
                       rows = 2:(nrow(.site_data)+1),
                       cols = s_n0,
                       gridExpand = TRUE)
    openxlsx::addStyle(B, 1, style = n1,
                       rows = 2:(nrow(.site_data)+1),
                       cols = s_n1,
                       gridExpand = TRUE)
    openxlsx::addStyle(B, 1, style = n4,
                       rows = 2:(nrow(.site_data)+1),
                       cols = s_n4,
                       gridExpand = TRUE)
    openxlsx::addStyle(B, 1, style = t,
                       rows = 2:(nrow(.site_data)+1),
                       cols = s_t,
                       gridExpand = TRUE)
    openxlsx::addStyle(B, 1, style = tm,
                       rows = 2:(nrow(.site_data)+1),
                       cols = s_tm,
                       gridExpand = TRUE)

    # Add rare fish data
    if (!is.null(.rare_data)) {
      openxlsx::addWorksheet(wb = B, sheetName = "Rare Fish datasheet")                # Adds a blank sheet and names it
      openxlsx::writeData(wb = B, sheet = 2, x = .rare_data)                            # Writes data to the blank sheet
      # Add column formatting
      openxlsx::addStyle(B, 2, style = n0,                                             # Associates styles to specific cols
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_n0,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 2, style = n1,
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_n1,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 2, style = n4,
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_n4,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 2, style = t,
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_t,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 2, style = tm,
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_tm,
                         gridExpand = TRUE)
    } else {
      warning("No Rare fish datasheet created!")
    }

    # Add nontagged fish data
    if (!is.null(.ntf_data)){
      openxlsx::addWorksheet(wb = B, sheetName = "Non-tagged Fish datasheet")
      openxlsx::writeData(wb = B, sheet = 3, x = .ntf_data)

      openxlsx::addStyle(B, 3, style = n0,                                             # Associates it with col 16
                         rows = 2:(nrow(.ntf_data)+1),
                         cols = n_n0,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 3, style = n1,
                         rows = 2:(nrow(.ntf_data)+1),
                         cols = n_n1,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 3, style = n4,
                         rows = 2:(nrow(.ntf_data)+1),
                         cols = n_n4,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 3, style = t,
                         rows = 2:(nrow(.ntf_data)+1),
                         cols = n_t,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 3, style = tm,
                         rows = 2:(nrow(.ntf_data)+1),
                         cols = n_tm,
                         gridExpand = TRUE)
    } else {
      warning("No Non-tagged Fish datasheet created!")
    }

    # Add second rare fish data sheet
    if (!is.null(.rare_data_2)) {
      openxlsx::addWorksheet(wb = B, sheetName = "Rare Fish datasheet part 2")                # Adds a blank sheet and names it
      openxlsx::writeData(wb = B, sheet = 4, x = .rare_data_2)                            # Writes data to the blank sheet
      # Add column formatting
      openxlsx::addStyle(B, 4, style = n0,                                             # Associates styles to specific cols
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_n0,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 4, style = n1,
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_n1,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 4, style = n4,
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_n4,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 4, style = t,
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_t,
                         gridExpand = TRUE)
      openxlsx::addStyle(B, 4, style = tm,
                         rows = 2:(nrow(.rare_data)+1),
                         cols = r_tm,
                         gridExpand = TRUE)
    }




    openxlsx::saveWorkbook(wb = B, file = file.path(.output_path, paste0(paste("STReaMS_field-collection_fmt",
                                                                               .study,
                                                                               .agency,
                                                                               .year,
                                                                               sep = "_"),
                                                                         ".xlsx")),
                           overwrite = .overwrite)
}


