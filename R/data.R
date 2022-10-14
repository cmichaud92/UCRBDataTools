#' Example of parsed .dat file
#'
#' A dataset containing the contents of a parsed Perminant antenna .dat file
#'
#' @format A table with 13,486 rows and 7 variables
#'
#'
"tbl_ex_parsed_dat"


#' Example of a parsed .xlsx file
#'
#' A dataset containing the contents of a parsed Biomark TagManager download file
#' exported to .xlsx
#'
#' @format A table with 890 rows and 7 variables
#'
#'
"tbl_ex_parsed_xlsx"

#' Template to check processed data's data types
#'
#' A dataset containing 1 record (NA) but possessing typed columns.  This may be
#' joined to processed data to enusre no unexpected changes to data types
#' This template may be deprecated... eval and delete
#'
#' @format A list containing 3 typed dataframes
#'
#'
"tmplt_dp_standard"

#' National Water Information System gage locatons and codes for STReaMS rivers
#'
#' A dataset containing ~438 records of location and other high level station data
#'
#' @format A data frame
#'
#'
"tbl_NWIS_gage"

#' National Water Information System gage locatons and codes for STReaMS rivers
#'
#' A dataset 11 useful data parameter codes for use with dataRetrevial
#'
#' @format A data frame
#'
#'
"tbl_NWIS_params"

#' Reach definitions and descriptions
#'
#' A dataset with 78 records and 12 fields
#'
#' @format A data frame
#'
#'
"tbl_reach"

#' Species definitions adapted from STReaMS species look-up table
#'
#' A dataset with 83 records and 5 fields
#'
#' @format A data frame
#'
#'
"tbl_spp"

#' Species definitions adapted from STReaMS species look-up table matched with
#' CPW species codes
#'
#' A dataset with 79 records and 6 fields
#'
#' @format A data frame
#'
#'
"tbl_spp_crswlk"

#' Data typed streams field data upload template, including vectors of rqd fields
#' and more
#'
#' A list, 9 elements
#'
#' @format A list
#'
#'
"tmplt_streams_field"

#' Data typed streams field data upload template, including vectors of rqd fields
#' and more
#'
#' A list, 9 elements
#'
#' @format A list
#'
#'
"tmplt_streams_stock"

#' Template to check data against the STReaMS stocking template data standard
#'
#'
#' @format A list containing 2 typed dataframes and a series of 4 vectors
#' referencing column names
#'
#'
"tmplt_electrofish_standard"

#' Template to check data against the electrofishing data standard
#'
#' A dataset containing 1 record (NA) but possessing typed columns.  This may be
#' joined to processed data to ensure no unexpected changes to data types
#' Also contains column types strings for import with readr::read_csv
#' This template superceeds `tmplt_electrofish_standard`
#'
#' @format A list containing 3 typed dataframes and 3 column type strings
#'
#'
"tmplt_electrofish_standard_v2"

#' A list including relevant dimension tables and vectors from the STReaMS database
#'
#' A large object containing 19 elements
#'
#' @format A list
#'
#'
"dimension_streams"

#' A list including useful species subsetting vectors
#'
#' A list object containing 4 elements
#'
#' @format A list
#'
#'
"vec_spp"
