#' Read in and parse STReaMS encounters download files
#'
#' @param .path A string representing the file path to the dataset
#'
#' @return A data frame
#' @export
#'
#' @examples
str_parse_enc <- function(.path) {
  readr::read_tsv(.path) %>%
    mutate(across(EncounterTime, as.character),
           across(EncounterTime, ~ifelse(is.na(EncounterTime),
                                         "00:00:00",
                                         .)),
           ts_enc = lubridate::ymd_hms(paste(EncounterDate, EncounterTime)),
           year = lubridate::year(ts_enc)) %>%
    dplyr::left_join(dplyr::select(tbl_spp, cd_spp, nm_com),
                     by = c("CommonName" = "nm_com")) %>%
    dplyr::select(id_indiv = IndividualID,
                  id_enc = EncounterID,
                  id_sample = SampleorStockingNumber,
                  cd_org = Org,
                  year,
                  ts_enc,
                  dt_enc = EncounterDate,
                  cd_hex = MostRecentTag,
                  nm_rvr = RiverName,
                  rmi_enc = RiverMile,
                  cd_spp,
                  tot_length = TotalLength,
                  weight = Weight,
                  enc_typ = EncounterType,
                  cd_gear = GearType,
                  nm_pia_array = ArrayName,
                  cd_pia_antenna = AntennaCode,
                  nm_pia_subarray = AntennaSubArray,
                  dt_tag_deploy = TagDeployDate,
                  nm_study = Study,
                  nm_sci = ScientificName,
                  nm_com = CommonName,
                  utm_x = UTMX,
                  utm_y = UTMY,
                  utm_zone = UTMZone,
                  lon = Longitude,
                  lat = Latitude,
                  field_notes_enc = EncounterNotes,
                  dba_flg_indiv = IndividualDBAFlag,
                  dba_flg_enc = EncounterDBAFlag,
                  dba_notes_indiv = IndividualDBAFlagNotes,
                  dba_notes_enc = EncounterDBAFlagNotes)
}
