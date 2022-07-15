spat_rch <- function(.reach_code, .rivers) {

  require(magrittr)

  nhd_pts <- sf::st_read("C:/Users/cmichaud/OneDrive - DOI/Documents/Data/Spatial/ucrb_spat_v2.sqlite",
                         layer = "nhd_rmi_pts",
                         stringsAsFactors = FALSE) %>%
    #    filter(cd_rvr %in% .rivers) %>%
    dplyr::rename(spat_rch = cd_rch)
  #    arrange(cd_rvr)

  nhd_lns <- sf::st_read("C:/Users/cmichaud/OneDrive - DOI/Documents/Data/Spatial/ucrb_spat_v2.sqlite",
                         layer = "nhd_ctr_lns",
                         stringsAsFactors = FALSE)
  #    filter(cd_rvr %in% .rivers) %>%
  #    arrange(cd_rvr)

  if (grepl('.csv$', .rch_tbl_path, ignore.case = TRUE)) {
    tmp_rch <- readr::read_csv(.rch_tbl_path)
  } else {
    tmp_rch <- .rch_tbl_path
  }

  rch <- tmp_rch %>%
    dplyr::filter(cd_rvr %in% .rivers) %>%
    dplyr::select(id_rch, cd_rvr, cd_rch, fct_rvr, fct_rch, nhd_up, nhd_dn)

  rch_brks <- rch %>%
    tidyr::pivot_longer(cols = c(nhd_up, nhd_dn),
                        names_to = "location",
                        values_to = "rmi_nhd") %>%
    dplyr::mutate(across(rmi_nhd, ~ifelse(. == 0,
                                          0.1,
                                          .))) %>%
    dplyr::left_join(dplyr::select(nhd_pts, cd_rvr, rmi_nhd), by = c("cd_rvr", "rmi_nhd")) %>%
    sf::st_as_sf()

  midpt_rch <- rch %>%
    dplyr::mutate(mid = round((nhd_up + nhd_dn) / 2, 1)) %>%
    dplyr::left_join(dplyr::select(nhd_pts, cd_rvr, rmi_nhd), by = c("cd_rvr", "mid" = "rmi_nhd")) %>%
    sf::st_as_sf()

  parts <- list()

  for (i in seq_along(.rivers)) {
    lns <- nhd_lns %>%
      dplyr::filter(cd_rvr == .rivers[[i]])
    brks <- rch_brks %>%
      dplyr::filter(cd_rvr == .rivers[[i]])

    parts[[i]] <-  sf::st_collection_extract(lwgeom::st_split(lns$GEOMETRY, brks$GEOMETRY),"LINESTRING") %>%
      sf::st_as_sf() %>%

      dplyr::rename(geometry = x)
  }

  mapview(parts)
  reach <- parts %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(idx = row_number()) %>%
    sf::st_join(midpt_rch, join = st_intersects, left = FALSE)

}
