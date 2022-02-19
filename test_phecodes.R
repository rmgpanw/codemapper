library(tidyverse)
library(targets)
library(codemapper)

# connect to ukb clinical events db
con <-
  DBI::dbConnect(RSQLite::SQLite(), "~/Documents/Data/UKB/KCL/ukb_db/ukb.db")
ukb_db <- ukbwranglr::db_tables_to_list(con)

# for reference
ukbwranglr::clinical_events_sources() %>%
  flextable::flextable()


# constants ---------------------------------------------------------------

phecode_colnames <- c("eid",
                      "source",
                      "index",
                      "phecode",
                      "date")

# Mapping dfs -------------------------------------------------------------

read2_icd10 <- get_mapping_df(from = "read2",
                              to = "icd10")

read3_icd10 <- get_mapping_df(from = "read3",
                              to = "icd10")

icd10_phecode <- get_mapping_df(from = "icd10",
                                to = "phecode")

icd9_phecode <- get_mapping_df(from = "icd9",
                                to = "phecode")


# functions ---------------------------------------------------------------

assert_sources_match_data_coding <- function(clinical_events,
                                           data_coding) {
  # validate args
  ukbwranglr:::validate_clinical_events_and_check_type(clinical_events)
  assertthat::is.string(data_coding)
  assertthat::assert_that(data_coding %in% ukbwranglr::clinical_events_sources()$data_coding,
                          msg = paste0("Error! Unrecognised `data_coding`: ",
                                       data_coding))

  # see what sources are in `clinical_events`
  included_sources <- unique(self_reported_icd10$source)

  # get which data codings are associated with these
  included_data_codings <- ukbwranglr::clinical_events_sources() %>%
    dplyr::filter(.data[["source"]] %in% !!included_sources) %>%
    dplyr::pull(.data[["source"]]) %>%
    unique()

  # check only one `data_coding`, and check that this matches what's expected
  assertthat::are_equal(included_data_codings, data_coding)

  invisible(NULL)
}

#' Map codes in a UKB clinical events table
#'
#' Joins the existing `code` column to a mapping data frame created by
#' [codemaper::get_mapping_df], then filters out any missing values for the new
#' code type, then replaces the old `code` column with the newly mapped codes.
#'
#' Clinical events can contain multiple sources, but these should all use the
#' same data_coding. An error is raised otherwise.
#'
#' @param clinical_events A clinical events table created by
#'   [ukbwranglr::tidy_clinical_events()]
#' @param from Coding type being mapped from
#' @param to Coding type being mapped to
#' @param all_lkps_maps Named list of SQLite database with lookup/mapping tables
#' @param strict_ukb If `TRUE`, extra checks are performed to check that
#'   `source` and `data_coding` match [ukbwranglr::clinical_events_sources()].
#'
#' @return A clinical events dataframe
map_codes_ukb_clinical_events <- function(clinical_events,
                                          from,
                                          to,
                                          all_lkps_maps = "all_lkps_maps.db",
                                          strict_ukb = TRUE) {

  # validate args
  check_mapping_args(from = from,
                     to = to)

  if (strict_ukb) {
    ukbwranglr:::validate_clinical_events_and_check_type(clinical_events)

    ## check `clinical_events` contains only one `data_coding` type
    assert_sources_match_data_coding(clinical_events = clinical_events,
                                     data_coding = from)
  }

  # all_lkps_maps
  ## connect to database file path
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  }

  # create mapping df
  mapping_df <- get_mapping_df(from = from,
                               to = to,
                               all_lkps_maps = all_lkps_maps)

  # map
  result <- clinical_events %>%
    dplyr::left_join(mapping_df,
                     by = c("code" = from)) %>%
    # remove `NA` values
    dplyr::filter(!is.na(.data[[to]])) %>%

    # rename/reformat
    dplyr::select(-.data[["code"]]) %>%
    dplyr::rename("code" = .data[[to]]) %>%
    dplyr::select(tidyselect::all_of(ukbwranglr:::CLINICAL_EVENTS_COLHEADERS)) %>%

    # remove duplicated rows
    dplyr::distinct()

  # return result
  return(result)
}

map_icd10_to_phecode <- function(clinical_events) {
  map_codes_ukb_clinical_events(
    clinical_events = clinical_events,
    from = "icd10",
    to = "phecode",
    all_lkps_maps = "all_lkps_maps.db",
    strict_ukb = FALSE
  ) %>%
    dplyr::rename("phecode" = .data[["code"]])
}

# Self-reported non-cancer -----------------------------------------------------------

self_reported_icd10 <-  get_clinical_events_source(
  clinical_events = ukb_db$clinical_events,
  sources = c("f20002_icd10")
)

self_reported_icd10 <- map_icd10_to_phecode(clinical_events = self_reported_icd10)

# HES/Cancer ---------------------------------------------------------------------


## ICD-10 ------------------------------------------------------------------

hes_cancer_icd10 <-
  get_clinical_events_source(
    clinical_events = ukb_db$clinical_events,
    sources = c("f41270", "f40006")
  )

hes_cancer_icd10 <- map_icd10_to_phecode(clinical_events = hes_cancer_icd10)

## ICD-9 ------------------------------------------------------------------

hes_cancer_icd9 <-
  get_clinical_events_source(
    clinical_events = ukb_db$clinical_events,
    sources = c("f41271", "f40013")
  )

hes_cancer_icd9 <- map_codes_ukb_clinical_events(
  clinical_events = hes_cancer_icd9,
  from = "icd9",
  to = "icd10",
  all_lkps_maps = "all_lkps_maps.db",
  strict_ukb = FALSE
) %>%
  map_icd10_to_phecode()

# Death -------------------------------------------------------------------

death_icd10 <-
  get_clinical_events_source(
    clinical_events = ukb_db$clinical_events,
    sources = c("f40001", "f40002")
  )

death_icd10 <- map_icd10_to_phecode(clinical_events = death_icd10)

# GP - read 3 -------------------------------------------------------------

# note: read 3 codes only for GP data provider 3 (England TPP)

gp_read3 <- get_clinical_events_source(
  clinical_events = ukb_db$clinical_events,
  sources = c("gpc1_r3", "gpc2_r3", "gpc3_r3", "gpc4_r3")
)

system.time(
gp_read3 <- gp_read3 %>%
  map_codes_ukb_clinical_events(
    from = "read3",
    to = "icd10",
    all_lkps_maps = "all_lkps_maps.db",
    strict_ukb = FALSE
  ) %>%
  map_icd10_to_phecode()
)

# GP - read 2 -------------------------------------------------------------

# note: no read 2 codes for GP data provider 3 (England TPP)
gp_read2 <- get_clinical_events_source(
  clinical_events = ukb_db$clinical_events,
  sources = c("gpc1_r2", "gpc2_r2", "gpc3_r2", "gpc4_r2"),
  allow_missing_sources = TRUE
)

gp_read2 <- gp_read2 %>%
  map_codes_ukb_clinical_events(
    from = "read2",
    to = "icd10",
    all_lkps_maps = "all_lkps_maps.db",
    strict_ukb = FALSE
  ) %>%
  map_icd10_to_phecode()

# Combine -----------------------------------------------------------------

result <- list(
  self_reported_icd10,
  hes_cancer_icd10,
  hes_cancer_icd9,
  death_icd10,
  gp_read2,
  gp_read3
) %>%
  dplyr::bind_rows()
