library(tidyverse)
library(targets)
library(codemapper)

# load private codemapper functions
devtools::load_all(".")

# connect to ukb clinical events db
con <-
  DBI::dbConnect(RSQLite::SQLite(), "~/Documents/Data/UKB/KCL/ukb_db/ukb.db")
ukb_db <- ukbwranglr::db_tables_to_list(con)

# for reference
ukbwranglr::clinical_events_sources() %>%
  flextable::flextable()

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
  sources = c("gpc1_r3", "gpc2_r3", "gpc3_r3", "gpc4_r3"),
  allow_missing_sources = TRUE
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

# take earliest date per eid-phecode (will be `NA` if no dates recorded)
result %>%
  head(n = 1000) %>%
  dplyr::group_by(eid,
                  phecode) %>%
  dplyr::arrange(date) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::select(eid,
                phecode,
                date) %>%
  # convert to format required for running phewas
  tidyr::pivot_wider(names_from = "phecode",
                     values_from = "date") %>%
  view()
