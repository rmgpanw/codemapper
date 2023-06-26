# Script to build local copy of all_lkps_maps.db
library(codemapper)


# Constants ---------------------------------------------------------------

SNOMED_CT_UK_MONOLITH <- ifelse(
  Sys.getenv("SNOMED_CT_UK_MONOLITH") == "",
  NULL,
  Sys.getenv("SNOMED_CT_UK_MONOLITH")
)

ALL_LKPS_MAPS_DB <- Sys.getenv("ALL_LKPS_MAPS_DB")

# Build db ----------------------------------------------------------------

build_all_lkps_maps(
  all_lkps_maps = read_all_lkps_maps(),
  ukb_codings = ukbwranglr::get_ukb_codings(),
  bnf_dmd = get_nhsbsa_snomed_bnf(),
  self_report_med_to_atc_map = get_ukb_self_report_med_to_atc_map(),
  phecode_1_2_lkp = get_phecode_definitions(),
  icd10_phecode_1_2 = get_phecode_icd10_map(),
  icd9_phecode_1_2 = get_phecode_icd9_map(),
  snomed_ct_uk_monolith = SNOMED_CT_UK_MONOLITH
) %>%
  all_lkps_maps_to_db(db_path = ALL_LKPS_MAPS_DB,
                      overwrite = TRUE)


# Connect to db -----------------------------------------------------------

con <- DBI::dbConnect(duckdb::duckdb(), ALL_LKPS_MAPS_DB)
on.exit(DBI::dbDisconnect(conn = con,
                          shutdown = TRUE))
all_lkps_maps_db <- ukbwranglr::db_tables_to_list(con)
