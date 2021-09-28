library(targets)
library(magrittr)

source("R/utils.R")
source("R/lookups_and_mappings.R")

config <- configr::read.config("config.ini")

# End this file with a list of target objects.
list(
  # all_lkps_maps and BNF-DMD map-----------------------------------------------------------
  tar_target(
    all_lkps_maps_raw,
    read_all_lkps_maps(config$PATHS$UKB_ALL_LKPS_MAPS)
  ),
  # all_lkps_maps with redundant bottom rows removed
  tar_target(all_lkps_maps,
             {
               all_lkps_maps <-
                 remove_irrelevant_rows_all_lkps_maps(all_lkps_maps_raw)
               all_lkps_maps$read_v2_drugs_bnf <-
                 extend_read_v2_drugs_bnf(all_lkps_maps)
               all_lkps_maps$bnf_lkp <-
                 extend_bnf_lkp(all_lkps_maps)
             }),
  tar_target(bnf_dmd,
             read_nhsbsa_snomed_bnf())
)
