library(targets)
library(tarchetypes)
library(magrittr)

source("R/utils.R")
source("R/lookups_and_mappings.R")

config <- configr::read.config("config.ini")

# End this file with a list of target objects.
list(
  # Look up and mapping tables-----------------------------------------------------------
  # raw UKB resource 592 - each sheet from the excel file is an item in the list
  tar_target(
    all_lkps_maps_raw,
    read_all_lkps_maps(config$PATHS$UKB_ALL_LKPS_MAPS)
  ),

  # the NHSBSA BNF to SNOMED mapping table
  tar_target(bnf_dmd,
             read_nhsbsa_snomed_bnf()),

  # `all_lkps_maps_raw` with redundant bottom rows removed, some tables extended, NHSBSA BNF-SNOMED table appended
  tar_target(all_lkps_maps,
             {
               # remove redundant bottom rows
               all_lkps_maps <-
                 remove_irrelevant_rows_all_lkps_maps(all_lkps_maps_raw)

               # extend tables
               all_lkps_maps$read_v2_drugs_bnf <-
                 extend_read_v2_drugs_bnf(all_lkps_maps)
               all_lkps_maps$bnf_lkp <-
                 extend_bnf_lkp(all_lkps_maps)

               # append NHSBSA BNF-SNOMED table
               all_lkps_maps <- c(all_lkps_maps, list(bnf_dmd = bnf_dmd))

               # final result
               all_lkps_maps
             }),

  # Workflowr Rmds ----------------------------------------------------------
  tar_target(
    reformat_all_lkps_maps,
    command = {
      !!tar_knitr_deps_expr("analysis/reformat_all_lkps_maps.Rmd")
      suppressMessages(workflowr::wflow_build("analysis/reformat_all_lkps_maps.Rmd", verbose = FALSE))
      c(
        "analysis/reformat_all_lkps_maps.Rmd",
        "public/reformat_all_lkps_maps.html"
      )
    },
    format = "file"
  ),

  tar_target(
    clinical_codes_lkps_and_mappings,
    command = {
      !!tar_knitr_deps_expr("analysis/clinical_codes_lkps_and_mappings.Rmd")
      suppressMessages(workflowr::wflow_build("analysis/clinical_codes_lkps_and_mappings.Rmd", verbose = FALSE))
      c(
        "analysis/clinical_codes_lkps_and_mappings.Rmd",
        "public/clinical_codes_lkps_and_mappings.html"
      )
    },
    format = "file"
  )

)
