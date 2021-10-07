library(targets)
library(tarchetypes)
library(magrittr)

tar_option_set(
  packages = c("codemapper"),
  imports = c("codemapper")
)

# source("R/utils.R")
# source("R/lookups_and_mappings.R")

config <- configr::read.config("config.ini")

# End this file with a list of target objects.
list(
  # Look up and mapping tables-----------------------------------------------------------
  # raw UKB resource 592 - each sheet from the excel file is an item in the list
  tar_target(
    all_lkps_maps_raw,
    codemapper:::get_ukb_all_lkps_maps_raw_direct()
  ),

  # the NHSBSA BNF to SNOMED mapping table
  tar_target(bnf_dmd,
             codemapper:::get_nhsbsa_snomed_bnf()),

  # `all_lkps_maps_raw` with redundant bottom rows removed, some tables extended, NHSBSA BNF-SNOMED table appended
  tar_target(all_lkps_maps,
             build_all_lkps_maps()),

  tar_target(ALL_LKPS_MAPS_DB,
             codemapper::all_lkps_maps_to_db(all_lkps_maps,
                                             "output/all_lkps_maps.db",
                                             overwrite = TRUE),
             format = "file"),

  # Workflowr Rmds ----------------------------------------------------------
  tar_target(
    index_RMD,
    command = {
      !!tar_knitr_deps_expr("analysis/index.Rmd")
      suppressMessages(workflowr::wflow_build("analysis/index.Rmd", verbose = FALSE))
      c(
        "analysis/index.Rmd",
        "public/index.html"
      )
    },
    format = "file"
  ),


  tar_target(
    reformat_all_lkps_maps_RMD,
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
    clinical_codes_lkps_and_mappings_RMD,
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
