library(targets)
library(tarchetypes)
library(magrittr)

tar_option_set(
  packages = c("codemapper"),
  imports = c("codemapper")
)

# End this file with a list of target objects.
list(
  # Files ------------------------

  ## TRUD ------------------------
  tar_target(
    CTV3SCTMAP2,
    Sys.getenv("CTV3SCTMAP2"),
    format = "file"
  ),

  ## PheCODES --------------------
  tar_target(
    PHECODE_1_2_LKP,
    Sys.getenv("PHECODE_1_2_LKP"),
    format = "file"
  ),

  tar_target(
    PHECODE_1_2_ICD10_MAP,
    Sys.getenv("PHECODE_1_2_ICD10_MAP"),
    format = "file"
  ),

  tar_target(
    PHECODE_1_2_ICD9_MAP,
    Sys.getenv("PHECODE_1_2_ICD9_MAP"),
    format = "file"
  ),

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
             build_all_lkps_maps(
               ctv3sctmap2 = CTV3SCTMAP2,
               phecode_1_2_lkp = PHECODE_1_2_LKP,
               icd10_phecode_1_2 = PHECODE_1_2_ICD10_MAP,
               icd9_phecode_1_2 = PHECODE_1_2_ICD9_MAP
               )),

  tar_target(ALL_LKPS_MAPS_DB,
             codemapper::all_lkps_maps_to_db(all_lkps_maps,
                                             "all_lkps_maps.db",
                                             overwrite = TRUE),
             format = "file"),

  tar_target(ALL_LKPS_MAPS_DB_GZIP,
             {
               gzip_file_path <- paste0(ALL_LKPS_MAPS_DB, ".gz")

               R.utils::gzip(filename = ALL_LKPS_MAPS_DB,
                           destname = gzip_file_path,
                           remove = FALSE)

               gzip_file_path
             },
             format = "file"),

  # Workflowr Rmds ----------------------------------------------------------
  tar_target(
    INDEX_RMD,
    command = {
      !!tar_knitr_deps_expr(file.path("analysis", "index.Rmd"))
      suppressMessages(workflowr::wflow_build(file.path("analysis", "index.Rmd"), verbose = FALSE))
      c(
        file.path("analysis", "index.Rmd"),
        file.path("public", "index.html")
      )
    },
    format = "file"
  ),


  tar_target(reformat_all_lkps_maps_RMD,
             command = {
               !!tar_knitr_deps_expr(file.path("analysis", "reformat_all_lkps_maps.Rmd"))
               suppressMessages(workflowr::wflow_build(
                 file.path("analysis", "reformat_all_lkps_maps.Rmd"),
                 verbose = FALSE
               ))
               c(
                 file.path("analysis", "reformat_all_lkps_maps.Rmd"),
                 file.path("public", "reformat_all_lkps_maps.html")
               )
             },
             format = "file"),

  tar_target(clinical_codes_lkps_and_mappings_RMD,
             command = {
               !!tar_knitr_deps_expr(file.path("analysis", "clinical_codes_lkps_and_mappings.Rmd"))
               suppressMessages(workflowr::wflow_build(
                 file.path("analysis", "clinical_codes_lkps_and_mappings.Rmd"),
                 verbose = FALSE
               ))
               c(
                 file.path("analysis", "clinical_codes_lkps_and_mappings.Rmd"),
                 file.path("public", "clinical_codes_lkps_and_mappings.html")
               )
             },
             format = "file"),

  tar_target(
    ICD10_LKP_RMD,
    command = {
      !!tar_knitr_deps_expr(file.path("analysis", "icd10_lkp.Rmd"))
      suppressMessages(workflowr::wflow_build(file.path("analysis", "icd10_lkp.Rmd"), verbose = FALSE))
      c(
        file.path("analysis", "icd10_lkp.Rmd"),
        file.path("public", "icd10_lkp.html")
      )
    },
    format = "file"
  ),

  tar_target(
    READ2_ICD10_MAPPING_RMD,
    command = {
      !!tar_knitr_deps_expr(file.path("analysis", "read2_icd10_mapping.Rmd"))
      suppressMessages(workflowr::wflow_build(file.path("analysis", "read2_icd10_mapping.Rmd"), verbose = FALSE))
      c(
        file.path("analysis", "read2_icd10_mapping.Rmd"),
        file.path("public", "read2_icd10_mapping.html")
      )
    },
    format = "file"
  ),

  tar_target(
    READ3_ICD10_MAPPING_RMD,
    command = {
      !!tar_knitr_deps_expr(file.path("analysis", "read3_icd10_mapping.Rmd"))
      suppressMessages(workflowr::wflow_build(file.path("analysis", "read3_icd10_mapping.Rmd"), verbose = FALSE))
      c(
        file.path("analysis", "read3_icd10_mapping.Rmd"),
        file.path("public", "read3_icd10_mapping.html")
      )
    },
    format = "file"
  ),

  tar_target(
    PHECODES_RMD,
    command = {
      !!tar_knitr_deps_expr(file.path("analysis", "phecodes.Rmd"))
      suppressMessages(workflowr::wflow_build(file.path("analysis", "phecodes.Rmd"), verbose = FALSE))
      c(
        file.path("analysis", "phecodes.Rmd"),
        file.path("public", "phecodes.html")
      )
    },
    format = "file"
  )

)
