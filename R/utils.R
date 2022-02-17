
# CONSTANTS ---------------------------------------------------------------

UPDATE_CODE_SELECTION_MATCHING_VARS <- c("disease", "code_type", "code")

# EXPORTED ----------------------------------------------------------------

#' Build a SQLite database of clinical code look up and mapping tables
#'
#' Write the output from \code{\link{build_all_lkps_maps}} to a SQLite database.
#'
#' @param all_lkps_maps A named list of look up and mapping tables, as created
#'   by \code{\link{build_all_lkps_maps}}.
#' @param db_path Where the database will be created. If an SQLite database file
#'   already exists here, then the lookup and mapping tables will be added to
#'   this. If \code{NULL} (default), then no database will be created/modified.
#' @param overwrite If \code{TRUE}, overwrite tables in the database if they
#'   already exist. Default value is \code{FALSE}.
#'
#' @return Returns \code{db_path} invisibly
#' @export
all_lkps_maps_to_db <- function(all_lkps_maps,
                                db_path,
                                overwrite = FALSE) {
  # If database already exists at db_path, check if tables to be written are
  # already present
  if (file.exists(db_path)) {
    warning(paste0("File found at ",
                   db_path))

    check_tables_do_not_already_exist <- TRUE
  } else {
    check_tables_do_not_already_exist <- FALSE
  }

  # connect to db
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  if (check_tables_do_not_already_exist) {
    tables_to_be_written <- c(
      CLINICAL_CODE_MAPPINGS_MAP$mapping_table,
      CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table
    )

    tables_already_in_db <- DBI::dbListTables(con)

    tables_already_present_in_db <- subset(tables_to_be_written,
                                           tables_to_be_written %in% tables_already_in_db)

    if (!overwrite) {
      assertthat::assert_that(
        rlang::is_empty(tables_already_present_in_db),
        msg = paste0(
          "Error! The following tables are already present in the database at ",
          db_path,
          ": ",
          stringr::str_c(
            tables_already_present_in_db,
            sep = "",
            collapse = ", "
          )
        )
      )
    } else if (overwrite &
               !rlang::is_empty(tables_already_present_in_db)) {
      warning(
        "The following tables are already present in the database at ",
        db_path,
        " and will be overwritten: ",
        stringr::str_c(
          tables_already_present_in_db,
          sep = "",
          collapse = ", "
        )
      )
    }
  }

  # write to db
  message(paste0(
    "Writing lookup and mapping tables to SQLite database at ",
    db_path
  ))
  for (table_name in names(all_lkps_maps)) {
    message(table_name)
    DBI::dbWriteTable(
      conn = con,
      name = table_name,
      value = all_lkps_maps[[table_name]],
      # ensure table is not inadvertently overwritten/appended to
      overwrite = overwrite,
      append = FALSE
    )
  }

  message("Success!")
  invisible(db_path)
}

#' Build named list of clinical code look up and mapping tables
#'
#' Downloads the lookup and mapping tables from
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UK Biobank resource
#' 592} as well as the NHSBSA BNF-SNOMED mapping table (available
#' \href{https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping}{here})
#' and OPCS4, self-reported medical conditions/medications/operations from the
#' UK Biobank codings file (available
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{here}).
#'
#' @param all_lkps_maps UK Biobank resource 592, as returned by
#'   \code{\link{get_ukb_all_lkps_maps_raw_direct}}.
#' @param bnf_dmd The NHSBSA BNF-SNOMED mapping table, as returned by
#'   \code{\link{get_nhsbsa_snomed_bnf}}.
#' @param ukb_codings The UK Biobank codings file, as returned by
#'   \code{\link[ukbwranglr]{get_ukb_codings_direct}}.
#' @param ctv3sctmap2 Optional: path to the NHS TRUD mapping file for Read 3 to
#'   SNOMEDCT ("ctv3sctmap2_uk_20200401000001.txt").
#' @param phecode_1_2_lkp Optional: path to the phecode v1.2 lookup file
#'   ("phecode_definitions1.2.csv.zip").
#' @param icd10_phecode_1_2 Optional: path to the phecode v1.2 to ICD10 mapping
#'   file ("Phecode_map_v1_2_icd10_beta.csv.zip").
#' @param icd9_phecode_1_2 Optional: path to the phecode v1.2 to ICD10 mapping
#'   file ("phecode_icd9_map_unrolled.csv.zip").
#'
#' @return Returns a named list of data frames.
#' @export
build_all_lkps_maps <-
  function(all_lkps_maps = get_ukb_all_lkps_maps_raw_direct(),
           bnf_dmd = get_nhsbsa_snomed_bnf(),
           ukb_codings = ukbwranglr::get_ukb_codings_direct(),
           ctv3sctmap2 = NULL,
           phecode_1_2_lkp = NULL,
           icd10_phecode_1_2 = NULL,
           icd9_phecode_1_2 = NULL) {
    # ukb resource 592
    all_lkps_maps <- all_lkps_maps
    all_lkps_maps <- all_lkps_maps %>%
      remove_irrelevant_rows_all_lkps_maps()

    # extend tables
    message("Extending tables in UKB resource 592")
    all_lkps_maps$read_v2_drugs_bnf <-
      extend_read_v2_drugs_bnf(all_lkps_maps)
    all_lkps_maps$bnf_lkp <-
      extend_bnf_lkp(all_lkps_maps)

    # opcs4, from ukb codings file
    opcs4_lkp <- make_lkp_from_ukb_codings(
      ukb_codings = ukb_codings,
      Coding = "240",
      Value_col_new_name = "opcs4_code"
    )

    # UKB self-reported medical conditions/medications/operations from ukb codings file
    self_report_cancer <-
      make_lkp_from_ukb_codings(
        ukb_codings = ukb_codings,
        Coding = "3",
        Value_col_new_name = "data_coding_3"
      )

    self_report_medication <-
      make_lkp_from_ukb_codings(
        ukb_codings = ukb_codings,
        Coding = "4",
        Value_col_new_name = "data_coding_4"
      )

    self_report_operation <-
      make_lkp_from_ukb_codings(
        ukb_codings = ukb_codings,
        Coding = "5",
        Value_col_new_name = "data_coding_5"
      )

    self_report_non_cancer <-
      make_lkp_from_ukb_codings(
        ukb_codings = ukb_codings,
        Coding = "6",
        Value_col_new_name = "data_coding_6"
      )

    # https://www.nature.com/articles/s41467-019-09572-5#additional-information
    self_report_med_to_atc_map <- get_ukb_self_report_med_to_atc_map()

    # add 'extra' tables

    ## NHS TRUD Read 3 to SNOMEDCT mapping table
    if (!is.null(ctv3sctmap2)) {
      read_ctv3_sct <- readr::read_tsv(ctv3sctmap2)
    }

    ## Phecode lookup
    if (!is.null(phecode_1_2_lkp)) {
      phecode_lkp <- readr::read_csv(phecode_1_2_lkp)
    }

    ## Phecode to ICD10 map
    if (!is.null(icd10_phecode_1_2)) {
      icd10_phecode <- readr::read_csv(icd10_phecode_1_2) %>%
        dplyr::mutate("ALT_CODE" = stringr::str_remove(.data[["ICD10"]],
                                                        pattern = "\\."))
    }

    ## Phecode to ICD9 map
    if (!is.null(icd9_phecode_1_2)) {
      icd9_phecode <- readr::read_csv(icd9_phecode_1_2) %>%
        dplyr::mutate("icd9" = stringr::str_remove(.data[["icd9"]],
                                                       pattern = "\\."))
    }

    # combine
    all_lkps_maps <- c(
      all_lkps_maps,
      list(
        bnf_dmd = bnf_dmd,
        opcs4_lkp = opcs4_lkp,
        self_report_cancer = self_report_cancer,
        self_report_medication = self_report_medication,
        self_report_operation = self_report_operation,
        self_report_non_cancer = self_report_non_cancer,
        self_report_med_to_atc_map = self_report_med_to_atc_map
      )
    )

    # append 'extra' lookup/mapping tables
    if (!is.null(ctv3sctmap2)) {
      all_lkps_maps <- c(all_lkps_maps,
                         list(read_ctv3_sct = read_ctv3_sct))
    }

    if (!is.null(phecode_1_2_lkp)) {
      all_lkps_maps <- c(all_lkps_maps,
                         list(phecode_lkp = phecode_lkp))
    }

    if (!is.null(icd10_phecode_1_2)) {
      all_lkps_maps <- c(all_lkps_maps,
                         list(icd10_phecode = icd10_phecode))
    }

    if (!is.null(icd9_phecode_1_2)) {
      all_lkps_maps <- c(all_lkps_maps,
                         list(icd9_phecode = icd9_phecode))
    }

    message("Success!")
    return(all_lkps_maps)
  }

#' Download and read the NHSBSA BNF_SNOMED mapping file
#'
#' Mapping table available from
#' \href{https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping}{here}.
#'
#' @return A data frame.
#' @export
get_nhsbsa_snomed_bnf <- function() {
  message("Getting NHSBSA BNF-SNOMED mapping table")
  # file paths
  bnf_dmd_map_zip_path <- file.path(tempdir(), "bnf_dmd.zip")
  bnf_dmd_map_unzipped_dir <- file.path(tempdir(), "bnf_dmd")

  # download file to tempdir, if not already there
  if (!file.exists(bnf_dmd_map_zip_path)) {
    utils::download.file(url = "https://www.nhsbsa.nhs.uk/sites/default/files/2021-08/BNF%20Snomed%20Mapping%20data%2020210819.zip",
                         destfile = bnf_dmd_map_zip_path,
                         mode = 'wb')
  }

  # read file and tidy names
  utils::unzip(bnf_dmd_map_zip_path,
               files = NULL,
               exdir = file.path(tempdir(), "bnf_dmd"))

  bnf_dmd_file <- list.files(bnf_dmd_map_unzipped_dir)
  assertthat::assert_that(length(bnf_dmd_file) == 1,
                          msg = "Error! Unexpected number of files after unzipping NHBSA BNF-SNOMED file")

  bnf_dmd <-
    readxl::read_excel(file.path(bnf_dmd_map_unzipped_dir, bnf_dmd_file))
  names(bnf_dmd) <-
    ukbwranglr:::remove_special_characters_and_make_lower_case(names(bnf_dmd)) %>%
    stringr::str_remove_all("plus_")

  return(bnf_dmd)
}

#' Get UK Biobank resource 592 directly from UKB
#' website
#'
#' Downloads the UK Biobank code mappings file (\code{all_lkps_maps_v3.xlsx},
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592})
#' directly from the UKB website to a temporary directory at
#' \code{\link[base]{tempdir}}. This is then read into R as a named list of data
#' frames, one for each sheet in the original file.
#'
#' \strong{Note:} This is a large object (>450 MB)
#'
#' @return A named list.
#' @export
get_ukb_all_lkps_maps_raw_direct <- function() {
  message("Getting UKB resource 592")
  # name of resource 592 excel file
  primarycare_codings <- "all_lkps_maps_v3.xlsx"

  # filepaths in tempdir
  primarycare_codings_zip_filepath <-
    file.path(tempdir(), "primarycare_codings.zip")
  primarycare_codings_excel_filepath <-
    file.path(tempdir(), primarycare_codings)

  # download primary care codings file to tempdir, if not already there
  if (!file.exists(primarycare_codings_zip_filepath)) {
    message("Downloading primarycare_codings.zip (UKB resource 592) to tempdir")
    utils::download.file(
      "https://biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip",
      primarycare_codings_zip_filepath,
      mode = "wb"
    )
  }

  # extract excel file only from zip
  message("Extracting all_lkps_maps_v3.xlsx from zip file to tempdir")
  utils::unzip(primarycare_codings_zip_filepath,
               files = primarycare_codings,
               exdir = tempdir())

  # reading all sheets into named list
  message("Reading sheets from all_lkps_maps_v3.xlsx to a named list")
  read_all_lkps_maps_raw(primarycare_codings_excel_filepath)
}

# PRIVATE -----------------------------------------------------------------

read_excel_to_named_list <- function(path,
                                     to_include = NULL,
                                     to_exclude = NULL,
                                     col_types = "text",
                                     ...) {
  # validate args
  if (!is.null(to_include) & !is.null(to_exclude)) {
    stop("Error! One or both of `to_include` and `to_exclude` must be `NULL`")
  }

  # get sheet names
  sheet_names <- path %>%
    readxl::excel_sheets()

  # choose sheets
  if (!is.null(to_include)) {
    sheet_names <- subset(sheet_names,
                          sheet_names %in% to_include)
  } else if (!is.null(to_exclude)) {
    sheet_names <- subset(sheet_names,!(sheet_names %in% to_exclude))
  }

  # read sheets into named list
  sheet_names %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel,
               path = path,
               col_types = col_types,
               ...)
}


read_all_lkps_maps_raw <- function(path) {
  read_excel_to_named_list(
    path = path,
    to_include = NULL,
    to_exclude = c("Description", "Contents"),
    col_types = "text"
  )
}

remove_irrelevant_rows_all_lkps_maps <-
  function(all_lkps_maps_raw) {
    nrows_to_remove <- tibble::tribble(
      ~ sheet,
      ~ n_to_remove,
      ~ nrow_before,
      'bnf_lkp',
      2,
      79829,
      'dmd_lkp',
      2,
      434951,
      'icd9_lkp',
      2,
      7973,
      'icd10_lkp',
      2,
      17936,
      'icd9_icd10',
      3,
      16163,
      'read_v2_lkp',
      2,
      101883,
      'read_v2_drugs_lkp',
      2,
      67614,
      'read_v2_drugs_bnf',
      3,
      67615,
      'read_v2_icd9',
      3,
      35664,
      'read_v2_icd10',
      3,
      36667,
      'read_v2_opcs4',
      3,
      14927,
      'read_v2_read_ctv3',
      2,
      108245,
      'read_ctv3_lkp',
      2,
      419098,
      'read_ctv3_icd9',
      3,
      67155,
      'read_ctv3_icd10',
      3,
      116377,
      'read_ctv3_opcs4',
      3,
      54321,
      'read_ctv3_read_v2',
      2,
      777522
    )

    # check expected number of rows are present
    expected_nrows_all_tables <- all_lkps_maps_raw %>%
      names() %>%
      purrr::set_names() %>%
      purrr::map_lgl(~ nrow(all_lkps_maps_raw[[.x]]) == nrows_to_remove[nrows_to_remove$sheet == .x, ][["nrow_before"]])

    unexpected_nrows_tables <- subset(expected_nrows_all_tables,!expected_nrows_all_tables)

    assertthat::assert_that(
      rlang::is_empty(unexpected_nrows_tables),
      msg = paste0(
        "Error! Unexpected number of rows for the following UKB resource 592 tables: ",
        stringr::str_c(
          unexpected_nrows_tables,
          sep = "",
          collapse = ", "
        )
      )
    )

    # remove unnecessary rows
    result <- purrr::pmap(nrows_to_remove,
                          function(sheet, n_to_remove, ...) {
                            all_lkps_maps_raw[[sheet]][1:(nrow(all_lkps_maps_raw[[sheet]]) - n_to_remove), ]
                          })

    names(result) <- names(all_lkps_maps_raw)

    return(result)
  }


make_lkp_from_ukb_codings <- function(ukb_codings,
                                      Coding,
                                      Value_col_new_name,
                                      Meaning_col_new_nae = "description") {
  result <- ukb_codings[ukb_codings$Coding == Coding,-1]

  result <- ukbwranglr:::rename_cols(
    df = result,
    old_colnames = c("Value", "Meaning"),
    new_colnames = c(Value_col_new_name, Meaning_col_new_nae)
  )

  return(result)
}

get_ukb_self_report_med_to_atc_map <- function() {
  # download file
  file_path <-
    file.path(tempdir(), "self_report_med_to_atc_map.xlsx")

  if (!file.exists(file_path)) {
    utils::download.file(
      "https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-019-09572-5/MediaObjects/41467_2019_9572_MOESM3_ESM.xlsx",
      destfile = file.path(tempdir(), "self_report_med_to_atc_map.xlsx")
    )
  }

  # read file
  result <- readxl::read_excel(
    file_path,
    skip = 2,
    col_names = c(
      "self_report_medication",
      "data_coding_4",
      "atc_code",
      "drug_name",
      "rm_1",
      "rm_2"
    )
  )

  # drop redundant cols
  result <- result[,-c(5, 6)]

  # append drug_name in brackets
  result$self_report_medication <-
    paste0(result$self_report_medication,
           " (",
           result$drug_name,
           ")")

  return(result)
}

#' Pre-populate a list of codes with category labels from an existing codelist
#'
#' To be used with \code{\link{runCodeMapper}}. The data frame returned by this
#' app is for a single disease and should have an empty 'category' column. If
#' the user uploads a (valid) clinical code list then the 'categories' will be
#' lifted over, matching on 'disease', 'code_type' and 'code'.
#'
#' @param current_selection A data frame, as returned by
#'   \code{\link{runCodeMapper}}.
#' @param previous_codelist A data frame of clinical codes. Must meet the
#'   requirements of \code{\link}
#'
#' @return A data frame.
#' @noRd
update_code_selection <- function(current_selection,
                                  previous_codelist) {
  # validate previous_codelist
  ukbwranglr::validate_clinical_codes(previous_codelist)

  # copy over previous categories and mark these codes as selected
  current_selection %>%
    dplyr::left_join(
      previous_codelist,
      # match on these columns (NB, does not include 'description' or author')
      by = UPDATE_CODE_SELECTION_MATCHING_VARS,
      suffix = c("", "_TOREMOVE")
    ) %>%
    dplyr::mutate("category" = .data[["category_TOREMOVE"]],
                  "selected" = dplyr::case_when((!is.na(.data[["category"]])) |
                                                  (.data[["category"]] != "") ~ "Yes",
                                                TRUE ~ "")) %>%
    dplyr::select(-tidyselect::ends_with("_TOREMOVE"))
}

# TODO --------------------------------------------------------------------

validate_all_lkps_maps <- function(all_lkps_maps) {
  TRUE
}
