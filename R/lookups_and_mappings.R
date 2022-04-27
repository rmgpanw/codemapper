# EXPORTED ----------------------------------------------------------------

#' Build a SQLite database of clinical code look up and mapping tables
#'
#' Write the output from \code{\link{build_all_lkps_maps}} to a SQLite database.
#'
#' @param all_lkps_maps A named list of look up and mapping tables, created
#'   by \code{\link{build_all_lkps_maps}}.
#' @param db_path Where the database will be created. If an SQLite database file
#'   already exists here, then the lookup and mapping tables will be added to
#'   this. If \code{NULL} (default), then no database will be created/modified.
#' @param overwrite If \code{TRUE}, overwrite tables in the database if they
#'   already exist. Default value is \code{FALSE}.
#'
#' @return Returns \code{db_path} invisibly
#' @seealso [build_all_lkps_maps()]
#' @export
all_lkps_maps_to_db <- function(all_lkps_maps = build_all_lkps_maps(),
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
#'   \code{\link{get_ukb_all_lkps_maps}}.
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
#' @seealso [all_lkps_maps_to_db()]
#' @export
build_all_lkps_maps <-
  function(all_lkps_maps = read_all_lkps_maps(),
           bnf_dmd = get_nhsbsa_snomed_bnf(),
           ukb_codings = ukbwranglr::get_ukb_codings_direct(),
           ctv3sctmap2 = NULL,
           phecode_1_2_lkp = NULL,
           icd10_phecode_1_2 = NULL,
           icd9_phecode_1_2 = NULL) {
    # ukb resource 592 ----------------

    ## remove metadata footer rows and add row index column -------------------
    all_lkps_maps <- all_lkps_maps %>%
      purrr::map(rm_footer_rows_all_lkps_maps_df) %>%
      purrr::map(~ tibble::rowid_to_column(.data = .x,
                                           var = ".rowid"))

    ## reformat tables individually ---------------

    ### icd9_icd10 -------------------

    all_lkps_maps$icd9_icd10 <- reformat_icd9_icd10(all_lkps_maps$icd9_icd10)

    ### read2_icd10 ------------------------

    all_lkps_maps$read_v2_icd10 <- reformat_read_v2_icd10(all_lkps_maps$read_v2_icd10,
                                                          icd10_lkp = all_lkps_maps$icd10_lkp)

    ### read3_icd10 ------------------------

    all_lkps_maps$read_ctv3_icd10 <- reformat_read_ctv3_icd10(all_lkps_maps$read_ctv3_icd10)

    ## extend tables -----------------
    message("Extending tables in UKB resource 592")
    all_lkps_maps$read_v2_drugs_bnf <-
      extend_read_v2_drugs_bnf(all_lkps_maps)
    all_lkps_maps$bnf_lkp <-
      extend_bnf_lkp(all_lkps_maps)

    # opcs4 -------------------------

    ## from ukb codings file
    opcs4_lkp <- make_lkp_from_ukb_codings(
      ukb_codings = ukb_codings,
      Coding = "240",
      Value_col_new_name = "opcs4_code"
    )

    # UKB self-reported medical conditions/medications/operations ------------

    ## from ukb codings file
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

    # Add 'extra' tables -------------------------

    ## NHS TRUD Read 3 to SNOMEDCT mapping table ---------
    if (!is.null(ctv3sctmap2)) {
      read_ctv3_sct <- readr::read_tsv(ctv3sctmap2)
    }

    ## Phecode lookup ----------------
    if (!is.null(phecode_1_2_lkp)) {
      phecode_lkp <- readr::read_csv(phecode_1_2_lkp)
    }

    ## Phecode to ICD10 map ---------------------
    if (!is.null(icd10_phecode_1_2)) {
      icd10_phecode <- readr::read_csv(icd10_phecode_1_2) %>%
        dplyr::mutate("ALT_CODE" = stringr::str_remove(.data[["ICD10"]],
                                                       pattern = "\\."))
    }

    ## Phecode to ICD9 map ------------------
    if (!is.null(icd9_phecode_1_2)) {
      icd9_phecode <- readr::read_csv(icd9_phecode_1_2) %>%
        dplyr::mutate("icd9" = stringr::str_remove(.data[["icd9"]],
                                                   pattern = "\\."))
    }

    # Combine -----------------------
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

    # Append 'extra' lookup/mapping tables
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

    # convert all to tibbles (avoids potential problems when writing to SQLite
    # database)
    all_lkps_maps <- all_lkps_maps %>%
      purrr::map(tibble::as_tibble)

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
#' directly from the UKB website.
#'
#' \strong{Note:} This is a large object (>450 MB)
#'
#' @param dir_path Directory path to download to.
#'
#' @return File path to downloaded `all_lkps_maps_v3.xlsx`.
#' @export
get_ukb_all_lkps_maps <- function(dir_path = tempdir()) {
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
               exdir = dir_path)

  # return file path
  return(file.path(dir_path, primarycare_codings))
}

#' Read UK Biobank resource 592 into a named list
#'
#' Reads the UK Biobank code mappings file (`all_lkps_maps_v3.xlsx`,
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592}) into
#' a named list of data frames.
#'
#' \strong{Note:} This is a large object (>450 MB)
#'
#' @param path Path to `all_lkps_maps_v3.xlsx`. By default, this is downloaded
#'   from the UK Biobank website using [get_ukb_all_lkps_maps()].
#'
#' @return A named list of data frames.
#' @export
read_all_lkps_maps <- function(path = get_ukb_all_lkps_maps()) {
  read_excel_to_named_list(
    path = path,
    to_include = NULL,
    to_exclude = c("Description", "Contents"),
    col_types = "text"
  )
}

# PRIVATE -----------------------------------------------------------------

#' Reformat the Read 2 to ICD10 mapping table
#'
#' Converts values in the `icd10_code` column to 'ALT_CODE' format ICD10 codes
#' that are recognised in the `icd10_lkp` lookup table. This involves dividing
#' cells containing more than one ICD10 code over multiple rows (e.g.
#' 'A414+J038' becomes 2 rows), and removing appended 'D'/'A' characters (which
#' indicate dagger/asterisk codes) to a separate column called
#' `icd10_dagger_asterisk` (e.g.'A010D I398A' becomes 'A010' and 'I398' under
#' `icd10_code`, with 'D' and 'A' recorded under `icd10_dagger_asterisk`).
#'
#' **NOTE:** A number of undivided 3 character ICD10 codes appear (incorrectly)
#' without an 'X' appended in this mapping table. For example, 'A64X' appears
#' (incorrectly) as 'A50-A64' in 2 rows. 'A65X' appears as 'A65-A69', 'A70X' as
#' 'A70-A74', 'A89X' as 'A80-A89', 'A99X' as 'A92-A99' etc. This function
#' converts appends 'X' to these codes to match how they appear in the
#' `icd10_lkp` table.
#'
#' ICD10 code 'C836' ('Diffuse non-Hodkin's lymphoma - Undifferentiated
#' (diffuse)') is also removed, as this code has been removed from ICD10 and does
#' not exist in the `icd10_lkp` table.
#'
#' @param read_v2_icd10 The Read 2 to ICD10 mapping table.
#' @param icd10_lkp Data frame. ICD10 lookup table (note, must have a '.rowid'
#'   column).
#'
#' @return A data frame.
#' @noRd
reformat_read_v2_icd10 <- function(read_v2_icd10,
                                   icd10_lkp) {

  # Undivided 3 character ICD10 ALT_CODE X/no X map (as named list)
  icd10_lkp_alt_x_map <- get_icd10_code_alt_code_x_map(icd10_lkp = icd10_lkp,
                                                       undivided_3char_only = TRUE,
                                                       as_named_list = "names_no_x")

  # remove icd10_code 'C836' (see notes above)
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::filter(.data[["icd10_code"]] != "C836")

  # replace spaces and '+' with commas
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate("icd10_code" = stringr::str_replace_all(.data[["icd10_code"]],
                                                          pattern = "[\\s|\\+]",
                                                          replacement = ","))

  # split by comma, then unnest
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate("icd10_code" = stringr::str_split(.data[["icd10_code"]],
                                                    pattern = ",")) %>%
    tidyr::unnest(cols = "icd10_code")

  # remove 'D' and 'A' final characters from ICD10 codes, and place in separate
  # column `icd10_dagger_asterisk`
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate(
      "icd10_dagger_asterisk" = rm_or_extract_appended_icd10_dxa(
        icd10_codes = .data[["icd10_code"]],
        keep_x = TRUE,
        rm_extract = "extract"
      )
    ) %>%
    dplyr::mutate("icd10_code" = rm_or_extract_appended_icd10_dxa(
      icd10_codes = .data[["icd10_code"]],
      keep_x = TRUE,
      rm_extract = "rm"
    ))

  # expand icd10 code ranges, which are flagged as '2' under `icd10_code_def` (e.g. 'E100-E109')
  read_v2_icd10 <- read_v2_icd10 %>%
    expand_icd10_ranges(icd10_lkp = icd10_lkp,
                        icd10_lkp_alt_x_map = icd10_lkp_alt_x_map)

  # Make sure undivided 3 character ICD10 codes have an 'X' appended
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate("icd10_code" = dplyr::recode(.data[["icd10_code"]],!!!icd10_lkp_alt_x_map))

  # check all ICD10 codes now exist in `icd10_lkp`
  check_codes_exist(
    codes = read_v2_icd10$icd10_code,
    lkp_codes = icd10_lkp$ALT_CODE,
    code_type = "icd10"
  )

  # return result
  return(read_v2_icd10)
}

#' Reformat mapping table `read_ctv3_icd10`
#'
#' Removes 'D' and 'A' from the ends of ICD10 codes, and separates these into a
#' separate column called `icd10_dagger_asterisk`. The 'D' and 'A' indicate
#' whether the code is a 'dagger' or 'asterisk' respectively. However, these
#' codes are listed without the appended 'D'/'A' in the `icd10_lkp` table.
#'
#' @param read_ctv3_icd10 A data frame.
#'
#' @return
#' @noRd
reformat_read_ctv3_icd10 <- function(read_ctv3_icd10) {
  read_ctv3_icd10 %>%
    dplyr::mutate(
      "icd10_dagger_asterisk" = rm_or_extract_appended_icd10_dxa(
        icd10_codes = .data[["icd10_code"]],
        keep_x = TRUE,
        rm_extract = "extract"
      )
    ) %>%
    dplyr::mutate("icd10_code" = rm_or_extract_appended_icd10_dxa(
      icd10_codes = .data[["icd10_code"]],
      keep_x = TRUE,
      rm_extract = "rm"
    ))
}

#' Reformat mapping table `icd9_icd10`
#'
#' For ICD9 codes without an equivalent ICD10 code, the ICD10 code is recorded
#' as 'UNDEF', with `NA` for the description (and vice versa). This function
#' converts values of 'UNDEF' in the `ICD9` and `ICD10` columns to `NA`, and
#' checks that `NA` only appears for such cases (i.e. a ICD code cannot be `NA`
#' if it has a description and vice versa).
#'
#' @param icd9_icd10 The `icd9_icd10` mapping table
#'
#' @return A data frame
#' @noRd
reformat_icd9_icd10 <- function(icd9_icd10) {
  # convert 'UNDEF' ICD9/10 codes to `NA`
  icd9_icd10 <- icd9_icd10 %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c(
      "ICD9",
      "ICD10"
    )),
    ~ ifelse(.x == "UNDEF",
             yes = NA_character_,
             no = .x)))

  # check that description and ICD code are either both `NA` or both not `NA`
  icd9_icd10_nabular <- icd9_icd10 %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                ~ is.na(.x)))

  assertthat::assert_that(all(icd9_icd10_nabular$ICD9 == icd9_icd10_nabular$DESCRIPTION_ICD9))
  assertthat::assert_that(all(icd9_icd10_nabular$ICD10 == icd9_icd10_nabular$DESCRIPTION_ICD10))

  # return result
  return(icd9_icd10)
}

extend_bnf_lkp <- function(all_lkps_maps) {
  # each drug entry in `bnf_lkp` gets repeated 8 times: chapter, section,
  # paragraph, subparagraph, chemical_substance, product_name, further_info,
  # full

  bnf_lkp <- all_lkps_maps[["bnf_lkp"]] %>%
    dplyr::collect()

  bnf_lkp %>%
    dplyr::mutate(
      "code_chapter" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 2
      ),
      "code_section" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 4
      ),
      "code_paragraph" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 6
      ),
      "code_subparagraph" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 7
      ),
      "code_chemical_substance" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 9
      ),
      "code_product_name" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 11
      ),
      "code_further_info" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 13
      )
    ) %>%
    dplyr::rename("code_full" = .data[["BNF_Presentation_Code"]]) %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("code"),
      names_to = "BNF_Code_Level",
      values_to = "BNF_Code"
    ) %>%
    dplyr::select(.data[["BNF_Code_Level"]],
                  .data[["BNF_Code"]],
                  tidyselect::everything()) %>%
    dplyr::mutate("BNF_Code_Level" = stringr::str_remove(.data[["BNF_Code_Level"]],
                                                         "code_")) %>%
    dplyr::distinct(.data[["BNF_Code"]],
                    .keep_all = TRUE) %>%
    dplyr::mutate(
      "Description" = dplyr::case_when(
        .data[["BNF_Code_Level"]] == "chapter" ~ .data[["BNF_Chapter"]],
        .data[["BNF_Code_Level"]] == "section" ~ .data[["BNF_Section"]],
        .data[["BNF_Code_Level"]] == "paragraph" ~ .data[["BNF_Paragraph"]],
        .data[["BNF_Code_Level"]] == "subparagraph" ~ .data[["BNF_Subparagraph"]],
        .data[["BNF_Code_Level"]] == "chemical_substance" ~ .data[["BNF_Chemical_Substance"]],
        .data[["BNF_Code_Level"]] == "product_name" ~ .data[["BNF_Product"]],
        .data[["BNF_Code_Level"]] == "further_info" ~ .data[["BNF_Presentation"]],
        .data[["BNF_Code_Level"]] == "full" ~ .data[["BNF_Presentation"]]
      )
    ) %>%
    dplyr::mutate(
      "BNF_Presentation" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Presentation"]]),
      "BNF_Product" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                         "product_name") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Product"]]),
      "BNF_Chemical_Substance" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                              "product_name",
                                                                              "chemical_substance") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Chemical_Substance"]]),
      "BNF_Subparagraph" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                              "product_name",
                                                                              "chemical_substance",
                                                                              "subparagraph") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Subparagraph"]]),
      "BNF_Paragraph" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                           "product_name",
                                                                           "chemical_substance",
                                                                           "subparagraph",
                                                                           "paragraph") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Paragraph"]]),
      "BNF_Section" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                         "product_name",
                                                                         "chemical_substance",
                                                                         "subparagraph",
                                                                         "paragraph",
                                                                         "section") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Section"]]),
    ) %>%
    dplyr::select(tidyselect::all_of(c(
      "BNF_Code",
      "BNF_Code_Level",
      "BNF_Chapter",
      "BNF_Section",
      "BNF_Paragraph",
      "BNF_Subparagraph",
      "BNF_Chemical_Substance",
      "BNF_Product",
      "BNF_Presentation",
      "Description"
    )))
}

extend_read_v2_drugs_bnf <- function(all_lkps_maps) {
  # get required tables
  read_v2_drugs_bnf <- all_lkps_maps[["read_v2_drugs_bnf"]] %>%
    dplyr::collect()
  read_v2_drugs_lkp <- all_lkps_maps[["read_v2_drugs_lkp"]] %>%
    dplyr::collect()
  bnf_lkp <- all_lkps_maps[["bnf_lkp"]] %>%
    dplyr::collect()

  # extend 'bnf_lkp'
  bnf_lkp_extended <- extend_bnf_lkp(all_lkps_maps)

  # extend `read_v2_drugs_bnf`
  expected_nrow <- nrow(read_v2_drugs_bnf)

  result <- read_v2_drugs_bnf %>%
    # add read code descriptions
    dplyr::left_join(read_v2_drugs_lkp,
                     by = "read_code") %>%
    # extract bnf chapter, section etc from `bnf_code` col in `read_v2_drugs_bnf`
    dplyr::mutate(
      "bnf_chapter_code" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 2
      ),
      "bnf_section_code" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 4
      ),
      "bnf_paragraph_code" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 6
      ),
      "bnf_subparagraph_code" = paste0(
        .data[["bnf_paragraph_code"]],
        stringr::str_sub(
          stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
          start = 8,
          end = 8
        )
      )
    ) %>%

    # add BNF details
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Chapter")],
                     by = c("bnf_chapter_code" = "BNF_Code")) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Section")],
                     by = c("bnf_section_code" = "BNF_Code")) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Paragraph")],
                     by = c("bnf_paragraph_code" = "BNF_Code")) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Subparagraph")],
                     by = c("bnf_subparagraph_code" = "BNF_Code"))

  # check nrows remains the same
  assertthat::assert_that(expected_nrow == nrow(result),
                          msg = "Error! Unexpected number of rows when extending `read_v2_drugs_bnf`")

  return(result)
}

