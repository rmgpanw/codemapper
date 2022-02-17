
# OVERVIEW ----------------------------------------------------------------

# Functions to map between different clinical codes e.g. between Read2 and
# Read3, or Read3 and ICD-10. These rely on the code mapping file provided by UK
# Biobank (resource 592: https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) and
# the BNF to SNOMED mapping file from the NHSBSA website
# (https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping)

# EXPORTED FUNCTIONS ------------------------------------------------------

# Exploring and mapping clinical codes ------------------------------------

#' Get codes that start with...
#'
#' This is case \emph{sensitive} (important for read codes especially).
#'
#' @param codes character. A vector of code strings to search for matching
#'   codes.
#' @param code_type character. The type of clinical code system to be searched.
#'   Must be one of \code{read2}, \code{read3}, \code{icd9}, \code{icd10},
#'   \code{bnf}, \code{dmd}, \code{read2_drugs} or \code{opcs4}.
#' @param all_lkps_maps Either a named list of lookup and mapping tables (either
#'   data frames or `tbl_dbi` objects), or the path to a SQLite database
#'   containing these tables (see also [build_all_lkps_maps()] and
#'   [all_lkps_maps_to_db()]). By default, will look for a SQLite datbase called
#'   `all_lkps_maps.db` in the current working directory.
#' @param codes_only bool. If \code{TRUE}, return a character vector of
#'   \emph{unique} codes. If \code{FALSE} (default), return a data frame of all
#'   results including code descriptions (useful for manual validation).
#' @param quiet bool. Warning message if any of \code{codes} are not found for
#'   the supplied \code{code_type}.
#' @param preferred_description_only bool. Return only preferred descriptions
#'   for clinical codes with synonyms. Default value is \code{TRUE}.
#'
#' @inheritParams lookup_codes
#' @export
#' @family Clinical code lookups and mappings
codes_starting_with <- function(codes,
                            code_type,
                            all_lkps_maps = "all_lkps_maps.db",
                            codes_only = FALSE,
                            preferred_description_only = TRUE,
                            standardise_output = TRUE,
                            quiet = FALSE) {
  # validate args
  match.arg(arg = code_type,
            choices = CODE_TYPE_TO_LKP_TABLE_MAP$code)

  # connect to database file path
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  }

  assertthat::assert_that(
    is.character(codes),
    msg = "Error! `codes` must be a character vector"
  )

  assertthat::assert_that(is.logical(codes_only),
                          msg = "`code_only` must be either 'TRUE' or 'FALSE'")

  assertthat::assert_that(!(codes_only & standardise_output),
                          msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`")

  # TODO check all sheets are present
  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  # determine code column for lookup sheet
  code_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_table,
                                       column = "code_col")

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_col_for_lookup_sheet(lookup_sheet = lkp_table,
                             column = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <- get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # reformat codes - escape '.', prefix with anchor and append '.*'
  codes <- stringr::str_replace_all(codes, pattern = "\\.", replacement = "\\\\.")
  codes <-  paste0("^", codes, ".*")

  # combine into single string, separated by "|"
  codes <- stringr::str_c(codes, sep = "", collapse = "|")

  # get children (filter for codes which match ANY of those in `codes` arg)
  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::collect() %>%
    dplyr::filter(stringr::str_detect(.data[[code_col]],
                                      pattern = stringr::regex(codes,
                                                               ignore_case = FALSE)))

  # filter for preferred code descriptions only if requested
  if (preferred_description_only & !is.na(preferred_description_col)) {
    result <- result %>%
      dplyr::filter(.data[[preferred_description_col]] == preferred_description_code)
  }

  # return result
  if (nrow(result) == 0) {
    message("No matching codes found. Returning `NULL`")
    return(NULL)
  }

  # TO DELETE - no need for warning message when searching for codes starting with <x>

  else {
    #   if (quiet == FALSE) {
    #     warning_if_codes_not_found(
    #       codes = codes_raw,
    #       code_type = code_type,
    #       search_col = all_lkps_maps[[lkp_table]] %>%
    #         dplyr::collect() %>%
    #         .[[code_col]]
    #     )
    #   }

    # return either unique codes only, or df including code descriptions
    if (codes_only) {
      return(unique(result[[code_col]]))
    } else if (standardise_output) {
      # Note, not all mapping sheets in UKB resource 592 contain descriptions
      # (e.g. 'read_v2_icd9'). Therefore need to use `lookup_codes` if
      # `standardise_output` is `TRUE`
      codes <- unique(result[[code_col]])

      return(
        lookup_codes(
          codes = codes,
          code_type = code_type,
          all_lkps_maps = all_lkps_maps,
          preferred_description_only = preferred_description_only,
          quiet = quiet
        )
      )
    } else {
      return(result)
    }
  }
}

#' Look up descriptions for clinical codes
#'
#' Returns a data frame including descriptions for the codes of interest
#'
#' @param codes character. Vector of codes to lookup
#' @param standardise_output bool. If \code{TRUE} (default), outputs a data
#'   frame with columns named 'code', 'description' and 'code_type'. Otherwise
#'   returns a data frame with all columns for the relevant lookup sheet from
#'   (\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UK Biobank
#'   resource 592}).
#' @inheritParams codes_starting_with
#'
#' @return data frame
#' @export
#' @family Clinical code lookups and mappings
lookup_codes <- function(codes,
                         code_type,
                         all_lkps_maps = "all_lkps_maps.db",
                         preferred_description_only = TRUE,
                         standardise_output = TRUE,
                         quiet = FALSE) {
  # validate args
  assertthat::assert_that(
    is.character(codes),
    msg = "Error! `codes` must be a character vector"
  )

  match.arg(arg = code_type,
            choices = CODE_TYPE_TO_LKP_TABLE_MAP$code)

  # connect to database file path
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  }

  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  # determine code column for lookup sheet
  code_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_table,
                                       column = "code_col")

  # determine description column for lookup sheet
  description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_table,
                                             column = "description_col")

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_table,
                                                        column = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <- get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # lookup - filter lookup sheet for codes
  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::filter(.data[[code_col]] %in% codes) %>%
    dplyr::collect()

  # filter for preferred code descriptions only if requested
  if (!is.null(preferred_description_only)) {
    if (preferred_description_only &
        !is.na(preferred_description_col)) {
      result <- result %>%
        dplyr::filter(.data[[preferred_description_col]] == preferred_description_code)
    }
  }

  # standardise output if requested
  if (standardise_output) {
    result <- standardise_output_fn(result,
                                    lkp_table = lkp_table,
                                    code_col = code_col,
                                    description_col = description_col,
                                    code_type = code_type)
  }

  # return result
  if (nrow(result) == 0) {
    message("No matching codes found. Returning `NULL`")
    return(NULL)
  } else {
    # warning if any `codes` not present in `from_col`
    # TODO - warning if duplicates found in `codes`
    if (quiet == FALSE) {
      warning_if_codes_not_found(codes = codes,
                                 code_type = code_type,
                                 search_col = all_lkps_maps[[lkp_table]] %>%
                                   dplyr::collect() %>%
                                   .[[code_col]])
    }

    # return either unique codes only, or df including code descriptions
    return(result)
  }
}


#' Search for codes that match a description
#'
#' Returns a data frame with clinical codes that match the supplied regular
#' expression. Ignores case by default.
#'
#' @param reg_expr a regular expression to search for
#' @inheritParams stringr::regex
#' @inheritParams codes_starting_with
#'
#' @return data frame by default, or a character vector of codes if
#'   \code{codes_only} is \code{TRUE}.
#' @export
code_descriptions_like <- function(reg_expr,
                                      code_type,
                                      all_lkps_maps = "all_lkps_maps.db",
                                      ignore_case = TRUE,
                                      codes_only = FALSE,
                                      preferred_description_only = TRUE,
                                      standardise_output = TRUE) {
  # validate args
  match.arg(arg = code_type,
            choices = CODE_TYPE_TO_LKP_TABLE_MAP$code)

  # connect to database file path
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  }

  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  # determine code and description columns for lookup sheet
  code_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_table,
                                              column = "code_col")

  description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_table,
                                       column = "description_col")

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_table,
                                                        column = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <- get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # search for codes
  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::collect() %>%
    dplyr::filter(stringr::str_detect(
      string = .data[[description_col]],
      pattern = stringr::regex(pattern = reg_expr,
                               ignore_case = ignore_case)
    ))

  # filter for preferred code descriptions only if requested
  if (!is.null(preferred_description_only)) {
    if (preferred_description_only &
        !is.na(preferred_description_col)) {
      result <- result %>%
        dplyr::filter(.data[[preferred_description_col]] == preferred_description_code)
    }
  }

  # standardise output, if requested
  if (standardise_output) {
    result <- standardise_output_fn(result,
                                    lkp_table = lkp_table,
                                    code_col = code_col,
                                    description_col = description_col,
                                    code_type = code_type)

    code_col <- "code"
  }

  # return result
  if (nrow(result) == 0) {
    message("No matching codes found. Returning `NULL`")
    return(NULL)
  } else {
    if (codes_only) {
      return(
        unique(result[[code_col]])
      )
    } else {
      return(result)
    }
  }
}


#' Map clinical codes from one coding system to another
#'
#' Uses the code mapping file provided by UK Biobank
#' (\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592}).
#'
#' The values for arguments \code{from} and \code{to} must be one of
#' \code{read2}, \code{read3}, \code{icd9}, \code{icd10}, \code{bnf},
#' \code{dmd}, \code{read2_drugs} or \code{opcs4}.
#'
#' @param codes A character vector of codes to be mapped.
#' @param from Coding system that \code{codes} belong to.
#' @param to Coding system to map \code{codes} to.
#' @param quiet bool. Warning message if any of \code{codes} are not found for
#'   the code type being mapped from.
#' @param preferred_description_only bool. Return only preferred descriptions
#'   for clinical codes with synonyms. Can only be \code{TRUE} if
#'   \code{standardise_output} is also \code{TRUE}. Default value is
#'   \code{NULL}.
#' @inheritParams codes_starting_with
#' @inheritParams lookup_codes
#'
#' @export
#' @family Clinical code lookups and mappings
map_codes <- function(codes,
                      from,
                      to,
                      all_lkps_maps = "all_lkps_maps.db",
                      codes_only = FALSE,
                      standardise_output = TRUE,
                      quiet = FALSE,
                      preferred_description_only = NULL) {
  # validate args
  ## connect to database file path
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  }

  validate_all_lkps_maps()

  assertthat::assert_that(
    is.character(codes),
    msg = "Error! `codes` must be a character vector"
  )

  assertthat::assert_that(is.logical(codes_only),
                          msg = "`code_only` must be either 'TRUE' or 'FALSE'")

  assertthat::assert_that(!(codes_only & standardise_output),
                          msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`")

  if (!is.null(preferred_description_only)) {
    assertthat::assert_that(!(
      preferred_description_only == TRUE & standardise_output == FALSE
    ),
    msg = "Error! `preferred_description_only` cannot be `TRUE` unless `standardise_output` is also `TRUE`")
  }

  # check mapping args and get required details - mapping_table, from_col and
  # to_col
  mapping_params <- check_mapping_args(from = from,
                                       to = to)

  from_col <- mapping_params$from_col
  to_col <- mapping_params$to_col
  mapping_table <- mapping_params$mapping_table

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <- get_value_for_mapping_sheet(mapping_table = mapping_table,
                                                        value = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <- get_value_for_mapping_sheet(mapping_table = mapping_table,
                                                              value = "preferred_code")
  }

  # do mapping


  result <- all_lkps_maps[[mapping_table]] %>%
    dplyr::filter(.data[[from_col]] %in% codes) %>%
    dplyr::collect()

  # return result
  if (nrow(result) == 0) {
    message("\nNo codes found after mapping. Returning `NULL`")
    return(NULL)
  } else {
    # warning if any `codes` not present in `from_col`
    # TODO - warning if duplicates found in `codes`
    if (quiet == FALSE) {
      warning_if_codes_not_found(
        codes = codes,
        code_type = from,
        search_col = all_lkps_maps[[mapping_table]] %>%
          dplyr::collect() %>%
          .[[from_col]]
      )
    }

    # return either unique codes only, or df including descriptions
    if (codes_only) {
      result <- unique(result[[to_col]])

      return(result)
    } else if (standardise_output) {
      # Note, not all mapping sheets in UKB resource 592 contain descriptions
      # (e.g. 'read_v2_icd9'). Therefore need to use `lookup_codes` if
      # `standardise_output` is `TRUE`

      codes <- unique(result[[to_col]])

      return(
        lookup_codes(
          codes = codes,
          code_type = to,
          all_lkps_maps = all_lkps_maps,
          preferred_description_only = preferred_description_only,
          quiet = quiet
        )
      )
    } else {
      return(result)
    }
  }
}

#' Get a 'from-to' mapping data frame
#'
#' Returns a data frame with 'from' and 'to' columns for a specified pair of
#' coding systems.
#'
#' @param from A clinical coding system to map from.
#' @param to A clinical coding system to map to.
#' @inheritParams codes_starting_with
#'
#' @return A data frame with column names 'from' and 'to'.
#' @export
#'
#' @family Clinical code lookups and mappings
get_mapping_df <- function(from,
                        to,
                        all_lkps_maps = "all_lkps_maps.db") {
  # validate args
  check_mapping_args(from = from,
                     to = to)

  # connect to database file path
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  }

  # get mapping sheet, from and to cols
  # check mapping args and get required details - mapping_table, from_col and
  # to_col
  mapping_params <- check_mapping_args(from = from,
                                       to = to)

  from_col <- mapping_params$from_col
  to_col <- mapping_params$to_col
  mapping_table <- mapping_params$mapping_table

  # get just distinct combinations of from_col and to_col for mapping_table
  from_to_cols <- c(
    from_col,
    to_col
  )

  result <- all_lkps_maps[[mapping_table]] %>%
    dplyr::select(tidyselect::all_of(from_to_cols)) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    ukbwranglr:::rename_cols(
      old_colnames = from_to_cols,
      new_colnames = c("from", "to")
    )

  return(result)
}

# Utilities ---------------------------------------------------------------

#' Reformat ICD-10 codes
#'
#' The lookup sheet in
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UKB resource 592}
#' for ICD-10 ("icd10_lkp") has a column called "ALT_CODE", which is an
#' alternative format for ICD-10 codes. This is the format used in the mapping
#' sheets for this resource, as well as in
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270}{Field ID 41270}.
#' This function converts from one format to the other.
#'
#' @param icd10_codes character vector of ICD-10 codes
#' @param input_icd10_format character. Must be either "ICD10_CODE" or
#'   "ALT_CODE".
#' @param output_icd10_format character. Must be either "ICD10_CODE" or
#'   "ALT_CODE".
#' @param strip_x logical. If \code{TRUE}, any 3 character ICD10 codes in
#'   'ALT_CODE' format with no children (e.g. 'A38X', Scarlet fever) will have
#'   the last 'X' removed. Default value is \code{FALSE}.
#' @inheritParams codes_starting_with
#'
#' @return character vector of ICD-10 codes, reformatted as specified by
#'   \code{output_icd10_format}.
#' @export
#' @family Clinical code lookups and mappings
reformat_icd10_codes <- function(icd10_codes,
                                 all_lkps_maps,
                                 input_icd10_format = "ICD10_CODE",
                                 output_icd10_format = "ALT_CODE",
                                 strip_x = FALSE) {
  # validate args
  match.arg(arg = input_icd10_format,
            choices = c("ICD10_CODE", "ALT_CODE"))

  match.arg(arg = output_icd10_format,
            choices = c("ICD10_CODE", "ALT_CODE"))


  assertthat::assert_that(input_icd10_format != output_icd10_format,
                          msg = "Error for `reformat_icd10_codes()`! Input and output icd10 formats cannot be the same")

  # TO DELETE reformat icd10 codes
  # result <- all_lkps_maps$icd10_lkp %>%
  #   dplyr::filter(.data[[input_icd10_format]] %in% icd10_codes) %>%
  #   dplyr::collect()

  icd10_lkp <- all_lkps_maps$icd10_lkp %>%
    dplyr::collect()

    # some ICD10_CODE values have multiple associated ALT_CODEs - the ALT_CODEs
    # include modifiers 4 and 5. (e.g. ICD-10 codes M00, M77, M07, M72, M65, S52
    # or S72). Need to take only ony ALT_CODE in these cases (slice(1L) takes
    # the first of each group, which should be the ALT_CODE without any
    # modifiers)

    # this is not the case the other way around (ALT_CODE to ICD10_CODE),
    # however running this step does not prolong the function time significantly

    # note, there are a couple of NA values as the last line of the icd10_lkp
    # sheet contains a statement re permissions
    icd10_lkp <- icd10_lkp %>%
      dplyr::filter(!is.na(.data[[input_icd10_format]])) %>%
      dplyr::group_by(.data[[input_icd10_format]]) %>%
      dplyr::slice(1L) %>%
      dplyr::ungroup()

  if (strip_x) {
    icd10_lkp <- strip_x_from_alt_icd10(df = icd10_lkp,
                           alt_icd10_code_col = "ALT_CODE")
  }

  dict <- icd10_lkp[[output_icd10_format]]
  names(dict) <- icd10_lkp[[input_icd10_format]]

  result <- ukbwranglr:::revalue_vector(icd10_codes,
                 dict = dict,
                 default_value = NULL,
                 suppress_warnings = TRUE)

  # TO DELETE
  # result <- result %>%
  #   .[[output_icd10_format]] %>%
  #   unique()

  if (rlang::is_empty(result)) {
    warning("Warning! `reformat_icd10_codes()` found no icd10 code matches. Returning NULL")
    return(NULL)
  } else(
    return(result)
  )
}

check_icd10_codes_are_alt_code_format <- function(icd10_codes,
                                                  all_lkps_maps,
                                                  check_3_char_codes_ending_X = TRUE) {
  icd10_lkp <- all_lkps_maps$icd10_lkp %>%
    dplyr::collect()

  # check if there are any codes that should end with 'X' in ALT_CODE format (e.g. Scarlet fever, "A38X")
  if (check_3_char_codes_ending_X) {
    icd10_format_3_char_codes <- icd10_lkp %>%
      dplyr::filter(stringr::str_detect(.data[["ALT_CODE"]],
                                        pattern = "X$")) %>%
      dplyr::pull(.data[["ICD10_CODE"]])

    icd10_codes_3_char_codes <- subset(icd10_codes,
                                       icd10_codes %in% icd10_format_3_char_codes)

    assertthat::assert_that(
      length(icd10_codes_3_char_codes) == 0,
      msg = paste0(
        "The following ",
        length(icd10_codes_3_char_codes),
        " 3 character ICD-10 codes should end with 'X'. Try converting to 'ALT_CODE' format with `reformat_icd10_codes` and `strip_x = FALSE`. Codes to review: ",
        stringr::str_c(icd10_codes_3_char_codes,
                       sep = "",
                       collapse = ", ")
      )
    )
  }

  # by this point, all ICD10 codes should be ALT_CODE format. Check all codes exist
  unrecognised_icd10_codes <- subset(icd10_codes,
                                     !icd10_codes %in% icd10_lkp$ALT_CODE)

  if (length(unrecognised_icd10_codes) > 0) {
    error_message <- paste0("Error! ",
                            length(unrecognised_icd10_codes),
                            " codes are not recognised as ICD-10 in ALT_CODE format: ",
                            stringr::str_c(unrecognised_icd10_codes,
                                           sep = "",
                                           collapse = ", "))

    # basic test - if `icd10_codes` contains '.', then it may be an ICD10 code,
    # but not in ALT_CODE format
    codes_with_dot <- subset(
      unrecognised_icd10_codes,
      stringr::str_detect(unrecognised_icd10_codes,
                          pattern = "\\.")
    )

    if (length(codes_with_dot) > 0) {
      error_message <- paste0(error_message,
                              " .\n",
                              length(codes_with_dot),
                              " codes contain a '.'. If definitely ICD-10, try converting these to 'ALT_CODE' format with `reformat_icd10_codes`: ",
                              stringr::str_c(codes_with_dot,
                                             sep = "",
                                             collapse = ", "))
    }

    stop(error_message)
  }

  # ...if all checks pass
  invisible(TRUE)
}

# PRIVATE FUNCTIONS -------------------------------------------------------


#' Helper function for \code{\link{map_codes}}
#'
#' Returns name of the appropriate mapping sheet from the UKB code mappings
#' excel file (resource 592) for mapping from one clinical coding system to
#' another.
#'
#' @param from character
#' @param to character
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_from_to_mapping_sheet <- function(from, to) {
  CLINICAL_CODE_MAPPINGS_MAP[
    (
      CLINICAL_CODE_MAPPINGS_MAP[["from"]] == from &
        CLINICAL_CODE_MAPPINGS_MAP[["to"]] == to
    ),
  ][["mapping_table"]]
}

#' Helper function for \code{\link{map_codes}}
#'
#' Returns the requested value for a 'mapping_table' in
#' \code{CLINICAL_CODE_MAPPINGS_MAP}.
#'
#' @param mapping_table character
#' @param value character. column name from
#'   \code{CLINICAL_CODE_MAPPINGS_MAP} (apart from
#'   "mapping_table").
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_value_for_mapping_sheet <- function(mapping_table,
                                        value) {

  # validate args
  match.arg(
    arg = mapping_table,
    choices = CLINICAL_CODE_MAPPINGS_MAP[["mapping_table"]]
  )

  match.arg(
    arg = value,
    choices = subset(
      names(CLINICAL_CODE_MAPPINGS_MAP),
      subset = names(CLINICAL_CODE_MAPPINGS_MAP) != "mapping_table"
    )
  )

  # return specified `value`
  CLINICAL_CODE_MAPPINGS_MAP[
    CLINICAL_CODE_MAPPINGS_MAP[["mapping_table"]] == mapping_table,
  ][[value]]
}

#' Get name of lookup sheet for a clinical code system
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{codes_starting_with}}
#'
#' @param code_type character
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_lookup_sheet <- function(code_type) {
  # validate args
  match.arg(code_type,
            choices = CODE_TYPE_TO_LKP_TABLE_MAP$code)

  # get lookup sheet
  CODE_TYPE_TO_LKP_TABLE_MAP %>%
    dplyr::filter(.data[["code"]] == code_type) %>%
    .$lkp_table
}

#' Get name of code, description or preferred synonym column for a lookup sheet
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{codes_starting_with}}
#'
#' @param lookup_sheet character
#' @param column character
#'
#' @return character (scalar)
#'
#' @family Clinical code lookups and mappings
#' @noRd
get_col_for_lookup_sheet <- function(lookup_sheet,
                                     column) {

  # validate args
  match.arg(arg = lookup_sheet,
            choices = CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table)

  match.arg(arg = column,
            choices = c("code_col", "description_col", "preferred_synonym_col"))

  # get column name for lookup sheet
  CODE_TYPE_TO_LKP_TABLE_MAP %>%
    dplyr::filter(.data[["lkp_table"]] == lookup_sheet) %>%
    .[[column]]
}

#' Get preferred description code for a lookup sheet
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{codes_starting_with}}
#'
#' @param lookup_sheet character
#'
#' @return character (scalar)
#'
#' @family Clinical code lookups and mappings
#' @noRd
get_preferred_description_code_for_lookup_sheet <- function(lookup_sheet) {
  # validate args
  match.arg(arg = lookup_sheet,
            choices = CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table)

  # get preferred description code for lookup sheet
  CODE_TYPE_TO_LKP_TABLE_MAP %>%
    dplyr::filter(.data[["lkp_table"]] == lookup_sheet) %>%
    .[["preferred_code"]]
}

#' Helper function - generate warning message
#'
#' Raises a warning if searched codes do not exist for a clinical code system.
#'
#' @param codes character vector. The codes being searched for.
#' @param code_type character. Type of code
#' @param search_col character vector. The column of codes (e.g. read2, ICD etc)
#'   being searched in.
#'
#' @return informative warning message, or nothing
#' @noRd
#' @family Clinical code lookups and mappings
warning_if_codes_not_found <-
  function(codes, code_type, search_col) {
    if (any(!codes %in% search_col)) {
      missing_codes <- unique(subset(codes, !codes %in% search_col))

      warning(
        "Warning! The following codes were not found for ",
        code_type,
        ": '",
        stringr::str_c(missing_codes, sep = "", collapse = "', '"),
        "'"
      )
    }
  }


# TO DELETE?

# Helper function for exploring and mapping clinical codes
# return_results_clinical_codes <- function(result,
#                                           all_lkps_maps,
#                                           code_col,
#                                           codes_only,
#                                           standardise_output,
#                                           quiet,
#                                           preferred_description_only) {
#   if (nrow(result) == 0) {
#     message("\nNo codes found after mapping. Returning `NULL`")
#     return(NULL)
#   } else {
#     # warning if any `codes` not present in `from_col`
#     # TODO - warning if duplicates found in `codes`
#     if (quiet == FALSE) {
#       warning_if_codes_not_found(
#         codes = codes,
#         code_type = from,
#         search_col = all_lkps_maps[[mapping_table]] %>%
#           dplyr::collect() %>%
#           .[[from_col]]
#       )
#     }
#
#     # return either unique codes only, or df including descriptions
#     if (codes_only) {
#       result <- unique(result[[to_col]])
#
#       return(result)
#     } else if (standardise_output) {
#       # Note, not all mapping sheets in UKB resource 592 contain descriptions
#       # (e.g. 'read_v2_icd9'). Therefore need to use `lookup_codes` if
#       # `standardise_output` is `TRUE`
#       if (to == "icd10") {
#         codes <-
#           reformat_icd10_codes(
#             icd10_codes = unique(result[[to_col]]),
#             all_lkps_maps = all_lkps_maps,
#             input_icd10_format = "ALT_CODE",
#             output_icd10_format = "ICD10_CODE"
#           )
#       } else {
#         codes <- unique(result[[to_col]])
#       }
#       return(
#         lookup_codes(
#           codes = codes,
#           code_type = to,
#           all_lkps_maps = all_lkps_maps,
#           preferred_description_only = preferred_description_only,
#           quiet = quiet
#         )
#       )
#     } else {
#       return(result)
#     }
#   }
# }

#' Reformat a dataframe of clinical codes to work with
#' \code{\link[ukbwranglr]{extract_phenotypes}}
#'
#' A utility function that helps reformat the output from \code{\link{map_codes}}
#' or \code{\link{lookup_codes}} to work with
#' \code{\link[ukbwranglr]{extract_phenotypes}}. See also output
#' from \code{\link[ukbwranglr]{example_clinical_codes}} for an example of
#' the format that this function will output.
#'
#' @param standardised_codelist a data frame with column names "code",
#'   "description", "code_type".
#' @param code_type character (scalar). The clinical code type e.g. "read2"
#' @param disease character (scalar), e.g. "Secondary polycythaemia"
#' @param disease_category character (scalar). The subcategory of \code{disease}
#'   that these codes belong to e.g. "Diagnosis of Secondary polycythaemia".
#' @param author character (scalar), e.g. "caliber".
#'
#' @return A data frame with the following column names: 'disease',
#'   'description', 'category', 'code_type', 'code' and 'author'.
#' @noRd
reformat_standardised_codelist <- function(standardised_codelist,
                                           code_type,
                                           disease,
                                           disease_category,
                                           author) {
  # validate args
  assertthat::assert_that(any(
    class(standardised_codelist) %in% c("data.frame", "data.table", "tbl_df")
  ),
  msg = "Error! standardised_codelist must be a data frame (or tibble/data table")
  assertthat::is.string(code_type)
  assertthat::is.string(disease)
  assertthat::is.string(disease_category)
  assertthat::is.string(author)

  match.arg(code_type,
            choices = ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding)

  assertthat::assert_that(all(
    names(standardised_codelist) == c("code", "description", "code_type")
  ),
  msg = "Error! `standardised_codelist` must be a data frame with the following headings: 'code', 'description', 'code_type'")

  assertthat::assert_that(
    all(
      standardised_codelist$code_type %in% unique(ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding)
    ),
    msg = paste0(
      "Error! `standardised_codelist$code_type` contains unrecognised code types. Recognised code types: ",
      stringr::str_c(
        unique(ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding),
        sep = "",
        collapse = ", "
      )
    )
  )

  # reformat to work with `extract_phenotypes()`
  standardised_codelist <- standardised_codelist %>%
    dplyr::mutate(
      "code_type" = code_type,
      "disease" = disease,
      "category" = disease_category,
      "author" = author,
    ) %>%
    dplyr::select(.data[["disease"]],
                  .data[["description"]],
                  .data[["category"]],
                  .data[["code_type"]],
                  .data[["code"]],
                  .data[["author"]])

  return(standardised_codelist)
}

standardise_output_fn <- function(df, lkp_table, code_col, description_col, code_type) {
  names(df)[which(names(df) == code_col)] <- "code"
  names(df)[which(names(df) == description_col)] <- "description"

  # ICD-10 has a modifier column e.g. "E10" = "Type 1 diabetes mellitus",
  # whereas "E10.0" = "Type 1 diabetes mellitus with coma". "With coma" is
  # contained in the modifier columns "MODIFIER-4". See 'S27' for an example
  # code where additional description is contained in the "MODIFER-5" column.
  # The returned "description" column from `standardise_output == TRUE`
  # therefore combines the 'DESCRIPTION' column with one of these 2 columns
  # (whichever is not NA).

  # Also, remove "."
  if (lkp_table == "icd10_lkp") {
    df$description <- dplyr::case_when(
      !is.na(df$MODIFIER_4) ~ paste(df$description, df$MODIFIER_4),
      !is.na(df$MODIFIER_5) ~ paste(df$description, df$MODIFIER_5),
      TRUE ~ df$description
    )

    df <- strip_x_from_alt_icd10(df = df,
                                 alt_icd10_code_col = "code")
  }

  # return code, description and code_type cols only
  df <- df[c("code", "description")]
  df[["code_type"]] <- code_type

  return(df)
}

strip_x_from_alt_icd10 <- function(df,
                                   alt_icd10_code_col) {
  # used for 3 character ICD10 codes in 'AlT_CODE' format, which end with 'X' if there are no children codes
  df[[alt_icd10_code_col]] <-
    stringr::str_remove(df[[alt_icd10_code_col]],
                        "X$")

  return(df)
}

## Validation helpers ---------------------------

check_mapping_args <- function(from,
                               to) {
  match.arg(arg = from,
            choices = CODE_TYPE_TO_LKP_TABLE_MAP$code)
  # choices = CLINICAL_CODE_MAPPINGS_MAP$from)

  match.arg(arg = to,
            choices = CODE_TYPE_TO_LKP_TABLE_MAP$code)
  # choices = CLINICAL_CODE_MAPPINGS_MAP$to)

  assertthat::assert_that(!from == to,
                          msg = "Error! `from` and `to` args cannot be the same")

  # get appropriate mapping sheet
  swap_mapping_cols <- FALSE
  mapping_table <- get_from_to_mapping_sheet(from = from, to = to)

  # if above returns `character(0)`, try to map the other way
  if (rlang::is_empty(mapping_table)) {
    swap_mapping_cols <- TRUE
    mapping_table <- get_from_to_mapping_sheet(from = to, to = from)
  }

  # if still returns `character(0)`, error
  if (rlang::is_empty(mapping_table)) {
    stop("Error! Invalid (or unavailable) code mapping request")
  } else if (swap_mapping_cols) {
    warning("Warning! No mapping sheet available for this request. Attempting to map anyway using: ", mapping_table)
  }

  # get from_col and to_col column names for mapping sheet
  # swap if appropriate
  if (swap_mapping_cols) {
    from_col <-
      get_value_for_mapping_sheet(mapping_table = mapping_table,
                                  value = "to_col")
    to_col <-
      get_value_for_mapping_sheet(mapping_table = mapping_table,
                                  value = "from_col")
  } else {
    from_col <-
      get_value_for_mapping_sheet(mapping_table = mapping_table,
                                  value = "from_col")
    to_col <-
      get_value_for_mapping_sheet(mapping_table = mapping_table,
                                  value = "to_col")
  }

  # return result
  return(list(
    from_col = from_col,
    to_col = to_col,
    mapping_table = mapping_table
  ))
}

check_all_lkps_maps_path <- function(file_path) {
  # check file exists
  assertthat::assert_that(file.exists(file_path),
                          msg = paste0("Error! No file found at ",
                                       file_path))

  # check file ends with '.db'
  assertthat::assert_that(stringr::str_detect(file_path,
                                              ".+\\.db"))

  # return con object if tests pass
  DBI::dbConnect(RSQLite::SQLite(), file_path)
}
