
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
#' @param preferred_description_only bool. Return only preferred descriptions
#'   for clinical codes with synonyms. Default value is \code{TRUE}.
#' @param col_filters A named list where each name in the list refers to the
#'   name of a lookup or mapping table. Each item is also a named list, where
#'   the names refer to column names in the corresponding table, and the items
#'   are vectors of values to filter for. For example, `list(my_lookup_table =
#'   list(colA = c("A", "B"))` will result in `my_lookup_table` being filtered
#'   for rows where `colA` is either 'A' or 'B'. Uses `default_col_filters()` by
#'   default. Set to `NULL` to remove all filters.
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
                            col_filters = default_col_filters()) {
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

  # filter on `filter_cols` parameters
  if (!is.null(filter_cols)) {
    result <- filter_cols(df = result,
                          df_name = lkp_table,
                          col_filters = col_filters)
  }

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

  else {

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
          unrecognised_codes = "error"
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
#' @param unrecognised_codes Either 'error' (default) or 'warning'. If any input
#'   `codes` are unrecognised, then either an error or warning will be raised.
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
                         unrecognised_codes = "error",
                         col_filters = default_col_filters()) {
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

  # filter on `filter_cols` parameters
  if (!is.null(filter_cols)) {
    result <- filter_cols(df = result,
                          df_name = lkp_table,
                          col_filters = col_filters)
  }

  # check for unrecognised codes
  missing_codes <- subset(codes,
                          !codes %in% result[[code_col]])

  handle_unrecognised_codes(unrecognised_codes = unrecognised_codes,
                            missing_codes = missing_codes,
                            code_type = code_type)

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
                                      standardise_output = TRUE,
                                   col_filters = default_col_filters()) {
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

  # filter on `filter_cols` parameters
  if (!is.null(filter_cols)) {
    result <- filter_cols(df = result,
                          df_name = lkp_table,
                          col_filters = col_filters)
  }

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
#' @param unrecognised_codes Either 'error' (default) or 'warning'. If any input
#'   `codes` are unrecognised for the coding system being mapped from, then
#'   either an error or warning will be raised.
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
                      unrecognised_codes = "error",
                      preferred_description_only = NULL,
                      col_filters = default_col_filters()) {
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

  # filter on `filter_cols` parameters
  if (!is.null(filter_cols)) {
    result <- filter_cols(df = result,
                          df_name = mapping_table,
                          col_filters = col_filters)
  }

  # check for unrecognised codes
  missing_codes <- subset(codes,
                          !codes %in% result[[from_col]])

  handle_unrecognised_codes(unrecognised_codes = unrecognised_codes,
                            missing_codes = missing_codes,
                            code_type = from)

  # return result
  if (nrow(result) == 0) {
    message("\nNo codes found after mapping. Returning `NULL`")
    return(NULL)
  } else {

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
          unrecognised_codes = "error"
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
#' @param rename_from_to Optionally supply a named vector to rename the 'from'
#'   and 'to' columns. For example `c(from = "original_codes", to =
#'   "new_codes")`. By default, the columns will be named using the values for
#'   `from` and `to` arguments.
#' @param na.rm If `TRUE` (default), remove any rows with `NA` from the returned
#'   mapping data frame. The mapping tables may sometimes include `NA` values to
#'   explicitly show which 'from' codes have not been mapped.
#'
#' @return A data frame with column names 'from' and 'to'.
#' @export
#'
#' @family Clinical code lookups and mappings
get_mapping_df <- function(from,
                        to,
                        all_lkps_maps = "all_lkps_maps.db",
                        rename_from_to = NULL,
                        na.rm = TRUE,
                        col_filters = default_col_filters()) {
  # validate args
  check_mapping_args(from = from,
                     to = to)

  rename_from_to_error_msg <- "Error! `rename_from_to` should be a named character vector of length 2, with names 'from' and 'to'"

  if (!is.null(rename_from_to)) {
    assertthat::assert_that(is.character(rename_from_to) &&
                              (length(rename_from_to) == 2) &&
                              all(c("from", "to") %in% names(rename_from_to)),
                            msg = rename_from_to_error_msg)
  }

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
    dplyr::collect()

  # filter on `filter_cols` parameters
  if (!is.null(filter_cols)) {
    result <- filter_cols(df = result,
                          df_name = mapping_table,
                          col_filters = col_filters)
  }

  # remove rows with `NA` values
  if (na.rm) {
    result <- tidyr::drop_na(result)
  }

  # rename
  if (!is.null(rename_from_to)) {
    new_from_to_cols <- c(rename_from_to['from'], rename_from_to['to'])
  } else {
    new_from_to_cols <- c(from, to)
  }

  result <- result %>%
    ukbwranglr:::rename_cols(
      old_colnames = from_to_cols,
      new_colnames = new_from_to_cols
    )

  return(result)
}

# Utilities ---------------------------------------------------------------

#' Reformat ICD-10 codes
#'
#' The lookup sheet in
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UKB resource 592}
#' for ICD-10 ("icd10_lkp") has a column called `ALT_CODE`, which is an
#' alternative format for ICD-10 codes. This is the format used in the mapping
#' sheets for this resource, as well as in
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270}{Field ID 41270}.
#' *Note however that in contrast to the ICD10 lookup and mapping sheets,
#' undivided 3 character codes do not have an 'X' appended in the UK Biobank
#' dataset*. This function converts from one format to the other, with the
#' option to strip the final 'X' from undivided 3 character codes if converting
#' to `ALT_CODE` format.
#'
#' @param icd10_codes character vector of ICD-10 codes
#' @param input_icd10_format character. Must be either "ICD10_CODE" or
#'   "ALT_CODE".
#' @param output_icd10_format character. Must be either "ICD10_CODE" or
#'   "ALT_CODE".
#' @inheritParams codes_starting_with
#' @inheritParams lookup_codes
#' @param strip_x If `TRUE` and converting to `ALT_CODE` format, 'X' is removed
#'   from the end of undivided 3 character codes (default is `FALSE`).
#'
#' @return character vector of ICD-10 codes, reformatted as specified by
#'   \code{output_icd10_format}.
#' @export
#' @family Clinical code lookups and mappings
reformat_icd10_codes <- function(icd10_codes,
                                 all_lkps_maps = "all_lkps_maps.db",
                                 input_icd10_format = "ICD10_CODE",
                                 output_icd10_format = "ALT_CODE",
                                 unrecognised_codes = "error",
                                 strip_x = FALSE) {
  # validate args
  match.arg(arg = input_icd10_format,
            choices = c("ICD10_CODE", "ALT_CODE"))

  match.arg(arg = output_icd10_format,
            choices = c("ICD10_CODE", "ALT_CODE"))


  assertthat::assert_that(input_icd10_format != output_icd10_format,
                          msg = "Error for `reformat_icd10_codes()`! Input and output icd10 formats cannot be the same")

  if (strip_x & (output_icd10_format == "ICD10_CODE")) {
    stop("`strip_x` can only be `TRUE` if `output_icd10_format` is 'ALT_CODE'")
  }

  # connect to database file path
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  }

  validate_all_lkps_maps()

  icd10_mapping_df <- all_lkps_maps$icd10_lkp %>%
    dplyr::filter(.data[[input_icd10_format]] %in% icd10_codes) %>%
    dplyr::select(tidyselect::all_of(c(
      "ICD10_CODE",
      "ALT_CODE"
    ))) %>%
    dplyr::collect()

  # handle any unrecognised codes
  missing_codes <- subset(icd10_codes,
                          !icd10_codes %in% icd10_mapping_df[[input_icd10_format]])

  handle_unrecognised_codes(unrecognised_codes = unrecognised_codes,
                            missing_codes = missing_codes,
                            code_type = input_icd10_format)

    # Note: some ICD10_CODE values have multiple associated ALT_CODEs - these
    # include a modifier description in `MODIFIER_5` (e.g. ICD-10 codes M00,
    # M77, M07, M72, M65, S52 or S72). The number of output codes may therefore
    # be larger/smaller than the input number. Raise informative message if this
    # is is the case.

  non_unique_icd10_codes <- icd10_mapping_df %>%
    dplyr::count(.data[["ICD10_CODE"]]) %>%
    dplyr::filter(.data[["n"]] > 1) %>%
    dplyr::pull(.data[["ICD10_CODE"]])

  if (length(non_unique_icd10_codes) > 0) {
    input_icd10_not_1_to_1_mapping <- icd10_mapping_df %>%
      dplyr::filter(.data[["ICD10_CODE"]] %in% non_unique_icd10_codes) %>%
      dplyr::pull(.data[[input_icd10_format]]) %>%
      unique()

    more_or_fewer_returned_codes <- ifelse(input_icd10_format == "ICD10_CODE",
                                        yes = "*more*",
                                        no = "*fewer*")

    message(
      "The following ",
      length(input_icd10_not_1_to_1_mapping),
      " input ICD10 codes do not have a 1-to-1 mapping: '",
      stringr::str_c(
        input_icd10_not_1_to_1_mapping,
        sep = "",
        collapse = "', '"
      ),
      "'. There will therefore be ",
      more_or_fewer_returned_codes,
      " output than input codes"
    )
  }

  # get requested icd10 format
  result <- unique(icd10_mapping_df[[output_icd10_format]])

  # optionally remove appended 'X' for undivided 3 character codes in `ALT_CODE` format
  if (strip_x & (output_icd10_format == "ALT_CODE")) {
    message("Removing 'X' from any undivided 3 character ICD10 codes")
      result <- stringr::str_remove(result,
                                    "X$")
    }

  # return result
  return(result)
}

#' Default filtering parameters for lookup and mapping tables.
#'
#' To be used as `filter_cols` argument in 'Clinical code lookups and mappings'
#' functions. Returns a named list where each name in the list refers to the
#' name of a lookup or mapping table. Each item is also a named list, where the
#' names refer to column names in the corresponding table, and the items are
#' vectors of values to filter for.
#'
#' @return A named list.
#' @export
#'
#' @family Clinical code lookups and mappings
#' @examples
#' default_col_filters()
default_col_filters <- function() {
  list(
    read_ctv3_icd10 = list(
      mapping_status = c("E", "G", "D"),
      refine_flag = c("C", "P")
    )
  )
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

#' Helper function - raise error or warning if unrecognised codes are present
#'
#' Raises an error or warning for unrecognised codes.
#'
#' @param unrecognised_codes Either 'error' or 'warning'. Determines how to
#'   handle unrecognised codes
#' @param missing_codes character vector of unrecognised codes.
#' @param code_type The type of clinical coding system
#'
#' @return Called for side effects.
#' @noRd
#' @family Clinical code lookups and mappings
handle_unrecognised_codes <-
  function(unrecognised_codes,
           missing_codes,
           code_type) {
    match.arg(unrecognised_codes,
              choices = c("error", "warning"))

    if (length(missing_codes) > 0) {
      missing_codes_message <- paste0(
        "The following ",
        length(missing_codes),
        " codes were not found for ",
        code_type,
        ": '",
        stringr::str_c(missing_codes, sep = "", collapse = "', '"),
        "'"
      )

      switch(unrecognised_codes,
             error = stop(missing_codes_message),
             warning = warning(missing_codes_message))
    }
  }

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
  assertthat::assert_that(is.data.frame(standardised_codelist),
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

  # Some ICD-10 descriptions include a modifier e.g. "E10" = "Type 1 diabetes
  # mellitus", whereas "E10.0" = "Type 1 diabetes mellitus with coma". "With
  # coma" is contained in the modifier columns "MODIFIER-4". See 'S27' for an
  # example code where additional description is contained in the "MODIFER-5"
  # column. The returned "description" column from `standardise_output == TRUE`
  # therefore combines the 'DESCRIPTION' column with one of these 2 columns
  # (whichever is not NA). There are no codes with a modifier description in
  # both "MODIFIER_4" and "MODIFIER_5".

  if (lkp_table == "icd10_lkp") {
    df$description <- dplyr::case_when(
      !is.na(df$MODIFIER_4) ~ paste(df$description, df$MODIFIER_4),
      !is.na(df$MODIFIER_5) ~ paste(df$description, df$MODIFIER_5),
      TRUE ~ df$description
    )
  }

  # return code, description and code_type cols only
  df <- df[c("code", "description")]
  df[["code_type"]] <- code_type

  return(df)
}

#' Filter lookup/mapping table for specified values in columns
#'
#' Helper function that enables filtering of lookup/mapping tables for certain
#' values (e.g. filter a mapping table for only 'exact' code mappings). Uses an
#' `%in%` filter statement. Note that no error is currently raised if attempting
#' to filter for any values that do not exist in `df` columns.
#'
#' @param df Lookup or mapping df to be filtered
#' @param df_name Name of lookup/mapping df (e.g. "icd10_lkp").
#' @param col_filters Either `NULL` (default, in which case `df` is returned
#'   unchanged) or a named list. First level names are names of lookup/mapping
#'   tables. Each item is also a named list of vectors. Columns in `df` that
#'   match the list names are filtered for values in the corresponding vectors
#'   (using `%in%`). An error is raised
#'
#' @return A dataframe
#' @noRd
filter_cols <- function(df,
                        df_name,
                        col_filters = NULL) {
  # if `col_filters` is `NULL`, return `df` unchanged (exit early)
  if (is.null(col_filters)) {
    return(df)
  }

  # get relevant columns/filter values from `col_filters`
  col_filters <- col_filters[[df_name]]

  # if `df_name` is not present in `names(col_filters)` return `df` unchanged (exit early)
  if (is.null(col_filters)) {
    return(df)
  }

  # check that selected element of `col_filters` is a named list of vectors
  stopifnot(is.list(col_filters))
  if (is.null(names(col_filters))) {
    stop("Each item in `col_filters` must be named")
  }

  if (any(names(col_filters) == "")) {
    stop("Each item in `col_filters` must be named")
  }

  col_filters_item_types <- col_filters %>%
    purrr::map_lgl(is.vector)

  assertthat::assert_that(sum(!col_filters_item_types) == 0,
                          msg = "Each item in `col_filters` must be a vector")

  # check that column names exist in df. Raise error is any are unrecognised.
  unrecognised_colnames <- subset(names(col_filters),
                                  !names(col_filters) %in% names(df))

  if (length(unrecognised_colnames) > 0) {
    stop(
      paste0(
        "The following ",
        length(unrecognised_colnames),
        " column names specified by `col_filters` are not present in `",
        df_name,
        "`: ",
        stringr::str_c(unrecognised_colnames,
                       sep = "",
                       collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # filter `df` for specified values in each column listed by `col_filters`
  for (i in names(col_filters)) {
    col_filter_values <- col_filters[[i]]

    # check that type matches
    df_col_class <- class(df[[i]])
    col_filter_values_class <- class(col_filter_values)
    assertthat::assert_that(df_col_class %in% col_filter_values_class,
                            msg = paste0("Cannot filter column ",
                                         i,
                                         " in ",
                                         df_name,
                                         " as classes do not match. Column `",
                                         i,
                                         "`` is class ",
                                         df_col_class,
                                         ", but filter values specified by `col_filters` are of class ",
                                         col_filter_values_class))

    df <- df %>%
      dplyr::filter(.data[[i]] %in% !!col_filter_values)
  }

  # return result
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
