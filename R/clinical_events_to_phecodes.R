
# CONSTANTS ---------------------------------------------------------------

CLINICAL_EVENTS_SOURCES_MAPPED_TO_PHECODES <- c(
  # icd10
  "f40001",
  "f40002",
  "f20002_icd10",
  "f40006",
  "f41270",

  # icd9
  "f40013",
  "f41271",

  # gp read 3
  "gpc1_r3",
  "gpc2_r3",
  "gpc3_r3",
  "gpc4_r3",

  # gp read 2
  "gpc1_r2",
  "gpc2_r2",
  "gpc3_r2",
  "gpc4_r2"
)

# PUBLIC ----------------------------------------------------------------

#' Map UK Biobank clinical events to phecodes
#'
#' UK Biobank clinical events sources that are recorded in ICD10 are mapped
#' directly to phecodes, while non-ICD10 sources are mapped to phecodes via
#' ICD10.
#'
#' Maps the following UK Biobank clinical events sources to phecodes: `r stringr::str_c(CLINICAL_EVENTS_SOURCES_MAPPED_TO_PHECODES, sep = "", collapse = ", ")`.
#'
#' @inheritParams ukbwranglr::extract_phenotypes
#' @inheritParams  codes_starting_with
#' @param min_date_only If `TRUE`, result will be filtered for only the earliest
#'   date per eid-phecode pair (date will be recorded as `NA` for cases where
#'   there are no dates).
#'
#' @return A data frame with column names 'eid', 'source', 'index', 'code',
#'   'icd10', 'phecode' and 'date'.
#' @export
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # dummy clinical events data frame
#' dummy_clinical_events_tidy()
#'
#' # map to phecodes
#' map_clinical_events_to_phecodes(
#'   clinical_events = dummy_clinical_events_tidy(),
#'   all_lkps_maps = all_lkps_maps_dummy,
#'   min_date_only = FALSE
#' )
map_clinical_events_to_phecodes <- function(clinical_events,
                                            all_lkps_maps = NULL,
                                            min_date_only = FALSE,
                                            col_filters = default_col_filters()) {
  start_time <- proc.time()

  # connect to database file path if `all_lkps_maps` is a string, or `NULL`
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  } else if (is.null(all_lkps_maps)) {
    if (Sys.getenv("ALL_LKPS_MAPS_DB") != "") {
      message(paste0("Attempting to connect to ", Sys.getenv("ALL_LKPS_MAPS_DB")))
      con <-
        check_all_lkps_maps_path(Sys.getenv("ALL_LKPS_MAPS_DB"))
      all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
      on.exit(DBI::dbDisconnect(con))
    } else if (file.exists("all_lkps_maps.db")) {
      message("Attempting to connect to all_lkps_maps.db in current working directory")
      con <- check_all_lkps_maps_path("all_lkps_maps.db")
      all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
      on.exit(DBI::dbDisconnect(con))
    } else {
      stop(
        "No/invalid path supplied to `all_lkps_maps` and no file called 'all_lkps_maps.db' found in current working directory. See `?all_lkps_maps_to_db()`"
      )
    }
  }

  # ascertain available code types in `clinical_events`
  available_clinical_events_sources <- clinical_events %>%
    dplyr::select(.data[["source"]]) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  # full list of clinical events sources that may potentially be mapped to
  # phecodes
  clinical_events_sources_to_map <-
    ukbwranglr::clinical_events_sources() %>%
    dplyr::filter(
      .data[["source"]] %in% !!CLINICAL_EVENTS_SOURCES_MAPPED_TO_PHECODES
    ) %>%
    dplyr::arrange(.data[["category"]])

  # list of clinical events sources to actually be mapped (intersect of above)
  clinical_events_sources_to_map <- clinical_events_sources_to_map %>%
    dplyr::filter(.data[["source"]] %in% !!available_clinical_events_sources$source)

  # combine category and description (for informative messages later)
  clinical_events_sources_to_map$rowid <- 1:nrow(clinical_events_sources_to_map)

  clinical_events_sources_to_map <- clinical_events_sources_to_map %>%
    dplyr::mutate(
      "category_description" = paste0("[", .data[["rowid"]], "] **", .data[["category"]], "** - ", .data[["description"]])
    ) %>%
    dplyr::select(tidyselect::all_of(c(
      "source",
      "data_coding",
      "category_description"
    )))

  # loop through - map icd10 directly to phecode; for read and icd9, map to
  # phecodes via icd10
  message(
    paste0(
      "Identified the following ",
      length(clinical_events_sources_to_map$category_description),
      " data sources to map to phecodes: ",
      stringr::str_c(clinical_events_sources_to_map$category_description,
        sep = "",
        collapse = ", "
      )
    )
  )

  message("\n***MAPPING clinical_events TO PHECODES***\n")

  map_clinical_events_source_to_phecode_partial <- purrr::partial(map_clinical_events_source_to_phecode,
    all_lkps_maps = all_lkps_maps,
    clinical_events = clinical_events,
    col_filters = col_filters
  )

  result <- clinical_events_sources_to_map %>%
    purrr::pmap(map_clinical_events_source_to_phecode_partial)

  # combine, remove duplicate rows and reorder columns
  result <- result %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::select(
      tidyselect::all_of(c(
        ukbwranglr:::CLINICAL_EVENTS_COLHEADERS
      )),
      tidyselect::everything()
    )

  if (min_date_only) {
    result <- result %>%
      # take only earliest date (or `NA` if no dates recorded) per eid-phecode
      dplyr::group_by(
        .data[["eid"]],
        .data[["phecode"]]
      ) %>%
      dplyr::arrange(.data[["date"]]) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
  }

  ukbwranglr:::time_taken_message(start_time)
  return(result)
}

#' Reverse map from phecodes to Read and ICD 9
#'
#' Requires the output from [map_clinical_events_to_phecodes()], which maps UK
#' Biobank clinical events from Read 2, Read 3 and ICD-9 to ICD-10, then uses
#' [Phecode Map 1.2 with ICD-10 Codes
#' (beta)](https://phewascatalog.org/phecodes_icd10) to map these ICD-10
#' equivalents (and any actual ICD-10 records) to Phecodes. This function uses
#' the output from [map_clinical_events_to_phecodes()] to *reverse* map from
#' phecodes to Read 2, Read 3, ICD-9 and ICD-10. This is useful for checking
#' which raw clinical codes have been used in any phecode-defined phenotypes.
#'
#' @param clinical_events_phecodes A data frame created by
#'   [map_clinical_events_to_phecodes()].
#' @inheritParams  lookup_codes
#'
#' @return A data frame with columns "phecode", "phecode_description",
#'   "data_coding", "code" "description", "icd10_equivalent" and
#'   "icd10_description". Column 'code' contains the original 'raw' UK clinical
#'   codes, with code type indicated by the 'data_coding' column.
#' @export
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # map dummy clinical events df to phecodes
#' clinical_events_phecodes_dummy <- map_clinical_events_to_phecodes(
#'   clinical_events = dummy_clinical_events_tidy(),
#'   all_lkps_maps = all_lkps_maps_dummy,
#'   min_date_only = FALSE
#' )
#'
#' # create reverse map
#' make_phecode_reverse_map(
#'   clinical_events_phecodes = clinical_events_phecodes_dummy,
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
make_phecode_reverse_map <- function(clinical_events_phecodes,
                                     all_lkps_maps) {
  start_time <- proc.time()

  # connect to database file path if `all_lkps_maps` is a string, or `NULL`
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  } else if (is.null(all_lkps_maps)) {
    if (Sys.getenv("ALL_LKPS_MAPS_DB") != "") {
      message(paste0("Attempting to connect to ", Sys.getenv("ALL_LKPS_MAPS_DB")))
      con <-
        check_all_lkps_maps_path(Sys.getenv("ALL_LKPS_MAPS_DB"))
      all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
      on.exit(DBI::dbDisconnect(con))
    } else if (file.exists("all_lkps_maps.db")) {
      message("Attempting to connect to all_lkps_maps.db in current working directory")
      con <- check_all_lkps_maps_path("all_lkps_maps.db")
      all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
      on.exit(DBI::dbDisconnect(con))
    } else {
      stop(
        "No/invalid path supplied to `all_lkps_maps` and no file called 'all_lkps_maps.db' found in current working directory. See `?all_lkps_maps_to_db()`"
      )
    }
  }

  # append code types
  clinical_events_phecodes <- ukbwranglr::clinical_events_sources() %>%
    dplyr::select(tidyselect::all_of(c(
      "source",
      "data_coding"
    ))) %>%
    dplyr::right_join(clinical_events_phecodes,
      by = "source"
    )

  # make `code` = `icd10_code` (currently `code` is `NA` if source uses icd10
  # coding)
  clinical_events_phecodes <- clinical_events_phecodes %>%
    dplyr::mutate("code" = dplyr::case_when(
      .data[["data_coding"]] == "icd10" ~ .data[["icd10"]],
      TRUE ~ .data[["code"]]
    ))

  # select required cols only
  clinical_events_phecodes <- clinical_events_phecodes %>%
    dplyr::select(tidyselect::all_of(c(
      "code",
      "data_coding",
      "icd10",
      "phecode"
    )))

  # check there are no columns with missing values (there should be none)
  na_summary <- clinical_events_phecodes %>%
    purrr::map_dbl(~ sum(is.na(.x)))

  cols_with_missing_values <- subset(
    na_summary,
    na_summary > 0
  )

  assertthat::assert_that(
    length(cols_with_missing_values) == 0,
    msg = paste0(
      "The following columns contain `NA` values (there should be none): ",
      stringr::str_c(names(cols_with_missing_values),
        sep = "",
        collapse = ", "
      )
    )
  )

  # get distinct phecode/code/combinations only
  clinical_events_phecodes <- clinical_events_phecodes %>%
    dplyr::distinct()

  # need to re-append 'X' to undivided 3 character ICD10 codes in 'icd10' col
  # (e.g. 'I10X' hypertension)
  icd10_code_alt_code_x_map <-
    get_icd10_code_alt_code_x_map(
      icd10_lkp = all_lkps_maps$icd10_lkp,
      undivided_3char_only = FALSE,
      as_named_list = "names_no_x"
    )

  clinical_events_phecodes <-
    clinical_events_phecodes %>%
    dplyr::mutate("icd10" = dplyr::recode(.data[["icd10"]], !!!icd10_code_alt_code_x_map))

  # append code descriptions
  clinical_events_phecodes_split <- split(
    clinical_events_phecodes,
    clinical_events_phecodes$data_coding
  )

  # need to re-append 'X' to undivided 3 character ICD10 codes in 'code' col
  # (e.g. 'I10X' hypertension)
  clinical_events_phecodes_split$icd10 <-
    clinical_events_phecodes_split$icd10 %>%
    dplyr::mutate("code" = dplyr::recode(
      .data[["code"]],
      !!!icd10_code_alt_code_x_map
    ))

  code_descriptions <- clinical_events_phecodes_split %>%
    purrr::imap(
      ~ lookup_codes(
        codes = .x$code,
        code_type = .y,
        all_lkps_maps = all_lkps_maps,
        preferred_description_only = TRUE,
        standardise_output = TRUE,
        unrecognised_codes = "warning",
        col_filters = default_col_filters()
      ) %>%
        dplyr::select(tidyselect::all_of(c(
          "code",
          "description"
        )))
    )

  clinical_events_phecodes_split <- clinical_events_phecodes_split %>%
    # append code descriptions
    purrr::imap(~ .x %>%
      dplyr::left_join(code_descriptions[[.y]],
        by = "code"
      ))

  # strip 'X' from icd10 codes in 'code' col and recombine
  clinical_events_phecodes_split$icd10 <- clinical_events_phecodes_split$icd10 %>%
    dplyr::mutate("code" = stringr::str_remove(
      .data[["code"]],
      "X$"
    ))

  clinical_events_phecodes <- clinical_events_phecodes_split %>%
    dplyr::bind_rows()

  # append phecode_description
  phecode_descriptions <- lookup_codes(
    codes = clinical_events_phecodes$phecode,
    code_type = "phecode",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    unrecognised_codes = "error",
    col_filters = default_col_filters()
  ) %>%
    dplyr::select(
      "phecode" = .data[["code"]],
      "phecode_description" = .data[["description"]]
    )

  clinical_events_phecodes <- clinical_events_phecodes %>%
    dplyr::left_join(phecode_descriptions,
      by = c("phecode")
    )

  # append icd10 equivalent descriptions
  icd10_descriptions <- lookup_codes(
    codes = clinical_events_phecodes$icd10,
    code_type = "icd10",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    unrecognised_codes = "warning",
    col_filters = default_col_filters()
  ) %>%
    dplyr::select(
      "icd10" = .data[["code"]],
      "icd10_description" = .data[["description"]]
    )

  clinical_events_phecodes <- clinical_events_phecodes %>%
    dplyr::left_join(icd10_descriptions,
      by = c("icd10")
    )

  # strip 'X' from icd10 codes in 'icd10' column
  clinical_events_phecodes <- clinical_events_phecodes %>%
    dplyr::mutate("icd10" = stringr::str_remove(
      .data[["icd10"]],
      "X$"
    ))

  # reorder cols
  clinical_events_phecodes <- clinical_events_phecodes %>%
    dplyr::select(tidyselect::all_of(
      c(
        "phecode",
        "phecode_description",
        "data_coding",
        "code",
        "description"
      )
    ),
    "icd10_equivalent" = .data[["icd10"]],
    .data[["icd10_description"]]
    )

  # TODO - codes_like() function, use this to make get_undivided_3char_icd10()
  # function (use this to re-append 'X' and avoid warnings for these codes
  # above)

  ukbwranglr:::time_taken_message(start_time)
  return(clinical_events_phecodes)
}

# PRIVATE -----------------------------------------------------------------

#' Helper function for `map_clinical_events_to_phecode()`
#'
#' Maps a single clinical events source to phecodes. If mapping from an ICD10
#' source, maps directly to phecodes. If mapping from a non-ICD10 source, maps
#' to phecodes via ICD10.
#'
#' @param source Character.
#' @param data_coding Character.
#' @param category_description Character (for informative message).
#' @param all_lkps_maps List of lookup/mappings tables (or path to database
#'   version of this).
#' @param clinical_events UKB clinical events table.
#' @param col_filters See [default_col_filters()]
#' @noRd
#'
#' @return A data frame.
map_clinical_events_source_to_phecode <- function(source,
                                                  data_coding,
                                                  category_description,
                                                  all_lkps_maps,
                                                  clinical_events,
                                                  col_filters = default_col_filters()) {
  # informative message
  message(category_description)


  # map icd10 to phecode
  if (data_coding == "icd10") {
    clinical_events_source <-
      get_clinical_events_source(
        clinical_events = clinical_events,
        sources = source
      )

    clinical_events_source <- clinical_events_source %>%
      dplyr::rename("icd10" = .data[["code"]])

    # need to re-append 'X' to undivided 3 character ICD10 codes (e.g. 'I10X'
    # hypertension)
    icd10_code_alt_code_x_map <-
      get_icd10_code_alt_code_x_map(
        icd10_lkp = all_lkps_maps$icd10_lkp,
        undivided_3char_only = TRUE,
        as_named_list = "names_no_x"
      )

    clinical_events_source <- clinical_events_source %>%
      dplyr::mutate("icd10" = dplyr::recode(
        .data[["icd10"]],
        !!!icd10_code_alt_code_x_map
      ))

    # map
    result <-
      map_icd10_to_phecode(
        clinical_events = clinical_events_source,
        all_lkps_maps = all_lkps_maps,
        col_filters = col_filters
      )
  } else {
    # map non-ICD10 to phecode (via ICD10)
    clinical_events_source <-
      get_clinical_events_source(
        clinical_events = clinical_events,
        sources = source
      )

    result <- map_codes_ukb_clinical_events(
      clinical_events = clinical_events_source,
      from = data_coding,
      to = "icd10",
      all_lkps_maps = all_lkps_maps,
      col_filters = col_filters
    ) %>%
      map_icd10_to_phecode(
        all_lkps_maps = all_lkps_maps,
        col_filters = col_filters
      )
  }

  # remove 'X' from 'icd10' column
  result$icd10 <- stringr::str_remove(result$icd10,
    pattern = "X$"
  )

  # result - a data frame
  return(result)
}


#' Maps ICD10 codes in `clinical_events` to phecodes
#'
#' @param clinical_events Clinical events data frame
#' @param all_lkps_maps Named list of lookup and mapping tables (can be
#'   `tbl_dbi` objects) or the path to an SQLite database containing these
#' @param col_filters See [default_col_filters()]
#'
#' @return A clinical events data frame.
#' @noRd
map_icd10_to_phecode <- function(clinical_events,
                                 all_lkps_maps = "all_lkps_maps.db",
                                 col_filters = default_col_filters()) {
  map_codes_ukb_clinical_events(
    clinical_events = clinical_events,
    from = "icd10",
    to = "phecode",
    from_colname = "icd10",
    to_colname = "phecode",
    all_lkps_maps = all_lkps_maps,
    col_filters = col_filters
  )
}


#' Map codes in a UKB clinical events table
#'
#' Joins the existing `code` column to a mapping data frame created by
#' [get_mapping_df()], then filters out any missing values for the new code
#' type. A column is appended containing the newly mapped codes.
#'
#' The `clinical_events` data frame can contain multiple sources, but an error
#' is raised by default if these do not all use the same data coding (see
#' [ukbwranglr::clinical_events_sources()] for recognised data sources and their
#' respective data codings).
#'
#' @param clinical_events A clinical events data frame created by
#'   [ukbwranglr::tidy_clinical_events()]
#' @param from Coding type being mapped from
#' @param to Coding type being mapped to
#' @param from_colname Name of column containing codes to be mapped from. If
#'   `NULL` (default), this is assumed to be named 'code'.
#' @param to_colname Name of new column containing mapped codes. If `NULL`
#'   (default), this will equal the value for argument `to`.
#' @param all_lkps_maps Named list of SQLite database with lookup/mapping tables
#' @param col_filters See [default_col_filters()]
#' @noRd
#'
#' @return A clinical events data frame.
map_codes_ukb_clinical_events <- function(clinical_events,
                                          from,
                                          to,
                                          from_colname = NULL,
                                          to_colname = NULL,
                                          all_lkps_maps = "all_lkps_maps.db",
                                          col_filters = default_col_filters()) {
  # validate args
  check_mapping_args(
    from = from,
    to = to
  )

  if (!is.null(to_colname)) {
    assertthat::is.string(to_colname)
  }

  if (!is.null(from_colname)) {
    assertthat::is.string(from_colname)
  }

  # all_lkps_maps
  ## connect to database file path
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con))
  }

  # create mapping df
  mapping_df <- get_mapping_df(
    from = from,
    to = to,
    all_lkps_maps = all_lkps_maps,
    rename_from_to = NULL,
    na.rm = TRUE,
    reverse_mapping = "error",
    col_filters = col_filters
  )

  # map
  if (is.null(from_colname)) {
    from_colname <- "code"
  }

  join_by <- from
  names(join_by) <- from_colname

  result <- clinical_events %>%
    dplyr::inner_join(mapping_df,
      by = join_by
    ) %>%
    # remove duplicated rows
    dplyr::distinct()

  # rename newly appended column (optionally)
  if (!is.null(to_colname)) {
    result <- ukbwranglr:::rename_cols(
      df = result,
      old_colnames = to,
      new_colnames = to_colname
    )
  }

  # return result
  return(result)
}

#' Filter UK Biobank clinical events for selected data sources
#'
#' @param clinical_events A clinical events table (data frame or `tbl_dbi`
#'   object) created by [ukbwranglr::tidy_clinical_events()].
#' @param sources A character vector of data sources. Must be listed under
#'   `source` in [ukbwranglr::clinical_events_sources()].
#' @param allow_missing_sources If `FALSE` (default), an error is raised if any
#'   values for `sources` are not present in `clinical_events`. If `TRUE`, a
#'   warning is raised instead.
#'
#' @return A data frame
#' @noRd
get_clinical_events_source <- function(clinical_events,
                                       sources,
                                       allow_missing_sources = FALSE) {
  # validate args
  assertthat::assert_that(all(sources %in% ukbwranglr::clinical_events_sources()$source))
  ukbwranglr:::validate_clinical_events_and_check_type(clinical_events)

  # check selected sources are present
  check_sources <- clinical_events %>%
    dplyr::filter(.data[["source"]] %in% !!sources) %>%
    dplyr::distinct(.data[["source"]],
      .keep_all = FALSE
    ) %>%
    dplyr::collect()

  # error/warning if any sources are not present
  missing_sources <-
    subset(sources, !sources %in% check_sources$source)

  if (length(missing_sources) != 0) {
    missing_sources_msg <-
      "The following sources are not present in `clinical events`: "
    if (allow_missing_sources) {
      warning(paste0(
        "Warning! ",
        missing_sources_msg,
        stringr::str_c(missing_sources,
          sep = "",
          collapse = ", "
        )
      ))
    } else {
      stop(paste0(
        "Error!",
        missing_sources_msg,
        stringr::str_c(missing_sources,
          sep = "",
          collapse = ", "
        )
      ))
    }
  }

  # filter clinical events table for selected sources
  result <- clinical_events %>%
    dplyr::filter(.data[["source"]] %in% !!sources) %>%
    dplyr::collect()

  # return result
  return(result)
}
