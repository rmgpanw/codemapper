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
    sheet_names <- subset(sheet_names,
                          !(sheet_names %in% to_exclude))
  }

  # read sheets into named list
  sheet_names %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel,
               path = path,
               col_types = col_types,
               ...)
}


read_all_lkps_maps <- function(path) {
  read_excel_to_named_list(path = path,
                           to_include = NULL,
                           to_exclude = c("Description", "Contents"),
                           col_types = "text")
}

remove_irrelevant_rows_all_lkps_maps <- function(all_lkps_maps_raw) {
  nrows_to_remove <- tibble::tribble(
    ~ sheet, ~ n_to_remove,
    'bnf_lkp', 2,
  'dmd_lkp', 2,
  'icd9_lkp', 2,
  'icd10_lkp', 2,
  'icd9_icd10', 3,
  'read_v2_lkp', 2,
  'read_v2_drugs_lkp', 2,
  'read_v2_drugs_bnf', 3,
  'read_v2_icd9', 3,
  'read_v2_icd10', 3,
  'read_v2_opcs4', 3,
  'read_v2_read_ctv3', 2,
  'read_ctv3_lkp', 2,
  'read_ctv3_icd9', 3,
  'read_ctv3_icd10', 3,
  'read_ctv3_opcs4', 3,
  'read_ctv3_read_v2', 2
  )

  result <- purrr::pmap(nrows_to_remove,
                               function(sheet, n_to_remove) {
                                 all_lkps_maps_raw[[sheet]][1:(nrow(all_lkps_maps_raw[[sheet]]) - n_to_remove), ]
                               })

  names(result) <- names(all_lkps_maps_raw)

  return(result)
}

read_nhsbsa_snomed_bnf <- function() {
  # download file to tempdir, if not already there
  bnf_dmd_map_zip_path <- file.path(tempdir(), "bnf_dmd.zip")

  if (!file.exists(bnf_dmd_map_zip_path)) {
    utils::download.file(url = "https://www.nhsbsa.nhs.uk/sites/default/files/2021-08/BNF%20Snomed%20Mapping%20data%2020210819.zip",
                         destfile = bnf_dmd_map_zip_path,
                         mode = 'wb')
  }

  # read file and tidy names
  bnf_dmd_map_path <- utils::unzip(bnf_dmd_map_zip_path)
  bnf_dmd <- readxl::read_excel(bnf_dmd_map_path)
  names(bnf_dmd) <- ukbwranglr:::remove_special_characters_and_make_lower_case(names(bnf_dmd)) %>%
    stringr::str_remove_all("plus_")

  return(bnf_dmd)
}

# TODO --------------------------------------------------------------------

validate_all_lkps_maps <- function(all_lkps_maps) {
  TRUE
}

get_all_lkps_maps <- function() {
  # download
  targets::tar_read(all_lkps_maps)
}

get_all_lkps_maps_db <- function() {
  # download
}
