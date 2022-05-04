
# PUBLIC ------------------------------------------------------------------

## all_lkps_maps ------------

#' Dummy UK Biobank codings file path
#'
#' Returns the file path to a dummy [UK Biobank
#' codings](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)
#' tsv file.
#'
#' @return A string.
#' @export
#' @family Dummy data
#' @examples
#' dummy_ukb_codings_path()
dummy_ukb_codings_path <- function() {
  system.file("extdata", "dummy_Codings.tsv", package = "codemapper")
}

#' Dummy UK Biobank resource 592 file path
#'
#' Returns the file path to a dummy [UK Biobank resource
#' 592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) excel spreadsheet.
#'
#' @return A string.
#' @export
#' @family Dummy data
#' @examples
#' dummy_all_lkps_maps_path()
dummy_all_lkps_maps_path <- function() {
  system.file("extdata", "dummy_all_lkps_maps_v3.xlsx", package = "codemapper")
}

#' Read dummy UK Biobank codings into R
#'
#' Reads a dummy [UK Biobank
#' codings](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)
#' tsv file into R.
#'
#' @return A data frame.
#' @export
#' @family Dummy data
#' @examples
#' read_dummy_ukb_codings
read_dummy_ukb_codings <- function() {
  ukb_codings <- ukbwranglr:::fread_tsv_as_character(dummy_ukb_codings_path())
}

#' Read dummy UK Biobank resource 592 into R
#'
#' Reads a dummy [UK Biobank resource
#' 592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) excel spreadsheet
#' into R.
#'
#' @return A named list of tibbles.
#' @export
#' @family Dummy data
#' @examples
#' read_dummy_all_lkps_maps()
read_dummy_all_lkps_maps <- function() {
  read_all_lkps_maps(path = dummy_all_lkps_maps_path())
}

#' Create a dummy all_lkps_maps
#'
#' A thin convenience wrapper around [build_all_lkps_maps()], using dummy data
#' included with this package.
#'
#' @return A named list of tibbles.
#' @export
#'
#' @family Dummy data
#' @seealso [build_all_lkps_maps()]
#' @examples
#' build_dummy_all_lkps_maps()
build_dummy_all_lkps_maps <- function() {
    build_all_lkps_maps(
      all_lkps_maps = read_dummy_all_lkps_maps(),
      ukb_codings = read_dummy_ukb_codings(),
      bnf_dmd = NULL,
      self_report_med_to_atc_map = NULL,
      ctv3sctmap2 = NULL,
      phecode_1_2_lkp = NULL,
      icd10_phecode_1_2 = NULL,
      icd9_phecode_1_2 = NULL
    )
}

## CALIBER -----------------------------------------------------------------

#' Dummy CALIBER repository
#'
#' Returns the file path to a dummy data
#' [CALIBER](https://github.com/spiros/chronological-map-phenotypes) repository.
#'
#' @return A string.
#' @export
#' @family Dummy data
dummy_caliber_dir_path <- function() {
  system.file("extdata", "test_caliber_repo", package = "codemapper")
}
