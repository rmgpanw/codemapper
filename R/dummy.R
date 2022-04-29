
# PUBLIC ------------------------------------------------------------------

#' Dummy UK Biobank codings file path
#'
#' Returns the file path to a dummy [UK Biobank
#' codings](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)
#' tsv file.
#'
#' @return A string.
#' @export
#' @family Dummy data
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
read_dummy_all_lkps_maps <- function() {
  read_all_lkps_maps(path = dummy_all_lkps_maps_path())
}
