% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clinical_events_to_phecodes.R
\name{make_phecode_reverse_map}
\alias{make_phecode_reverse_map}
\title{Reverse map from phecodes to Read and ICD 9}
\usage{
make_phecode_reverse_map(clinical_events_phecodes, all_lkps_maps)
}
\arguments{
\item{clinical_events_phecodes}{A data frame created by
\code{\link[=map_clinical_events_to_phecodes]{map_clinical_events_to_phecodes()}}.}

\item{all_lkps_maps}{Either a named list of lookup and mapping tables (either
data frames or \code{tbl_dbi} objects), or the path to a SQLite database
containing these tables (see also \code{\link[=build_all_lkps_maps]{build_all_lkps_maps()}} and
\code{\link[=all_lkps_maps_to_db]{all_lkps_maps_to_db()}}). If \code{NULL}, will attempt to connect to an SQLite
database named 'all_lkps_maps.db' in the current working directory, or to a
a SQLite database specified by an environmental variable named
'ALL_LKPS_MAPS_DB' (see
\href{https://resources.numbat.space/using-rprofile-and-renviron.html#renviron}{here}
for how to set environment variables using a \code{.Renviron} file). The latter
method will be used in preference.}
}
\value{
A data frame with columns "phecode", "phecode_description",
"data_coding", "code" "description", "icd10_equivalent" and
"icd10_description". Column 'code' contains the original 'raw' UK clinical
codes, with code type indicated by the 'data_coding' column.
}
\description{
Requires the output from \code{\link[=map_clinical_events_to_phecodes]{map_clinical_events_to_phecodes()}}, which maps UK
Biobank clinical events from Read 2, Read 3 and ICD-9 to ICD-10, then uses
\href{https://phewascatalog.org/phecodes_icd10}{Phecode Map 1.2 with ICD-10 Codes (beta)} to map these ICD-10
equivalents (and any actual ICD-10 records) to Phecodes. This function uses
the output from \code{\link[=map_clinical_events_to_phecodes]{map_clinical_events_to_phecodes()}} to \emph{reverse} map from
phecodes to Read 2, Read 3, ICD-9 and ICD-10. This is useful for checking
which raw clinical codes have been used in any phecode-defined phenotypes.
}
\examples{
# build dummy all_lkps_maps
all_lkps_maps_dummy <- build_all_lkps_maps_dummy()

# map dummy clinical events df to phecodes
clinical_events_phecodes_dummy <- map_clinical_events_to_phecodes(
  clinical_events = dummy_clinical_events_tidy(),
  all_lkps_maps = all_lkps_maps_dummy,
  min_date_only = FALSE
)

# create reverse map
make_phecode_reverse_map(
  clinical_events_phecodes = clinical_events_phecodes_dummy,
  all_lkps_maps = all_lkps_maps_dummy
)
}
