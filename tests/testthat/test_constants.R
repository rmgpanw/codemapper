
# `CLINICAL_CODE_MAPPINGS_MAP` --------------------------------------------

# TODO - get this test working. Oly a subset of code types (`data_coding`) will
# be shared between ukbwranglr and codemapper if codemapper includes coding
# systems that are not in UKB (e.g. SNOMED, PheCODEs)

# test_that("`CLINICAL_CODE_MAPPINGS_MAP` has no spelling mistakes", {
#   expect_true(all(
#     unique(ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding) %in% unique(
#       c(
#         CLINICAL_CODE_MAPPINGS_MAP$from,
#         CLINICAL_CODE_MAPPINGS_MAP$to
#       )
#     )
#   ))
#
# })

test_that("`CLINICAL_CODE_MAPPINGS_MAP` has only unique to_from mapping combinations", {
  expect_true(
    length(
      paste(CLINICAL_CODE_MAPPINGS_MAP$from, CLINICAL_CODE_MAPPINGS_MAP$to, sep = "_")
    ) == nrow(CLINICAL_CODE_MAPPINGS_MAP)
  )
})

test_that("`CLINICAL_CODE_MAPPINGS_MAP` has only unique values in 'mapping_table' column", {
  expect_true(
    length(unique(CLINICAL_CODE_MAPPINGS_MAP$mapping_table)) == nrow(CLINICAL_CODE_MAPPINGS_MAP)
  )
})

# `CODE_TYPE_TO_LKP_TABLE_MAP` --------------------------------------------

test_that("`CODE_TYPE_TO_LKP_TABLE_MAP` has only unique values", {
  expect_true(length(CODE_TYPE_TO_LKP_TABLE_MAP$code) == length(unique(CODE_TYPE_TO_LKP_TABLE_MAP$code)))

  expect_true(length(CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table) == length(unique(CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table)))
})

# TODO - get this test working. Oly a subset of code types (`data_coding`) will
# be shared between ukbwranglr and codemapper if codemapper includes coding
# systems that are not in UKB (e.g. SNOMED, PheCODEs)

# test_that("`CODE_TYPE_TO_LKP_TABLE_MAP` only contains code types in `ukbwranglr:::CLINICAL_EVENTS_SOURCES`", {
#   expect_true(
#     all(CODE_TYPE_TO_LKP_TABLE_MAP$code %in% ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding)
#   )
# })
