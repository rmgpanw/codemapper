
# `CLINICAL_CODE_MAPPINGS_MAP` --------------------------------------------

test_that("`CLINICAL_CODE_MAPPINGS_MAP` has no spelling mistakes", {
  expect_true(
    all(unique(c(
      CLINICAL_CODE_MAPPINGS_MAP$from,
      CLINICAL_CODE_MAPPINGS_MAP$to)) %in% unique(ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding))
  )

})

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

# TO DELETE?

# test_that(
#   "`CLINICAL_CODE_MAPPINGS_MAP`: for each 'mapping_table', the 'from_col' and 'to_col' values are actually column names in that 'mapping table'",
#   {
#     # check colnames for each mapping sheet
#     lambda <- function() {
#     result <- NULL
#     for (mapping_table in CLINICAL_CODE_MAPPINGS_MAP$mapping_table) {
#       if (all(
#         c(
#           get_value_for_mapping_sheet(mapping_table = mapping_table, value = "from_col"),
#           get_value_for_mapping_sheet(mapping_table = mapping_table, value = "to_col")
#         ) %in% colnames_for_ukb_code_mappings_sheet_names[[mapping_table]]
#       )) {
#         result <- TRUE
#       } else {
#         result <- FALSE
#         break()
#       }
#     }
#     return(result)
#     }
#
#     expect_true(object = lambda())
#   }
# )

# `CODE_TYPE_TO_LKP_TABLE_MAP` --------------------------------------------

test_that("`CODE_TYPE_TO_LKP_TABLE_MAP` has only unique values", {
  expect_true(length(CODE_TYPE_TO_LKP_TABLE_MAP$code) == length(unique(CODE_TYPE_TO_LKP_TABLE_MAP$code)))

  expect_true(length(CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table) == length(unique(CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table)))
})

test_that("`CODE_TYPE_TO_LKP_TABLE_MAP` only contains code types in `ukbwranglr:::CLINICAL_EVENTS_SOURCES`", {
  expect_true(
    all(CODE_TYPE_TO_LKP_TABLE_MAP$code %in% ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding)
  )
})
