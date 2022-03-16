
# CONSTANTS ---------------------------------------------------------------

all_lkps_maps <- get_ukb_code_mappings() %>%
  purrr::map(rm_footer_rows_all_lkps_maps_df) %>%
  purrr::map(~ tibble::rowid_to_column(.data = .x,
                                       var = ".rowid"))

# TESTS -------------------------------------------------------------------


# `all_lkps_maps` -----------------------------------------------------

test_that("`all_lkps_maps` table 'icd10_lkp' has no rows with values in both the 'MODIFER-4' and 'MODIFER-5' columns", {
  # relevant to `lookup_codes()` when `standardise_output` is `TRUE`. Some
  # ICD-10 codes have a description modifier in one of these 2 columns (e.g.
  # `E10` for T1DM (MODIFER-4) and `S27` for traumatic pneumothorax
  # (MODIFER-5)). `lookup_codes()` creates a description column by pasting
  # together the 'DESCRIPTION' column with *only* one of these. Therefore only
  # one of these columns should contain a description.
  expect_true(
    sum(!is.na(all_lkps_maps$icd10_lkp$MODIFIER_4) & !is.na(all_lkps_maps$icd10_lkp$MODIFIER_5)) == 0
  )
})

# `codes_starting_with()` -----------------------------------------------------

test_that("`codes_starting_with()` returns the expected nuber of results, escaping '.'", {
  # return - codes only

  # escaping '.'
  expect_equal(
    codes_starting_with(
      codes = c("C10E."),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE
    )
  ,
  expected = "C10E.")

  # no '.'
  expect_equal(
    length(codes_starting_with(
      codes = c("C10E"),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = TRUE,
      standardise_output = FALSE
    )),
    expected = 27)

  # return codes and descriptions as a data frame
  expect_equal(nrow(
    codes_starting_with(
      codes = c("C10E"),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = FALSE,
      preferred_description_only = FALSE
    )
  ),
  expected = 73)

  expect_equal(nrow(
    codes_starting_with(
      codes = c("C10E"),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      codes_only = FALSE,
      preferred_description_only = TRUE
    )
  ),
  expected = 27)
})

# `lookup_codes()` --------------------------------------------------------

test_that("`lookup_codes()` returns the expected number of results", {
  expect_equal(nrow(
    lookup_codes(
      codes = c("C10E.", "C108."),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      preferred_description_only = FALSE
    )
  ),
  expected = 7)

  expect_equal(nrow(
    lookup_codes(
      codes = c("C10E.", "C108."),
      code_type = "read2",
      all_lkps_maps = all_lkps_maps,
      preferred_description_only = TRUE
    )
  ),
  expected = 2)
})

test_that(
  "`lookup_codes()` returns the expected columns when `standardise_output` is `TRUE`",
  {
    result <- lookup_codes(
      codes = c("E10", "E100"),
      code_type = "icd10",
      all_lkps_maps = all_lkps_maps,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    )

    expect_equal(names(result), c("code", "description", "code_type"))

    expect_equal(result$description,
                 c("Type 1 diabetes mellitus",
                   "Type 1 diabetes mellitus With coma"))
  }
  )

# `code_descriptions_like()` ----------------------------------------------

test_that("`code_descriptions_like()` returns expected results", {
  expect_equal(
    code_descriptions_like(
      reg_expr = "diabetic retinopathy",
      code_type = "icd10",
      all_lkps_maps = all_lkps_maps,
      ignore_case = TRUE,
      codes_only = TRUE
    ),
    "H360"
  )
})

# `map_codes()` -----------------------------------------------------------

test_that(
  "`map_codes()` raises warning if any of the supplied codes are not present in the coding system being mapped from", {
    expect_warning(
      map_codes(codes = c("C10E.", "foo", "bar"),
                from = "read2",
                to = "read3",
                all_lkps_maps = all_lkps_maps,
                unrecognised_codes = "warning"),
      regexp = "The following 2 codes were not found for read2: 'foo', 'bar'",
      fixed = TRUE
    )
  }
)

test_that(
  "`map_codes()` returns the expected codes", {
    # codes only
    expect_equal(
      map_codes(codes = c("C10E."),
                from = "read2",
                to = "read3",
                all_lkps_maps = all_lkps_maps,
                unrecognised_codes = "error",
                codes_only = TRUE,
                standardise_output = FALSE),
      "X40J4"
    )

    # codes and ALL descriptions
    expect_equal(
      nrow(map_codes(codes = c("C10E."),
                     from = "read2",
                     to = "read3",
                     all_lkps_maps = all_lkps_maps,
                     unrecognised_codes = "error",
                     codes_only = FALSE,
                     preferred_description_only = FALSE,
                     standardise_output = FALSE)),
      4
    )

    # codes and preferred descriptions only - should raise an error as can miss
    # codes e.g. try searching for "D4104", will only return the secondary
    # description for its Read3 equivalent (which is also "D4104")
    expect_error(
      map_codes(
        codes = c("C10E.", "C108."),
        from = "read2",
        to = "read3",
        all_lkps_maps = all_lkps_maps,
        unrecognised_codes = "error",
        codes_only = FALSE,
        preferred_description_only = TRUE,
        standardise_output = FALSE
      ),
      regexp = "Error! `preferred_description_only` cannot be `TRUE` unless `standardise_output` is also `TRUE`"
    )
  }
)

test_that(
  "`map_codes` returns the expected output when `standardise_output` is `TRUE`",
  {
    expect_equal(
      map_codes(
        codes = c("C10E.", "C108."),
        from = "read2",
        to = "read3",
        all_lkps_maps = all_lkps_maps,
        unrecognised_codes = "error",
        codes_only = FALSE,
        preferred_description_only = TRUE,
        standardise_output = TRUE
      )$code,
      "X40J4"
    )
  }
)

test_that("`map_codes()` works as expected for mapping icd10 to icd9 codes", {
  expect_equal(
    suppressWarnings(map_codes(
    codes = "D751",
    from = "icd10",
    to = "icd9",
    all_lkps_maps = all_lkps_maps,
    unrecognised_codes = "error",
    codes_only = FALSE,
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    reverse_mapping = "warning"
  )$code),
  "2890")
})

test_that("`map_codes()` works when mapping icd9 to icd10", {
  expect_equal(
    map_codes(
      codes = "0020",
      from = "icd9",
      to = "icd10",
      all_lkps_maps = all_lkps_maps,
      unrecognised_codes = "error",
      codes_only = FALSE,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    )$code,
    expected = "A010"
  )
})

# `get_mapping_df()` --------------------------
test_that("`get_mapping_df()` returns the expected output", {
  read2_icd10_df <- get_mapping_df(from = "read2",
                                   to = "icd10",
                                   all_lkps_maps = all_lkps_maps) %>%
    head(n = 1)

  read2_icd10_df_renamed <- get_mapping_df(
    from = "read2",
    to = "icd10",
    all_lkps_maps = all_lkps_maps,
    rename_from_to = c(from = "from", to = "to")
  ) %>%
    head(n = 1)

  read2_icd10_df_renamed2 <- get_mapping_df(
    from = "read2",
    to = "icd10",
    all_lkps_maps = all_lkps_maps,
    rename_from_to = c(to = "to", from = "from")
  ) %>%
    head(n = 1)

  icd10_read2_df <- suppressWarnings(get_mapping_df(
    from = "icd10",
    to = "read2",
    all_lkps_maps = all_lkps_maps,
    reverse_mapping = "warning"
  )) %>%
    head(n = 1)

  expect_equal(
    read2_icd10_df,
    tibble::tibble(read2 = "A00..",
                   icd10 = "A00")
  )

  expect_equal(
    read2_icd10_df_renamed,
    tibble::tibble(from = "A00..",
                   to = "A00")
  )

  # should be the same as above
  expect_equal(
    read2_icd10_df_renamed2,
    read2_icd10_df_renamed
  )

  expect_equal(
    icd10_read2_df,
    tibble::tibble(icd10 = "A00",
                   read2 = "A00..")
  )
})

# `reformat_standardised_codelist()` --------------------------------------

test_that("`reformat_standardised_codelist()` returns the expected output format",
          {
            expect_equal(
              lookup_codes(
                codes = c("C10E.", "C108."),
                code_type = "read2",
                all_lkps_maps = all_lkps_maps,
                preferred_description_only = TRUE
              ) %>%
                reformat_standardised_codelist(
                  code_type = "read2",
                  disease = "T1DM",
                  disease_category = "T1DM GP diagnosis",
                  author = "test"
                ) %>%
                names(),
              c(
                'disease',
                'description',
                'category',
                'code_type',
                'code',
                'author'
              )
            )
          })

test_that("`reformat_standardised_codelist()` raises error with invalid args", {
  expect_error(
    reformat_standardised_codelist(
      standardised_codelist = data.frame(
        code = "C10E.",
        description = "T1DM",
        code_type = "invalid_code"
      ),
      code_type = "read2",
      disease = "T1DM",
      disease_category = "T1DM GP diagnosis",
      author = "test"
    ),
    regexp = "contains unrecognised code types"
  )

  expect_error(
    reformat_standardised_codelist(
      standardised_codelist = data.frame(
        code = "C10E.",
        description = "T1DM",
        A_TYPE_OF_CODE = "read2"
      ),
      code_type = "read2",
      disease = "T1DM",
      disease_category = "T1DM GP diagnosis",
      author = "test"
    ),
    regexp = "must be a data frame with the following headings: 'code', 'description', 'code_type'"
  )
})

# `get_from_to_mapping_sheet()` -------------------------------------------

test_that(
  "`get_from_to_mapping_sheet()` returns the correct mapping table for various 'from'/'to' combinations", {
    expect_equal(get_from_to_mapping_sheet(from = "read2", "read3"),
                 "read_v2_read_ctv3")

    expect_equal(get_from_to_mapping_sheet(from = "read3", "read2"),
                 "read_ctv3_read_v2")

    expect_equal(get_from_to_mapping_sheet(from = "read2_drugs", "bnf"),
                 "read_v2_drugs_bnf")
  }
)

# `handle_unrecognised_codes()` ------------------------------------------

test_that("`handle_unrecognised_codes()` produces an error/warning message appropriately", {

  # should raise an error
  expect_error(
    handle_unrecognised_codes(unrecognised_codes = "error",
                              missing_codes = "foo",
                              code_type = "imaginary_coding_system"),
    regexp = "The following 1 codes were not found for imaginary_coding_system"
  )

  # should raise a warning
  expect_warning(
    handle_unrecognised_codes(unrecognised_codes = "warning",
                              missing_codes = "foo",
                               code_type = "imaginary_coding_system"),
    regexp = "The following 1 codes were not found for imaginary_coding_system"
  )

  # should return NULL
  expect_null(
    handle_unrecognised_codes(
      unrecognised_codes = "error",
      missing_codes = character(),
      code_type = "imaginary_coding_system"
    )
  )
})

# `reformat_icd10_codes()` ------------------------------------------------
test_that("`reformat_icd10_codes()` returns the expected values for ICD10_CODE to ALT_CODE", {
  expect_equal(
    # warning raised because "I714" not present in ICD10_CODE col of icd10_lkp
    # table
    suppressWarnings(reformat_icd10_codes(
        icd10_codes = c("D75.1",
                        "H40", # will be the same for ICD10_CODE and ALT_CODE
                        "H40.1",
                        "I714", # not in ICD10_CODE col
                        "M00.0"), # multiple associated ALT_CODEs
        all_lkps_maps = all_lkps_maps,
        input_icd10_format = "ICD10_CODE",
        output_icd10_format = "ALT_CODE",
        unrecognised_codes = "warning"
      )),
    c("D751", "H40", "H401", "M000", "M0000", "M0001", "M0002", "M0003", "M0004", "M0005", "M0006", "M0007", "M0008", "M0009")
  )
})

test_that("`reformat_icd10_codes()` returns the expected values for ALT_CODE to ICD10_CODE", {
  expect_equal(
    reformat_icd10_codes(
        icd10_codes = c("D751",
                        "H40", # will be the same for ICD10_CODE and ALT_CODE
                        "H401",
                        "I714", # not in ICD10_CODE col
                        "M000", # multiple associated ALT_CODEs - all map to "M00.0"
                        "M0001",
                        "M0002"),
        all_lkps_maps = all_lkps_maps,
        input_icd10_format = "ALT_CODE",
        output_icd10_format = "ICD10_CODE"
      ),
    c("D75.1", "H40", "H40.1", "I71.4", "M00.0")
  )
})

test_that(
  "`reformat_icd10_codes()` returns the expected values for ICD10_CODE to ALT_CODE for a 3 character code with no children",
  {
    expect_equal(
      reformat_icd10_codes(
        icd10_codes = c("A38"),
        all_lkps_maps = all_lkps_maps,
        input_icd10_format = "ICD10_CODE",
        output_icd10_format = "ALT_CODE"
      ),
      "A38X"
    )
  }
)

test_that(
  "`reformat_icd10_codes()` strips 'X' from undivided 3 character codes in `ALT_CODE` format (when `strip_x` is `TRUE`)",
  {
    expect_equal(
      reformat_icd10_codes(
        icd10_codes = c("A38"),
        all_lkps_maps = all_lkps_maps,
        input_icd10_format = "ICD10_CODE",
        output_icd10_format = "ALT_CODE",
        strip_x = TRUE
      ),
      "A38"
    )
  }
)

# `filter_cols` -----------------------------------------------------------

test_that("`filter_cols` filters columns as expected (or returns `df` unchanged, if appropriate)", {
  # change `Species` column to class 'character'
  iris_chr <- iris %>%
    dplyr::mutate(Species = as.character(Species))

  # check returns expected number of rows for single/multiple column/value combinations
  expect_equal(nrow(
    filter_cols(
      df = iris_chr,
      df_name = "iris",
      col_filters = list(iris = list(Species = c("setosa")))
    )
  ),
  50)

  expect_equal(nrow(
    filter_cols(
      df = iris_chr,
      df_name = "iris",
      col_filters = list(iris = list(Species = c("setosa", "virginica")))
    )
  ),
  100)

  expect_equal(nrow(filter_cols(
    df = iris_chr,
    df_name = "iris",
    col_filters = list(iris = list(
      Species = c("setosa", "virginica"),
      Petal.Width = c(0.5, 0.6)
    ))
  )),
  2)

  # returns df unchanged if `df_name` not in `names(col_filters)`
  expect_equal(nrow(filter_cols(
    df = iris_chr,
    df_name = "iris",
    col_filters = list(FOO = list(
      Species = c("setosa", "virginica"),
      Petal.Width = c(0.5, 0.6)
    ))
  )),
  150)
})

test_that("`filter_cols` raises error if `col_filters` includes unrecognised/missing column names", {
  # unrecognised column name
  expect_error(
    filter_cols(
      df = iris,
      df_name = "iris",
      col_filters = list(iris = list(Species2 = c("setosa"),
                                     Foo = c("setosa")))
    ),
    "are not present in"
  )

  # unnamed item in `col_filters`
  expect_error(
    filter_cols(
      df = iris,
      df_name = "iris",
      col_filters = list(iris = list(Species2 = c("setosa"),
                                     c("setosa")))
    ),
    "must be named"
  )
})

test_that("`filter_cols` raises error if `col_filters` contains items that are not vectors", {
  # unrecognised column name
  expect_error(
    filter_cols(
      df = iris,
      df_name = "iris",
      col_filters = list(iris = list(Species = iris))
    ),
    "Each item in `col_filters` must be a vector"
  )
})

test_that("`filter_cols` raises error if class of df column to be filtered does not match class of supplied filter values", {
  expect_error(
    filter_cols(
      df = iris,
      df_name = "iris",
      col_filters = list(iris = list(Species = c("setosa")))
    ),
    "classes do not match"
  )
})

# `rm_footer_rows_all_lkps_maps_df()` ----------------------------------------

test_that("`rm_footer_rows_all_lkps_maps_df()` removes footer rows as expected", {
  df <- data.frame(
    col1 = c("A", NA, "C", "D", NA, "Footer text"),
    col2 = c(letters[1:4], NA, NA)
  )

  expect_equal(
    rm_footer_rows_all_lkps_maps_df(df),
    data.frame(
      col1 = c("A", NA, "C", "D"),
      col2 = c(letters[1:4])
    )
  )
})


# `get_icd10_code_range()` ------------------------------------------------

test_that("`get_icd10_code_range()` returns expected codes", {

  # 4 character ICD10 code range
  expect_equal(
    get_icd10_code_range(
      start_icd10_code = "E100",
      end_icd10_code = "E109",
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    c(
      "E100",
      "E101",
      "E102",
      "E103",
      "E104",
      "E105",
      "E106",
      "E107",
      "E108",
      "E109"
    )
  )

  # 'D' appended - expect error
  expect_error(
    get_icd10_code_range(
      start_icd10_code = "A170D",
      end_icd10_code = "A179D",
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    regexp = "were not found for icd10"
  )

  # 3 character ICD10 code range
  expect_equal(
    get_icd10_code_range(
      start_icd10_code = "A80",
      end_icd10_code = "A81",
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    c(
      "A80",
      "A800",
      "A801",
      "A802",
      "A803",
      "A804",
      "A809",
      "A81",
      "A810",
      "A811",
      "A812",
      "A818",
      "A819"
    )
  )

  # 3 character ICD10 code range, including final 'X' character
  expect_equal(
    get_icd10_code_range(
      start_icd10_code = "A56",
      end_icd10_code = "A58X",
      icd10_lkp = all_lkps_maps$icd10_lkp
    ),
    c(
      "A56",
      "A560",
      "A561",
      "A562",
      "A563",
      "A564",
      "A568",
      "A57X",
      "A58X"
    )
  )
})

# `rm_or_extract_appended_icd10_dxa()` -----------------------------

test_that("`rm_or_extract_appended_icd10_dxa()` works", {
  icd10_codes <- c("A00",
                   "A408",
                   "A390D",
                   "A38X",
                   "G01XA")

  # remove
  rm_expected_result <- c("A00",
                          "A408",
                          "A390",
                          "A38X",
                          "G01X")

  rm_expected_result_x_rm <- c("A00",
                               "A408",
                               "A390",
                               "A38",
                               "G01")

  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes),
    rm_expected_result
  )

  # remove twice - should return the same result
  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes) %>%
      rm_or_extract_appended_icd10_dxa(),
    rm_expected_result
  )

  # remove 'X'
  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes,
                                     keep_x = FALSE),
    rm_expected_result_x_rm
  )

  # extract
  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes,
                                         rm_extract = "extract"),
    c(NA,
      NA,
      "D",
      NA,
      "A")
  )

  # extract 'X'
  expect_equal(
    rm_or_extract_appended_icd10_dxa(icd10_codes,
                                     keep_x = FALSE,
                                     rm_extract = "extract"),
    c(NA,
      NA,
      "D",
      "X",
      "XA")
  )
})


# `check_codes()` ---------------------------------------------------------

test_that("`check_codes()` raises an error appropriately", {
  # NA value
  expect_error(check_codes(c(NA, "A")),
               regexp = "cannot contain `NA` values")

  # not character
  expect_error(check_codes(1:2),
               regexp = "must be a character vector")
})

