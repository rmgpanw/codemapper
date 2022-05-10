# This file is for testing the applications in the apps/ directory.


# snapshot ----------------------------------------------------------------

all_lkps_maps_dummy <- build_all_lkps_maps_dummy()

library(shinytest)

test_that("RunCodelistBuilder() UI works (compare snapshot)", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp(test_path("apps/RunCodelistBuilder/"),
    compareImages = FALSE
  ))
})

# Server ------------------------------------------------------------------

test_that("RunCodelistBuilder_server returns expected number of codes", {
  app <- shinytest::ShinyDriver$new(path = "apps/RunCodelistBuilder/")
  app$setInputs(
    `RunCodelistBuilder-description_search` = "hypertension",
    `RunCodelistBuilder-code_type` = c("icd10"),
    `RunCodelistBuilder-description_search_and` = "essential|primary",
    `RunCodelistBuilder-description_search_not` = "intracranial|pulmonary",
    `RunCodelistBuilder-new_search` = "click"
  )

  expect_equal(
    app$getValue("RunCodelistBuilder-n_matching_codes"),
    "N matching codes: 2"
  )
})
