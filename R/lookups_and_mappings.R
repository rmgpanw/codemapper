extend_bnf_lkp <- function(all_lkps_maps) {
  # each drug entry in `bnf_lkp` gets repeated 8 times: chapter, section,
  # paragraph, subparagraph, chemical_substance, product_name, further_info,
  # full

  bnf_lkp <- all_lkps_maps[["bnf_lkp"]] %>%
    dplyr::collect()

  bnf_lkp %>%
    dplyr::mutate(
      "code_chapter" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 2
      ),
      "code_section" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 4
      ),
      "code_paragraph" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 6
      ),
      "code_subparagraph" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 7
      ),
      "code_chemical_substance" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 9
      ),
      "code_product_name" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 11
      ),
      "code_further_info" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 13
      )
    ) %>%
    dplyr::rename("code_full" = .data[["BNF_Presentation_Code"]]) %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("code"),
      names_to = "BNF_Code_Level",
      values_to = "BNF_Code"
    ) %>%
    dplyr::select(.data[["BNF_Code_Level"]],
                  .data[["BNF_Code"]],
                  tidyselect::everything()) %>%
    dplyr::mutate("BNF_Code_Level" = stringr::str_remove(.data[["BNF_Code_Level"]],
                                                         "code_")) %>%
    dplyr::distinct(.data[["BNF_Code"]],
                    .keep_all = TRUE) %>%
    dplyr::mutate(
      "Description" = dplyr::case_when(
        .data[["BNF_Code_Level"]] == "chapter" ~ .data[["BNF_Chapter"]],
        .data[["BNF_Code_Level"]] == "section" ~ .data[["BNF_Section"]],
        .data[["BNF_Code_Level"]] == "paragraph" ~ .data[["BNF_Paragraph"]],
        .data[["BNF_Code_Level"]] == "subparagraph" ~ .data[["BNF_Subparagraph"]],
        .data[["BNF_Code_Level"]] == "chemical_substance" ~ .data[["BNF_Chemical_Substance"]],
        .data[["BNF_Code_Level"]] == "product_name" ~ .data[["BNF_Product"]],
        .data[["BNF_Code_Level"]] == "further_info" ~ .data[["BNF_Presentation"]],
        .data[["BNF_Code_Level"]] == "full" ~ .data[["BNF_Presentation"]]
      )
    ) %>%
    dplyr::mutate(
      "BNF_Presentation" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Presentation"]]),
      "BNF_Product" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                         "product_name") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Product"]]),
      "BNF_Chemical_Substance" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                              "product_name",
                                                                              "chemical_substance") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Chemical_Substance"]]),
      "BNF_Subparagraph" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                              "product_name",
                                                                              "chemical_substance",
                                                                              "subparagraph") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Subparagraph"]]),
      "BNF_Paragraph" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                           "product_name",
                                                                           "chemical_substance",
                                                                           "subparagraph",
                                                                           "paragraph") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Paragraph"]]),
      "BNF_Section" = dplyr::case_when(!.data[["BNF_Code_Level"]] %in% c("full",
                                                                              "further_info",
                                                                         "product_name",
                                                                         "chemical_substance",
                                                                         "subparagraph",
                                                                         "paragraph",
                                                                         "section") ~ as.character(NA),
                                            TRUE ~ .data[["BNF_Section"]]),
    ) %>%
    dplyr::select(tidyselect::all_of(c(
      "BNF_Code",
      "BNF_Code_Level",
      "BNF_Chapter",
      "BNF_Section",
      "BNF_Paragraph",
      "BNF_Subparagraph",
      "BNF_Chemical_Substance",
      "BNF_Product",
      "BNF_Presentation",
      "Description"
    )))
}

extend_read_v2_drugs_bnf <- function(all_lkps_maps) {
  # get required tables
  read_v2_drugs_bnf <- all_lkps_maps[["read_v2_drugs_bnf"]] %>%
    dplyr::collect()
  read_v2_drugs_lkp <- all_lkps_maps[["read_v2_drugs_lkp"]] %>%
    dplyr::collect()
  bnf_lkp <- all_lkps_maps[["bnf_lkp"]] %>%
    dplyr::collect()

  # extend 'bnf_lkp'
  bnf_lkp_extended <- extend_bnf_lkp(all_lkps_maps)

  # extend `read_v2_drugs_bnf`
  expected_nrow <- nrow(read_v2_drugs_bnf)

  result <- read_v2_drugs_bnf %>%
    # add read code descriptions
    dplyr::left_join(read_v2_drugs_lkp,
                     by = "read_code") %>%
    # extract bnf chapter, section etc from `bnf_code` col in `read_v2_drugs_bnf`
    dplyr::mutate(
      "bnf_chapter" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 2
      ),
      "bnf_section" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 4
      ),
      "bnf_paragraph" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 6
      ),
      "bnf_subparagraph" = paste0(
        .data[["bnf_paragraph"]],
        stringr::str_sub(
          stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
          start = 8,
          end = 8
        )
      )
    ) %>%

    # add BNF details
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Chapter")],
                     by = c("bnf_chapter" = "BNF_Code")) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Section")],
                     by = c("bnf_section" = "BNF_Code")) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Paragraph")],
                     by = c("bnf_paragraph" = "BNF_Code")) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Subparagraph")],
                     by = c("bnf_subparagraph" = "BNF_Code"))

  # check nrows remains the same
  assertthat::assert_that(expected_nrow == nrow(result),
                          msg = "Error! Unexpected number of rows when extending `read_v2_drugs_bnf`")

  return(result)
}
