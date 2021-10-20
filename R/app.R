#' Create a clinical codelist.
#'
#' Launches a shiny app to select clinical codes (see also 'User notes' section
#' below). In general: \enumerate{\item Complete details for essential search
#' fields (leaving 'category' box blank and leaving the 'Ignore case' boxes
#' ticked - if un-ticked, this will not be recorded under
#' 'description_search_strategy'). \item Optionally upload a previous codelist.
#' \item Click 'Download selected'.}
#'
#' @section User notes:
#'
#'   \itemize{ \item Essential fields: 'Disease', either 'Code descriptions
#'   like...' or at least one of 'Codes starting with...'. \item 'Download all':
#'   downloads all codes under the 'Matching clinical codes' tab,
#'   \emph{disregarding} any selections (i.e. the 'selected' column will appear
#'   blank). \item 'Download selected': downloads all codes under the 'Preview
#'   selected codes only' tab. \item If uploading a previous codelist selection,
#'   this must be formatted as required by
#'   \code{\link[ukbwranglr]{validate_clinical_codes}} (see
#'   \code{\link[ukbwranglr]{example_clinical_codes}} for a valid example).
#'   \item ...this will automatically select any codes matching on 'disease',
#'   'code_type' and 'code' (see \code{update_code_selection} function in
#'   \code{utils.R}), and on clicking 'Download selected', will copy over any
#'   values in the 'category' column and set the 'selected' column to 'Yes' only
#'   for these rows. \item ...the full list of uploaded codes can be viewed
#'   under the 'Uploaded codelist' tab, which includes a column called
#'   'included_in_matching', indicating whether each uploaded code is present
#'   ('Yes') or \emph{not} present ('No') in the list under 'Matching clinical
#'   codes'. \item Also, click through the tabs after each step before moving on
#'   to the next. \item The final list of matching codes is codes matching 'Code
#'   descriptions like...' \strong{OR} 'Codes starting with...'. \item
#'   \strong{CAUTION:} make sure there are no inadvertent white spaces (e.g.
#'   'search term', not 'search term  ').}
#'
#' @param all_lkps_maps A named list or path to SQLite database of clinical
#'   codes created by \code{\link{build_all_lkps_maps}} and
#'   \code{\link{all_lkps_maps_to_db}}.
#' @param ... Additional args passed on to \code{\link[shiny]{shinyApp}}
#' @inheritParams shiny::shinyApp
#'
#' @return \code{NULL}
#' @export
#' @import shiny
runCodelistBuilder <- function(all_lkps_maps,
                          options = list(launch.browser = TRUE),
                          ...) {
  if (is.character(all_lkps_maps)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), all_lkps_maps)
    # on.exit(DBI::dbDisconnect(con))
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
  }

  # UI ----------------------------------------------------------------------


  ui <- fluidPage(
    # Application title
    shinyFeedback::useShinyFeedback(),
    titlePanel("Build a clinical codes list"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        width = 2,
        textInput("disease",
                  "Disease",
                  value = "DISEASE"),
        textInput("category",
                  "Category",
                  value = ""),
        textInput("author",
                  "Author",
                  value = "Anon"),
        checkboxGroupInput(
          inputId = "code_type",
          label = "Code types",
          choices = stats::setNames(object = CODE_TYPE_TO_LKP_TABLE_MAP$code,
                                    nm = CODE_TYPE_TO_LKP_TABLE_MAP$code_label),
          selected = c(
            # 'bnf',
            # 'dmd',
            'icd9',
            'icd10',
            'read2',
            # 'read2_drugs',
            'read3',
            'opcs4',
            'data_coding_3',
            'data_coding_4',
            'data_coding_5',
            'data_coding_6'
          )
        ),
        h4("Upload codelist"),
        fileInput("upload",
                  label = NULL,
                  buttonLabel = "Upload"),
        h4("Download codes"),
        downloadButton("download_confirmed_codes",
                       label = "Download all"),
        downloadButton("download_confirmed_codes_selected_only",
                       label = "Download selected")

      ),

      # Show clinical codes matching input settings
      mainPanel(
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Search strategy",
            fluidRow(
              h4("Run query"),
              actionButton(
                inputId = "new_search",
                label = "Search",
                class = "btn-lg btn-success"
              ),
            ),
            column(
              h4("Code descriptions like..."),
              textInput("description_search",
                        "",
                        value = ""),
              checkboxInput(
                "description_search_ignore_case",
                label = "Ignore case",
                value = TRUE
              ),
              textInput("description_search_and",
                        "and... ",
                        value = ""),
              checkboxInput(
                "description_search_and_ignore_case",
                label = "Ignore case",
                value = TRUE
              ),
              textInput("description_search_not",
                        "but not...",
                        value = ""),
              checkboxInput(
                "description_search_not_ignore_case",
                label = "Ignore case",
                value = TRUE
              ),
              width = 6
            ),
            column(
              h4("Codes starting with..."),
              textInput("bnf_starts",
                        "BNF",
                        value = ""),
              textInput("dmd_starts",
                        "DMD",
                        value = ""),
              textInput("icd9_starts",
                        "ICD-9",
                        value = ""),
              textInput("icd10_starts",
                        "ICD-10",
                        value = ""),
              textInput("read2_starts",
                        "Read 2",
                        value = ""),
              textInput("read2_drugs_starts",
                        "Read 2 drugs",
                        value = ""),
              textInput("read3_starts",
                        "Read 3",
                        value = ""),
              textInput("opcs4_starts",
                        "OPCS4",
                        value = ""),
              width = 6
            ),
            fluidRow(verbatimTextOutput("n_matching_codes"))
          ),
          tabPanel(
            "Matching clinical codes",
            h4("Select codes"),
            reactable::reactableOutput("matching_codes")
          ),
          tabPanel(
            "Preview selected codes only",
            reactable::reactableOutput("selected_matching_codes_preview"),
          ),
          tabPanel(
            "Uploaded codelist",
            tableOutput("uploaded_file"),
            reactable::reactableOutput("uploaded_codelist")
          )
        ),
        width = 9
      )
    )
  )

  server <- function(input, output, session) {
    # Search for codes --------------------------------------------------------


    matching_codes <- eventReactive(input$new_search, {
      # prepare to match codes starting with...
      code_starts_params <- tibble::tribble(
        ~ code_type,
        ~ starts_with,
        "bnf",
        input$bnf_starts,
        "dmd",
        input$dmd_starts,
        "icd9",
        input$icd9_starts,
        "icd10",
        input$icd10_starts,
        "read2",
        input$read2_starts,
        "read2_drugs",
        input$read2_drugs_starts,
        "read3",
        input$read3_starts,
        "opcs4",
        input$opcs4_starts
      )

      code_starts_params <- code_starts_params %>%
        dplyr::filter(.data[["starts_with"]] != "")

      # error message if both description and code search boxes are empty
      if ((input$description_search == "") &
          (nrow(code_starts_params) == 0)) {
        validate(
          "Invalid request: a search value is required for at least one of 'Code description like...' or 'Codes starting with...' "
        )
      }

      # set up notification
      notify <- function(msg, id = NULL) {
        showNotification(msg,
                         id = id,
                         duration = NULL,
                         closeButton = FALSE)
      }

      message("Searching for matching codes")
      id <- notify("Searching for matching codes...")
      on.exit(removeNotification(id), add = TRUE)

      # match by description
      if (input$description_search != "") {
        notify("Searching code descriptions...", id = id)

        matching_codes_description <- input$code_type %>%
          purrr::set_names() %>%
          purrr::map(
            ~ code_descriptions_like(
              reg_expr = input$description_search,
              code_type = .x,
              all_lkps_maps = all_lkps_maps,
              ignore_case = input$description_search_ignore_case,
              codes_only = FALSE,
              preferred_description_only = TRUE
            )
          ) %>%
          dplyr::bind_rows(.id = "code_type")

        description_search_strategy <- input$description_search

        if (nrow(matching_codes_description) == 0) {
          matching_codes_description <- NULL
        }

        # AND statement
        if ((input$description_search_and != "") &
            !is.null(description_search_strategy)) {
          matching_codes_description <- matching_codes_description %>%
            dplyr::filter(stringr::str_detect(
              .data[["description"]],
              pattern = stringr::regex(
                input$description_search_and,
                ignore_case = input$description_search_and_ignore_case
              )
            ))

          description_search_strategy <- paste0(
            "'",
            input$description_search,
            "' AND '",
            input$description_search_and,
            "'"
          )

          if (nrow(matching_codes_description) == 0) {
            matching_codes_description <- NULL
          }
        }

        # NOT statement
        if ((input$description_search_not != "") &
            !is.null(description_search_strategy)) {
          matching_codes_description <- matching_codes_description %>%
            dplyr::filter(stringr::str_detect(
              .data[["description"]],
              pattern = stringr::regex(
                input$description_search_not,
                ignore_case = input$description_search_not_ignore_case
              ),
              negate = TRUE
            ))

          description_search_strategy <- paste0(
            "'",
            input$description_search,
            "' NOT '",
            input$description_search_not,
            "'"
          )
          if (nrow(matching_codes_description) == 0) {
            matching_codes_description <- NULL
          }
        }
      } else {
        matching_codes_description <- NULL
        description_search_strategy <- NA
      }

      # match codes starting with
      if (nrow(code_starts_params) > 0) {
        notify("Searching for codes starting with...", id = id)

        matching_codes_starts_with <-
          code_starts_params$code_type %>%
          purrr::set_names() %>%
          purrr::map(
            ~ codes_starting_with(
              codes = code_starts_params[code_starts_params$code_type == .x,]$starts_with,
              code_type = .x,
              all_lkps_maps = all_lkps_maps,
              codes_only = FALSE,
              preferred_description_only = TRUE,
              standardise_output = TRUE
            )
          ) %>%
          dplyr::bind_rows()

        if (nrow(matching_codes_starts_with) == 0) {
          matching_codes_starts_with <- NULL
        }

      } else {
        matching_codes_starts_with <- NULL
      }

      # combine results (codes matching EITHER search criteria), or UI message if not matching codes found
      if (is.null(matching_codes_description) &
          is.null(matching_codes_starts_with)) {
        notify("No codes found matching search criteria!", id = id)
        validate("No codes found matching search criteria!")
      } else if (!is.null(matching_codes_description) &
                 is.null(matching_codes_starts_with)) {
        matching_codes <- matching_codes_description
      } else if (is.null(matching_codes_description) &
                 !is.null(matching_codes_starts_with)) {
        matching_codes <- matching_codes_starts_with
      } else {
        # combine
        matching_codes <-
          dplyr::full_join(
            matching_codes_description,
            matching_codes_starts_with,
            by = c("code", "description", "code_type")
          )
      }

      notify("Final steps...", id = id)
      # add columns recording search strategy
      matching_codes$description_search_strategy <-
        description_search_strategy

      matching_codes$code_starts_search_strategy <- NA

      if (!is.null(matching_codes_starts_with)) {
        code_starts_search_strategy <- paste0(code_starts_params$code_type,
                                              ": '",
                                              code_starts_params$starts_with, "'") %>%
          stringr::str_c(sep = "", collapse = "; ")

        code_starts_search_strategy <- paste0("STARTS WITH - ",
                                              code_starts_search_strategy)

        matching_codes$code_starts_search_strategy <- code_starts_search_strategy
      }

      # add column showing included code types
      matching_codes$included_code_types <-
        stringr::str_c(input$code_type,
                       sep = "",
                       collapse = ", ")

      # reformat
      matching_codes <- matching_codes %>%
        dplyr::mutate(
          "disease" = input$disease,
          "category" = input$category,
          "author" = input$author
        ) %>%
        dplyr::select(tidyselect::all_of(
          c(
            names(ukbwranglr::example_clinical_codes()),
            "description_search_strategy",
            "code_starts_search_strategy",
            "included_code_types"
          )
        ))

      notify("Search complete!", id = id)
      matching_codes
    })

    # Display matching codes --------------------------------------------------

    output$n_matching_codes <- renderText(paste0("N matching codes: ", nrow(matching_codes())))

    output$matching_codes <- reactable::renderReactable({
      req(matching_codes())

      reactable::reactable(
        # data,
        matching_codes(),
        filterable = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100, 200),
        onClick = "select",
        # groupBy = "Field",
        paginationType = "jump",
        selection = "multiple",
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        )
      )
    })

    # Preview selected codes -----------------------------------------------------
    # get selected codes (reactable state)
    selected <-
      reactive(reactable::getReactableState("matching_codes", "selected"))

    observeEvent(selected(), {
      if (!is.null(selected())) {
        updateTabsetPanel(inputId = "preview_selected_codes", selected = "preview_selected_codes")
      } else {
        updateTabsetPanel(inputId = "preview_selected_codes", selected = "empty")
      }
    })

    selected_matching_codes_preview <- reactive({
      req(selected())

      # add column indicating whether code is selected
      selected_matching_codes_preview <- matching_codes()

      selected_matching_codes_preview$selected <-
        ifelse(rownames(matching_codes()) %in% selected(),
               yes = "Yes",
               no = "")

      selected_matching_codes_preview
    })

    output$selected_matching_codes_preview <-
      reactable::renderReactable({
        req(selected_matching_codes_preview())

        reactable::reactable(
          selected_matching_codes_preview()[(selected_matching_codes_preview()$selected) == "Yes",-10],
          filterable = TRUE,
          searchable = TRUE,
          resizable = TRUE,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 25, 50, 100, 200),
          # onClick = "select",
          # groupBy = "Field",
          # selection = "multiple",
          # theme = reactable::reactableTheme(
          #   rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          # )),
          paginationType = "jump"
        )
      })


    # Uploaded codelist -------------------------------------------------------
    # file details
    output$uploaded_file <- renderTable(input$upload)

    # codelist
    uploaded_codelist <- reactive({
      req(input$upload)

      ext <- ukbwranglr:::extract_file_ext(input$upload$name)
      switch(
        ext,
        csv = readr::read_csv(input$upload$datapath),
        xlsx = readxl::read_excel(input$upload$datapath),
        validate("Invalid file; Please upload a .csv file")
      )
    })

    # codelist - only shows matching disease; includes indicator for overlap
    # with codes matching serach criteria
    output$uploaded_codelist <- reactable::renderReactable({
      req(matching_codes())

      # validate uploaded codelist
      ukbwranglr::validate_clinical_codes(uploaded_codelist())

      # filter for matching disease
      uploaded_codelist_overlap <- uploaded_codelist() %>%
        dplyr::filter(.data[["disease"]] == input$disease)

      # validate - are there any matching diseases?
      if (nrow(uploaded_codelist_overlap) == 0) {
        validate("No matching diseases in uploaded codelist file")
      }

      # validate - multiple authors per disease?
      disease_author_combos <-
        paste0(
          unique(uploaded_codelist_overlap$disease),
          unique(uploaded_codelist_overlap$author)
        )

      if (length(disease_author_combos) != 1) {
        validate(
          paste0(
            "Multiple authors detected for ",
            input$disease,
            ". Please supply a file with only one author per disease"
          )
        )
      }

      # add column indicating whether code is included in current list of
      # matching codes
      uploaded_codelist_overlap_included <-
        uploaded_codelist_overlap %>%
        dplyr::semi_join(matching_codes(),
                         by = UPDATE_CODE_SELECTION_MATCHING_VARS) %>%
        dplyr::mutate("included_in_matching" = "Yes")

      uploaded_codelist_overlap_notincluded <-
        uploaded_codelist_overlap %>%
        dplyr::anti_join(matching_codes(),
                         by = UPDATE_CODE_SELECTION_MATCHING_VARS) %>%
        dplyr::mutate("included_in_matching" = "No")

      result <- dplyr::bind_rows(uploaded_codelist_overlap_included,
                                 uploaded_codelist_overlap_notincluded)

      reactable::reactable(
        result,
        filterable = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100, 200),
        # onClick = "select",
        # groupBy = "Field",
        # selection = "multiple",
        # theme = reactable::reactableTheme(
        #   rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        # )),
        paginationType = "jump"
      )
    })

    # update selected rows to include those in data_dict_preselected
    observeEvent(uploaded_codelist(), {
      # currently selected
      currently_selected_rowids <-
        reactable::getReactableState("matching_codes", "selected")

      # get row indices to update
      new_selected <-
        update_code_selection(current_selection = matching_codes(),
                              previous_codelist = uploaded_codelist()) %>%
        tibble::rowid_to_column() %>%
        dplyr::filter(.data[["selected"]] == "Yes") %>%
        dplyr::pull(.data[["rowid"]])

      # union
      updated_selection_rowids <-
        unique(c(currently_selected_rowids, new_selected))

      # update
      reactable::updateReactable("matching_codes", selected = updated_selection_rowids)
    })

    # DOWNLOADS ---------------------------------------------------------------


    # All codes ---------------------------------------------------------------
    output$download_confirmed_codes <- downloadHandler(
      filename = function() {
        paste0(input$disease, "_codes.csv")
      },
      content = function(file) {
        result <- matching_codes()
        result$selected <- NA

        # include search strategy and included codes in first row only
        result$description_search_strategy[2:nrow(result)] <- NA
        result$code_starts_search_strategy[2:nrow(result)] <- NA
        result$included_code_types[2:nrow(result)] <- NA

        readr::write_csv(result, file, na = "")
      }
    )


    # Selected codes ----------------------------------------------------------
    output$download_confirmed_codes_selected_only <-
      downloadHandler(
        filename = function() {
          paste0(input$disease, "_codes_selected_only.csv")
        },
        content = function(file) {
          if (is.null(input$upload)) {
            result <-
              selected_matching_codes_preview()[(selected_matching_codes_preview()$selected) == "Yes", ]
          } else {
            result <-
              update_code_selection(current_selection = selected_matching_codes_preview()[(selected_matching_codes_preview()$selected) == "Yes", ],
                                    previous_codelist = uploaded_codelist())
          }


          # include search strategy and included codes in first row only
          result$description_search_strategy[2:nrow(result)] <- NA
          result$code_starts_search_strategy[2:nrow(result)] <- NA
          result$included_code_types[2:nrow(result)] <- NA

          readr::write_csv(result, file, na = "")
        }
      )

  }

  shinyApp(ui, server, options = options, ...)
}
