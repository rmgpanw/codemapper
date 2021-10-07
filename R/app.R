#' Launch a shiny app to select clinical codes
#'
#' TODO
#'
#' @param all_lkps_maps A named list or path to SQLite database
#' @param ... Additional args passed on to \code{\link[shiny]{shinyApp}}
#'
#' @return \code{NULL}
#' @export
#' @import shiny
runCodeMapper <- function(all_lkps_maps,
                          options = list(launch.browser = TRUE),
                          ...) {
  if (is.character(all_lkps_maps)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), all_lkps_maps)
    # on.exit(DBI::dbDisconnect(con))
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
  }

  ui <- fluidPage(# Application title
    shinyFeedback::useShinyFeedback(),
    titlePanel("Build a clinical codes list"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        width = 2,
        textInput("disease",
                  "Disease",
                  value = "Diabetes"),
        textInput("category",
                  "Category",
                  value = ""),
        textInput("author",
                  "Author",
                  value = "Anon"),
        checkboxGroupInput(
          inputId = "code_type",
          label = "Code types",
          choices = CODE_TYPE_TO_LKP_TABLE_MAP$code,
          selected = c("read2", "read3", "icd9", "icd10", "opcs4")
        )
      ),

      # Show clinical codes matching input settings
      mainPanel(
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Search strategy",
            fluidRow(
              h4("Run query"),
              actionButton(inputId = "new_search",
                           label = "Search",
                           class = "btn-lg btn-success"),
            ),
            column(
              h4("Code descriptions like..."),
              textInput("description_search",
                        "",
                        value = "diabetes"),
              checkboxInput(
                "description_search_ignore_case",
                label = "Ignore case",
                value = TRUE
              ),
              textInput("description_search_and",
                        "and... ",
                        value = "non-insulin|2|II"),
              checkboxInput(
                "description_search_and_ignore_case",
                label = "Ignore case",
                value = TRUE
              ),
              textInput("description_search_not",
                        "but not...",
                        value = "complications"),
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
                        value = "C109"),
              textInput("read2_drugs_starts",
                        "Read 2 drugs",
                        value = ""),
              textInput("read3_starts",
                        "Read 3",
                        value = "C109"),
              textInput("opcs4_starts",
                        "OPCS4",
                        value = ""),
              width = 6
            ),
          ),
          tabPanel(
            "Matching clinical codes",
            h4("Select codes"),
            reactable::reactableOutput("matching_codes")
          ),
          tabPanel(
            "Preview selected codes",
            h4("Confirm selection"),
            actionButton(inputId = "confirm_selection",
                         label = "Confirm",
                         class = "btn-lg btn-success"),
            reactable::reactableOutput("selected_matching_codes_preview")
          ),
          tabPanel(
            "Confirmed codes",
            h4("Download code selection"),
            downloadButton("download_confirmed_codes",
                           label = "Download"),
            reactable::reactableOutput("confirmed_codes_reactable")
          )
        ),
        width = 9
      )
    ))

  server <- function(input, output, session) {

    # Search for codes --------------------------------------------------------
    matching_codes <- eventReactive(input$new_search, {

      # prepare to match codes starting with...
      code_starts_params <- tibble::tribble(
        ~ code_type, ~ starts_with,
        "bnf", input$bnf_starts,
        "dmd", input$dmd_starts,
        "icd9", input$icd9_starts,
        "icd10", input$icd10_starts,
        "read2", input$read2_starts,
        "read2_drugs", input$read2_drugs_starts,
        "read3", input$read3_starts,
        "opcs4", input$opcs4_starts
      )

      code_starts_params <- code_starts_params %>%
        dplyr::filter(.data[["starts_with"]] != "")

      # error message if both description and code search boxes are empty
      if ((input$description_search == "") &
          (nrow(code_starts_params) == 0)) {
            validate("A search value is required for at least one of 'Code description like...' or 'Codes starting with...' ")
      }

      # set up notification
      notify <- function(msg, id = NULL) {
        showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
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

        if (input$description_search_and != "") {
          matching_codes_description <- matching_codes_description %>%
            dplyr::filter(stringr::str_detect(
              .data[["description"]],
              pattern = stringr::regex(
                input$description_search_and,
                ignore_case = input$description_search_and_ignore_case
              )
            ))

          description_search_strategy <- paste0("'",
                                                input$description_search,
                                                "' AND '",
                                                input$description_search_and,
                                                "'")
        }

        if (input$description_search_not != "") {
          matching_codes_description <- matching_codes_description %>%
            dplyr::filter(stringr::str_detect(
              .data[["description"]],
              pattern = stringr::regex(
                input$description_search_not,
                ignore_case = input$description_search_not_ignore_case
              ),
              negate = TRUE
            ))

          description_search_strategy <- paste0("'",
                                                input$description_search,
                                                "' NOT '",
                                                input$description_search_not,
                                                "'")
        }
      } else {
        matching_codes_description <- NULL
        description_search_strategy <- NA
      }

      # match codes starting with
      if (nrow(code_starts_params) > 0) {
        notify("Searching for codes starting with...", id = id)

        matching_codes_starts_with <- code_starts_params$code_type %>%
          purrr::set_names() %>%
          purrr::map(
            ~ codes_starting_with(
              codes = code_starts_params[code_starts_params$code_type == .x, ]$starts_with,
              code_type = .x,
              all_lkps_maps = all_lkps_maps,
              codes_only = FALSE,
              preferred_description_only = TRUE,
              standardise_output = TRUE
            )
          ) %>%
          dplyr::bind_rows()

      } else {
        matching_codes_starts_with <- NULL
      }

      # combine results, or UI message if not matching codes found
      if (is.null(matching_codes_description) & is.null(matching_codes_starts_with)) {
        notify("No codes found matching search criteria!", id = id)
        validate("No codes found matching search criteria!")
      } else if (!is.null(matching_codes_description) & is.null(matching_codes_starts_with)) {
        matching_codes <- matching_codes_description
      } else if (is.null(matching_codes_description) & !is.null(matching_codes_starts_with)) {
        matching_codes <- matching_codes_starts_with
      } else {
        matching_codes_overalapping_code_types <-
          dplyr::inner_join(
            matching_codes_description,
            matching_codes_starts_with,
            by = c("code", "description", "code_type")
          )

        matching_codes_description <- matching_codes_description %>%
          dplyr::filter(!.data[["code_type"]] %in% code_starts_params$code_type)

        matching_codes <- dplyr::bind_rows(
          matching_codes_overalapping_code_types,
          matching_codes_description
        )
      }

      notify("Final steps...", id = id)
      # add columns recording search strategy
      matching_codes$description_search_strategy <-
        description_search_strategy

      matching_codes$code_starts_search_strategy <- NA

      if (!is.null(matching_codes_starts_with)) {
        for (code_type in code_starts_params$code_type) {
          matching_codes$code_starts_search_strategy <- ifelse(
            matching_codes$code_type == code_type,
            yes = paste0("'",
                         code_starts_params[code_starts_params$code_type == code_type, ]$starts_with,
                         "'"),
            no = matching_codes$code_starts_search_strategy
          )
        }
      }

      # reformat
      matching_codes <- matching_codes %>%
        dplyr::mutate("disease" = input$disease,
                      "category" = input$category,
                      "author" = input$author) %>%
        dplyr::select(tidyselect::all_of(c(names(ukbwranglr::example_clinical_codes()),
                                           "description_search_strategy",
                                           "code_starts_search_strategy")))

      notify("No codes found matching search criteria!", id = id)
      matching_codes
    })

    # Display matching codes --------------------------------------------------
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
        ))
    })

    # Preview selected codes -----------------------------------------------------
    # get selected codes (reactable state)
    selected <- reactive(reactable::getReactableState("matching_codes", "selected"))

    selected_matching_codes_preview <- reactive({
      req(selected())

      # reformat
      # selected_matching_codes_preview <- matching_codes() %>%
      #   dplyr::mutate("disease" = input$disease,
      #                 "category" = input$category,
      #                 "author" = input$author) %>%
      #   dplyr::select(tidyselect::all_of(c(names(ukbwranglr::example_clinical_codes()),
      #                                      "description_search_strategy",
      #                                      "code_starts_search_strategy")))

      # add column indicating whether code is selected
      selected_matching_codes_preview <- matching_codes()

      selected_matching_codes_preview$selected <-
        ifelse(rownames(matching_codes()) %in% selected(),
               yes = "Yes",
               no = "")

      selected_matching_codes_preview
    })

    output$selected_matching_codes_preview <- reactable::renderReactable({
      req(selected_matching_codes_preview())

      reactable::reactable(
        selected_matching_codes_preview()[(selected_matching_codes_preview()$selected) == "Yes", -9],
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


    # Confirmed selected codes ------------------------------------------------
    confirmed_codes <- reactiveValues(df = NULL)

    observeEvent(input$confirm_selection, {
      req(selected_matching_codes_preview())
      confirmed_codes$df <- dplyr::bind_rows(confirmed_codes$df,
                                             selected_matching_codes_preview())
    })

    output$confirmed_codes_reactable <- reactable::renderReactable({
      req(confirmed_codes$df)

      reactable::reactable(
        confirmed_codes$df,
        filterable = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100, 200),
        groupBy = c("disease", "code_type"),
        paginationType = "jump"
      )
    })

    # DOWNLOADS ---------------------------------------------------------------

    output$download_confirmed_codes <- downloadHandler(
      filename = function() {
        paste0(input$disease, "_codes.csv")
      },
      content = function(file) {
        readr::write_csv(confirmed_codes$df, file, na = "")
      }
    )

  }

  shinyApp(ui, server, options = options, ...)
}
