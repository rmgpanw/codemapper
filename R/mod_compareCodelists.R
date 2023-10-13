compareCodelistsInput <- function(id) {
  ns <- NS(id)

  tagList(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    selectInput(
      ns("code_type"),
      "Code type",
      choices = ""
    ),
    fluidRow(
      column(
        6,
        radioButtons(ns("input_codelist1_type"), "", choices = c("Query", "Look up")),
        selectInput(ns("input_codelist1"), "Codelist 1", choices = NULL)
      ),
      column(
        6,
        radioButtons(ns("input_codelist2_type"), "", choices = c("Query", "Look up")),
        selectInput(ns("input_codelist2"), "Codelist 2", choices = NULL)
      )
    ),
    actionButton(ns("compare"), "Compare", class = "btn-lg btn-success"),
    tableOutput(ns("compare_codelists_summary")),
    csvDownloadButton(ns("compare_codelists"), filename = paste0(Sys.Date(), "_", "codelist_comparison.csv")),
    reactable::reactableOutput(ns("compare_codelists"))
  )
}

compareCodelistsServer <-
  function(id, saved_queries, saved_lookups) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      observe({
        available_code_types <- unique(c(names(saved_queries()$objects), names(saved_lookups())))

        selected <- input$code_type

        updateSelectInput(
          inputId = "code_type",
          choices = get_code_type_labels(available_code_types = available_code_types),
          selected = selected
        )
      })

      observe({
        # update select codelist input based on code type and saved query/lookup
        updateSelectInput(inputId = "input_codelist1", choices = switch(
          input$input_codelist1_type,
          "Query" = as.list(saved_queries()$objects[[input$code_type]]),
          "Look up" = as.list(names(saved_lookups()[[input$code_type]]))
        ))

        updateSelectInput(inputId = "input_codelist2", choices = switch(
          input$input_codelist2_type,
          "Query" = as.list(saved_queries()$objects[[input$code_type]]),
          "Look up" = as.list(names(saved_lookups()[[input$code_type]]))
        ))
      })

      observe({
        # require 2 different codelists to be selected
        shinyjs::toggleState(
          ns("compare"),
          condition = isTruthy(input$input_codelist1) &
            isTruthy(input$input_codelist2) &
            (input$input_codelist1 != input$input_codelist2),
          asis = TRUE
        )
      })

      codelist_comparison <-
        eventReactive(input$compare,
                      ignoreInit = TRUE,
                      valueExpr = {
                        df_1 <- switch(
                          input$input_codelist1_type,
                          "Query" = saved_queries()$results[[input$input_codelist1]],
                          "Look up" = saved_lookups()[[input$code_type]][[input$input_codelist1]]
                        )

                        df_2 <- switch(
                          input$input_codelist2_type,
                          "Query" = saved_queries()$results[[input$input_codelist2]],
                          "Look up" = saved_lookups()[[input$code_type]][[input$input_codelist2]]
                        )

                        # combine codelists, and indicate which codes are shared/unique
                        result <-
                          dplyr::bind_rows(df_1, df_2) |>
                          dplyr::distinct()

                        result[["compare"]] <- dplyr::case_when(
                          (result[["code"]] %in% df_1[["code"]]) &
                            (!result[["code"]] %in% df_2[["code"]]) ~ input$input_codelist1,
                          (result[["code"]] %in% df_2[["code"]]) &
                            (!result[["code"]] %in% df_1[["code"]]) ~ input$input_codelist2,
                          (result[["code"]] %in% df_1[["code"]]) &
                            (result[["code"]] %in% df_2[["code"]]) ~ "Both",
                          TRUE ~ "Error!"
                        )

                        stopifnot(sum(result[["compare"]] == "Error!") == 0)

                        result$compare <-
                          factor(result$compare, levels = unique(c(result$compare, "Both")))

                        result
                      })

      output$compare_codelists <- reactable::renderReactable({
        app_reactable(codelist_comparison())
      })

      output$compare_codelists_summary <- renderTable({
        codelist_comparison()$compare %>%
          table() %>%
          as.matrix() %>%
          t() %>%
          as.data.frame()
      })

    })
  }
