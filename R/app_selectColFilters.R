selectColFiltersInput <- function(id) {
  ns <- NS(id)

  # create UI programatically
  ui_inputs <- get_col_filters() %>%
    purrr::imap(\(x, idx) {
      tab <- idx

      tabPanel(
        title = tab,
        h1(tab),
        get_col_filters(defaults_only = FALSE,
                        selected_table = idx) %>%
          purrr::imap(
            \(x, idx)
            checkboxGroupInput(
              ns(paste(tab, idx, sep = ".")),
              idx,
              choices = x,
              inline = TRUE,
              selected = get_col_filters(defaults_only = TRUE,
                                         selected_table = tab)[[idx]]
            )
          ) %>%
          purrr::set_names(NULL)
      )
    })

  tagList(
    actionButton(
      ns("restore_defaults"),
      "Restore defaults",
      width = "100%",
      class = "btn-info"
    ),
    navlistPanel(
      !!!purrr::set_names(ui_inputs, NULL),
      id = ns("col_filter_options")
    ),
    fluidRow(
      column(6, actionButton(ns("update_selections"), "Update selections", class = "btn-lg btn-danger")),
      column(6, actionButton(ns("cancel"), "Cancel", class = "btn-lg"))
    ),
    fluidRow(
      column(6, h1("Selected"),
             verbatimTextOutput(ns("selected"))),
      column(6, h1("All filters"),
             verbatimTextOutput(ns("all_filters")))
    ),
    h1("Confirmed selected"),
    verbatimTextOutput(ns("confirmed_selected"))
  )
}

selectColFiltersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # get user-selected values
    selected_filters <- reactive({
      get_col_filters() %>%
        purrr::imap(\(x, idx) {
          tab <- idx

          get_col_filters(defaults_only = FALSE,
                          selected_table = idx) %>%
            purrr::imap(\(x, idx) input[[paste(tab, idx, sep = ".")]])
        })
    })

    output$selected <- renderPrint(lobstr::tree(selected_filters()))

    output$all_filters <-
      renderPrint(lobstr::tree(get_col_filters(FALSE)))

    # restore default selections, on request
    observeEvent(input$restore_defaults, {
      get_col_filters() %>%
        purrr::iwalk(\(x, idx) {
          tab <- idx

          get_col_filters(defaults_only = FALSE,
                          selected_table = idx) %>%
            purrr::imap(\(x, idx)
                        updateCheckboxGroupInput(
                          inputId = paste(tab, idx, sep = "."),
                          selected = get_col_filters(defaults_only = TRUE,
                                                     selected_table = tab)[[idx]]
                        )) %>%
            purrr::set_names(NULL)
        })
    })

    # confirm selected choices
    confirmed_selected_filters <- eventReactive(input$update_selections, ignoreNULL = FALSE, ignoreInit = FALSE, valueExpr =  {
      selected_filters()
    })

    output$confirmed_selected <- renderPrint(lobstr::tree(confirmed_selected_filters()))

    # cancel selected choices (restore to most recently confirmed choices)
    observeEvent(input$cancel, {
      get_col_filters() %>%
        purrr::iwalk(\(x, idx) {
          tab <- idx

          get_col_filters(defaults_only = FALSE,
                          selected_table = idx) %>%
            purrr::imap(\(x, idx)
                        updateCheckboxGroupInput(
                          inputId = paste(tab, idx, sep = "."),
                          selected = confirmed_selected_filters()[[tab]][[idx]]
                        )) %>%
            purrr::set_names(NULL)
        })
    })

    # returns reactive
    confirmed_selected_filters
  })
}

# for testing
selectColFiltersApp <- function() {
  ui <- fluidPage(selectColFiltersInput("col_filters"))

  server <- function(input, output, session) {
    selectColFiltersServer("col_filters")
  }

  shinyApp(ui, server)
}
