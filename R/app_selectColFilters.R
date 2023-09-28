selectColFiltersInput <- function(id, display_filters = FALSE) {
  ns <- NS(id)

  # create select col_filters UI elements programatically
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

  # All UI elements
  ui_list <- list(
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
    ))

  if (display_filters) {

  display_filters_ui <- list(
    fluidRow(
      column(6, h1("Selected"),
             verbatimTextOutput(ns("selected"))),
      column(6, h1("Confirmed selected"),
             verbatimTextOutput(ns("confirmed_selected")))
    ),
    h1("All filters"),
    verbatimTextOutput(ns("all_filters"))
  )

  ui_list <- c(ui_list,
               display_filters_ui)
  }

  # UI
  tagList(
    ui_list
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

    # display select/confirmed col_filters (for debugging)
    output$selected <- renderPrint(selected_filters())

    output$all_filters <-
      renderPrint(get_col_filters(FALSE))

    output$confirmed_selected <- renderPrint(confirmed_selected_filters())

    # cancel selected choices (restore to most recently confirmed choices)
    observeEvent(input$cancel, {
      get_col_filters() %>%
        purrr::iwalk(\(x, idx) {
          tab <- idx

          get_col_filters(defaults_only = FALSE,
                          selected_table = idx) %>%
            purrr::imap(\(x, idx) {
              selected <- confirmed_selected_filters()[[tab]][[idx]]

              if (is.null(selected)) {
                selected <- character(0)
              }

              updateCheckboxGroupInput(inputId = paste(tab, idx, sep = "."),
                                       selected = selected)
            }) %>%
            purrr::set_names(NULL)
        })
    })

    # returns reactive
    confirmed_selected_filters
  })
}

# for testing
selectColFiltersApp <- function() {
  ui <- fluidPage(selectColFiltersInput("col_filters", display_filters = TRUE))

  server <- function(input, output, session) {
    selectColFiltersServer("col_filters")
  }

  shinyApp(ui, server)
}
