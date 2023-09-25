selectColFiltersInput <- function(id) {

  ns <- NS(id)

  # create UI programatically
  ui_inputs <- get_col_filters() %>%
    purrr::imap(
      \(x, idx) {
        tab <- idx

        tabPanel(
          title = tab,
          h1(tab),
          get_col_filters(defaults_only = FALSE,
                          selected_table = idx) %>%
            purrr::imap(\(x, idx)
                        checkboxGroupInput(ns(
                          paste(tab, idx, sep = ".")
                        ),
                        idx,
                        choices = x,
                        selected = get_col_filters(defaults_only = TRUE,
                                                   selected_table = tab)[[idx]])) %>%
            purrr::set_names(NULL
            )
      )
    })

  tagList(
    tabsetPanel(
      !!!purrr::set_names(ui_inputs, NULL),
      id = ns("col_filter_options"),
      type = "pills"
    ),
    h1("Selected"),
    verbatimTextOutput(ns("selected")),
    h1("All filters"),
    verbatimTextOutput(ns("all_filters"))
  )
}

selectColFiltersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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

selectColFiltersApp()
