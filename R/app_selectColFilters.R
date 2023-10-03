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
    shinyjs::useShinyjs(),
    actionButton(
      ns("restore_defaults"),
      "Restore defaults",
      width = "100%",
      class = "btn-lg btn-info"
    ),
    fluidRow(
      column(6, actionButton(ns("undo"), "Undo", width = "100%", class = "btn-lg")),
      column(6, actionButton(ns("update_selections"), "Update", width = "100%", class = "btn-lg btn-danger"))
    ),
    navlistPanel(
      !!!purrr::set_names(ui_inputs, NULL),
      id = ns("col_filter_options")
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

selectColFiltersServer <-
  function(id,
           default_filters = get_col_filters(defaults_only = TRUE),
           confirmation_modal = TRUE) {

  confirmed_selected_filters <- reactiveVal(default_filters)

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

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

    # toggle action buttons
    observe({
      shinyjs::toggleState(ns("restore_defaults"), condition = !identical(selected_filters(), default_filters), asis = TRUE)
      shinyjs::toggleState(ns("undo"), condition = !identical(selected_filters(), confirmed_selected_filters()), asis = TRUE)
      shinyjs::toggleState(ns("update_selections"), condition = !identical(selected_filters(), confirmed_selected_filters()), asis = TRUE)
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
                          selected = default_filters[[tab]][[idx]]
                        )) %>%
            purrr::set_names(NULL)
        })
    })

    # confirm selected choices
    observeEvent(input$update_selections, {
      if (confirmation_modal) {
      showModal(
        modalDialog(
          "This will update all saved queries. Are you sure you want to continue?",
          title = "Confirm",
          footer = tagList(
            actionButton(ns("confirm_cancel"), "Cancel"),
            actionButton(ns("confirm_proceed"), "Proceed", class = "btn btn-danger")
          )
        )
      ) } else {
        confirmed_selected_filters(selected_filters())
      }
    })

    observeEvent(input$confirm_cancel, {
      removeModal()
    })

      observeEvent(
        input$confirm_proceed,
        handlerExpr = {
          confirmed_selected_filters(selected_filters())
          removeModal()
        }
      )

    # display select/confirmed col_filters (for debugging)
    output$selected <- renderPrint(selected_filters())

    output$all_filters <-
      renderPrint(get_col_filters(FALSE))

    output$confirmed_selected <- renderPrint(confirmed_selected_filters())

    # cancel selected choices (restore to most recently confirmed choices)
    observeEvent(input$undo, {
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
    reactive(confirmed_selected_filters())
  })
}

# for testing
selectColFiltersApp <- function(display_filters = TRUE, confirmation_modal = TRUE) {
  ui <- fluidPage(selectColFiltersInput("col_filters", display_filters = display_filters))

  server <- function(input, output, session) {
    selectColFiltersServer("col_filters", confirmation_modal = confirmation_modal)
  }

  shinyApp(ui, server)
}
