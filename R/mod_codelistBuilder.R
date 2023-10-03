

# PUBLIC ------------------------------------------------------------------

## App ---------------------------------------------------------------------


#' Shiny app for building clinical codelists.
#'
#' Launches a Shiny app. Note, requires `all_lkps_maps` database file.
#'
#' @inheritParams get_child_codes
#' @param ... Additional args passed on to \code{\link[shiny]{shinyApp}}
#' @inheritParams shiny::shinyApp
#'
#' @return `NULL`
#' @export
#' @import shiny
#'
#' @examples
#' \dontrun{
#' # build dummy all_lkps_maps database
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#' all_lkps_maps_db <- all_lkps_maps_to_db(
#'   all_lkps_maps = all_lkps_maps_dummy,
#'   db_path = tempfile(fileext = ".db")
#' )
#'
#' # launch app
#' RunCodelistBuilder(all_lkps_maps = all_lkps_maps_db)
#' }
RunCodelistBuilder <- function(all_lkps_maps = NULL,
                               options = list(launch.browser = TRUE),
                               ...) {
  # Requires a data base file, the path to which will be made available as an
  # environmental variable
  if (is.null(all_lkps_maps)) {
    all_lkps_maps <- Sys.getenv("ALL_LKPS_MAPS_DB")
    if (all_lkps_maps == "") {
      all_lkps_maps <- "all_lkps_maps.db"
    }
    stopifnot(file.exists(all_lkps_maps))
  } else {
    stopifnot(rlang::is_string(all_lkps_maps))
    stopifnot(file.exists(all_lkps_maps))
  }

  # connect to database file path if `all_lkps_maps` is a string, or `NULL`
  con <- check_all_lkps_maps_path(all_lkps_maps)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # determine which lookup tables are available
  available_code_types <- CODE_TYPE_TO_LKP_TABLE_MAP %>%
    dplyr::filter(.data[["lkp_table"]] %in% DBI::dbListTables(con)) %>%
    dplyr::pull(.data[["code"]])

  # determine which mapping tables are available
  available_maps <- CLINICAL_CODE_MAPPINGS_MAP %>%
    dplyr::filter(.data[["mapping_table"]] %in% DBI::dbListTables(con)) %>%
    dplyr::select(tidyselect::all_of(c("from", "to")))

  available_maps <- dplyr::bind_rows(
    available_maps,
    available_maps %>%
      dplyr::rename(
        "to" = tidyselect::all_of("from"),
        "from" = tidyselect::all_of("to")
      )
  )

  # reactive value to store saved queries - to be shared between modules
  saved_queries <- reactiveVal(list(
    objects = list(),
    results = new.env(),
    results_meta = new.env(),
    dag = list(nodes = data.frame(),
               edges = data.frame())
  ))

  # reactive value to store saved look ups - to be shared between modules
  saved_lookups <- reactiveVal(list())

  ui <- shinydashboard::dashboardPage(
    skin = "purple",
    shinydashboard::dashboardHeader(title = "CODEMINER"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Build",
                                 tabName = "builder_tab",
                                 icon = icon("pen")),
        shinydashboard::menuItem(
          "Search",
          tabName = "lookup_tab",
          icon = icon("magnifying-glass")
        ),
        shinydashboard::menuItem(
          "Compare",
          tabName = "compare_tab",
          icon = icon("code-compare")
        ),
        shinydashboard::menuItem(
          "Bookmark",
          tabName = "bookmark_tab",
          icon = icon("bookmark")
        )
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "builder_tab",
                                fluidPage(
                                  codelistBuilderInput(
                                    "builder",
                                    available_code_types = available_code_types,
                                    available_maps = available_maps
                                  )
                                )),
        shinydashboard::tabItem(
          tabName = "compare_tab",
          h2("Compare codelists"),
          fluidPage(
            compareCodelistsInput("compare_codelists")
          )
        ),
        shinydashboard::tabItem(tabName = "lookup_tab",
                                fluidPage(
                                  lookupCodesInput("lookup_codes",
                                                   available_code_types = available_code_types)
                                )),
        shinydashboard::tabItem(
          tabName = "bookmark_tab",
          h2("Save/restore codelists"),
          fluidPage(
            downloadButton("download", label = "Save", icon = icon("bookmark")),
            fileInput("upload", "Restore")
          )
        )
      )
    )
  )

  server <- function(input, output, sesion) {
    codelistBuilderServer("builder",
                          available_maps = available_maps,
                          saved_queries = saved_queries)

    compareCodelistsServer("compare_codelists",
                           saved_queries = saved_queries,
                           saved_lookups = saved_lookups)

    lookupCodesServer("lookup_codes",
                      available_maps = available_maps,
                      saved_lookups = saved_lookups)

    output$download <- downloadHandler(
      filename = function() {
        "codeminer.rds"
      },
      content = function(file) {
        saveRDS(list(saved_queries = saved_queries,
                     saved_lookups = saved_lookups),
                file = file)
      }
    )

    observeEvent(input$upload, {
      new_bookmark <- readRDS(input$upload$datapath)
      saved_queries(new_bookmark$saved_queries())
      saved_lookups(new_bookmark$saved_lookups())
    })
  }

  withr::with_envvar(
    new = c("ALL_LKPS_MAPS_DB" = all_lkps_maps),
    code = shinyApp(ui, server, options = options, ...),
    action = "replace"
  )
}


#' UI for codelistBuilder shiny module
#'
#' @param id character
#' @param available_code_types Character vector of code types (must match code
#'   types in `CODE_TYPE_TO_LKP_TABLE_MAP$code`)
#' @param available_maps Data frame with columns 'from' and 'to' (must match
#'   'from-to' and 'to-from' combinations in `CLINICAL_CODE_MAPPINGS_MAP`)
#'
#' @return UI (html)
#' @noRd
#' @import shiny
codelistBuilderInput <-
  function(id, available_code_types, available_maps) {
    stopifnot(all(available_code_types %in% CODE_TYPE_TO_LKP_TABLE_MAP$code))

    ns <- NS(id)

    tagList(
      shinyFeedback::useShinyFeedback(),
      shinyjs::useShinyjs(),
      jqbr::useQueryBuilder(),

      tabsetPanel(
        id = ns("mainpanel"),
        tabPanel(
          "Build query",
          icon = icon("pen"),

          ## Side bar -------------------------------
          sidebarLayout(
            sidebarPanel = sidebarPanel(
              width = 2,
              actionButton(ns("reset_qbr"), "Reset"),
              tabsetPanel(
                id = ns("tabs_select_code_type"),
                type = "hidden",
                tabPanelBody(
                  value = "tab_select_code_type_show",
                  radioButtons(
                    ns("code_type"),
                    "Code type",
                    choices = CODE_TYPE_TO_LKP_TABLE_MAP %>%
                      dplyr::filter(.data[["code"]] %in% !!available_code_types) %>%
                      dplyr::select(tidyselect::all_of(c(
                        "code_label", "code"
                      ))) %>%
                      tibble::deframe() %>%
                      as.list()
                  )
                ),
                tabPanelBody(
                  value = "tab_select_code_type_hide",
                  textOutput(ns("currently_updating_query")),
                  actionButton(ns("cancel_query_update"), "Cancel query update")
                )
              ),
              tabsetPanel(
                id = ns("tabs_load_saved_query"),
                type = "hidden",
                tabPanelBody(value = "tab_load_saved_query_hide"),
                tabPanelBody(
                  value = "tab_load_saved_query_show",
                  actionButton(ns("btn_qb_load_saved_query"), "Load/update query"),
                  ### Saved queries -----------------------------------------------------------

                  selectizeInput(
                    ns("saved_queries_checkboxes"),
                    "Remove saved queries",
                    choices = list("None"),
                    multiple = TRUE
                  ),
                  tabsetPanel(
                    id = ns("tabs_remove_saved_queries"),
                    type = "hidden",
                    tabPanelBody(value = "tab_remove_saved_queries_hide"),
                    tabPanelBody(value = "tab_remove_saved_queries_show",
                                 actionButton(ns(
                                   "remove_saved_queries"
                                 ), "Remove"))
                  )
                )
              )
              # )
              # )
            ),

            ## Main panel ------------------------------------
            mainPanel = mainPanel(
              ### Query builder input -----------------------------------------------------
              jqbr::queryBuilderInput(
                ns("qb"),

                plugins = list(
                  "sortable" = NULL,
                  "filter-description" = list("mode" = "bootbox"),
                  "bt-tooltip-errors" = NULL
                ),

                filters = filters,

                operators = operators,

                rules = list(condition = "AND",
                             rules = list(
                               list(
                                 id = "description",
                                 operator = "read2",
                                 value = "diab",
                                 description = "I'm a description"
                               )
                             )),
                conditions = c("AND", "OR", "NOT"),
                return_value = "rules",
                display_errors = TRUE
              ),

              ### Summarise and run query -------------------------------------------------

              verbatimTextOutput(ns("current_query")),
              actionButton(ns("run"), "Run query", class = "btn-lg btn-success"),


              ### Query results -----------------------------------------------------------

              h1("Result"),
              tabsetPanel(
                id = ns("query_result_tabs"),
                type = "hidden",
                tabPanelBody(value = "initial"),
                tabPanelBody(value = "query_result_empty_or_invalid",
                             verbatimTextOutput(
                               ns("result_empty_or_invalid_message")
                             )),
                tabPanelBody(value = "empty_query",
                             p("Please enter a query")),
                tabPanelBody(
                  value = "query_result",
                  verbatimTextOutput(ns("result_query")),
                  downloadButton(ns("download"), label = "Download report"),
                  tabsetPanel(
                    id = ns("tabs_save_or_update_query"),
                    type = "hidden",
                    tabPanelBody(
                      value = "tab_save_query_input_show",
                      textInput(
                        ns("save_query_name"),
                        "Save as",
                        value = "",
                        placeholder = "Use capitals"
                      ),
                      tabsetPanel(
                        id = ns("tabs_show_save_query_button"),
                        type = "hidden",
                        tabPanelBody(value = "tab_save_query_button_hide"),
                        tabPanelBody(value = "tab_save_query_button_show",
                                     actionButton(ns("save_query"), "Save"))
                      )
                    ),
                    tabPanelBody(
                      value = "tab_update_query_button_show",
                      actionButton(ns("btn_save_updated_query"), "Update", class = "btn-lg btn-danger")
                    )
                  ),
                  textOutput(ns("result_summary")),
                  csvDownloadButton(ns("result"), filename = paste0(Sys.Date(), "_", "codelist.csv")),
                  reactable::reactableOutput(ns("result"))
                )
              )
            )
          )
        ),

        ### Saved queries -----------------------------------
        tabPanel(
          "Saved queries",
          icon = icon("diagram-project"),
          tabsetPanel(
            id = ns("tabs_dag"),
            tabPanel(title = "DAG",
                     visNetwork::visNetworkOutput(NS(
                       id, "dag_visnetwork"
                     ))),
            tabPanel(title = "Nodes",
                     tableOutput(ns("dag_nodes"))),
            tabPanel(title = "Edges",
                     tableOutput(ns("dag_edges")))
          )
        ),

        ### Advanced settings ------------------------------------
        tabPanel(
          "Advanced settings",
          icon = icon("gears"),
          selectColFiltersInput(ns("builder_advanced_settings"),
                                display_filters = FALSE)
        )

      )
    )
  }


#' UI for codelistBuilder shiny module
#'
#' @param id character
#' @param available_code_types Character vector of code types (must match code types in `CODE_TYPE_TO_LKP_TABLE_MAP$code`)
#'
#' @return UI (html)
#' @noRd
#' @import shiny
codelistBuilderServer <-
  function(id,
           available_maps,
           saved_queries = reactiveVal(list(
             objects = list(),
             results = new.env(),
             results_meta = new.env(),
             dag = list(nodes = data.frame(),
                        edges = data.frame())
           ))) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      ## Advanced settings -------------------------------------------------------

      col_filters <-
        selectColFiltersServer("builder_advanced_settings", confirmation_modal = TRUE)

      # TODO - update all saved queries when col_filters() updates

      # observeEvent(col_filters(), {
      #   saved_queries
      # })

      ## Query -------------------------------------------------------------------

      output$current_query <- renderPrint({
        if (is.null(input$qb)) {
          cat("<Enter query>")
        } else {
          custom_qbr_translation(input$qb) %>%
            rlang::expr_deparse() %>%
            cat()
        }
      })

      # observe({
      #   lobstr::tree(input$qb)
      # })


      ## Reset query builder -----------------------------------------------------

      observeEvent(input$reset_qbr,
                   jqbr::updateQueryBuilder(
                     inputId = "qb",
                     setRules = list(condition = "AND",
                                     rules = list(
                                       list(
                                         id = "description",
                                         operator = "read2",
                                         value = "diab",
                                         description = "I'm a description"
                                       )
                                     ))
                   ))


      ## Run query ---------------------------------------------------------------

      query_result <- eventReactive(input$run, {
        # `FALSE` if no query input; `NA` if no results returned by query
        if (is.null(input$qb)) {
          x <- FALSE
        } else {
          query <- custom_qbr_translation(input$qb)

          query_options <- rlang::expr(list(
            codemapper.code_type = !!input$code_type,
            codemapper.map_to = !!input$code_type,
            codemapper.reverse_mapping = "warning",
            codemapper.unrecognised_codes_mapped = "warning",
            codemapper.unrecognised_codes_lookup = "error",
            codemapper.col_filters = !!col_filters()
          ))

          execute_query <-
            purrr::safely(\(x) withr::with_options(
              eval(query_options),
              eval(query, envir = saved_queries()$results)
            ))

          x <- list(
            query = query,
            query_options = query_options,
            result = execute_query(),
            qb = input$qb,
            code_type = input$code_type
          )

          if (!rlang::is_empty(x$result$error) ||
              nrow(x$result$result) == 0) {
            # error, or empty result (no codes matching search criteria)
            x <- x$result
          } else {
            # success
            x$result <- x$result$result

            # get saved query dependencies (if any)
            dependencies <- get_qbr_saved_queries(x$qb) %>%
              unique()

            # append dependencies
            if ((length(dependencies) > 0)) {
              if (nrow(saved_queries()$dag$edges) > 0) {
                dependencies <- dependencies %>%
                  purrr::set_names() %>%
                  purrr::map(
                    ~ find_node_dependencies(
                      graph = dag_igraph(),
                      node = .x,
                      mode = "in",
                      node_rm = FALSE
                    )
                  ) %>%
                  purrr::compact() %>%
                  purrr::reduce(c, .init = NULL) %>%
                  unique() %>%
                  c(dependencies)

                dependencies <- saved_queries()$dag$nodes %>%
                  dplyr::filter(.data[["id"]] %in% !!dependencies) %>%
                  dplyr::arrange(.data[["order"]]) %>%
                  dplyr::pull(tidyselect::all_of("id"))
              }

              # ordered dependencies
              x$dependencies <- dependencies
            }

            # code to generate query result
            query_code <- list(rlang::call2(.fn = "=",
                                            rlang::sym("RESULT"),
                                            query))

            if (length(dependencies) > 0) {
              if (length(dependencies) == 1) {
                query_code_deps <- dependencies %>%
                  purrr::map(
                    ~ rlang::call2(
                      .fn = "=",
                      rlang::sym(.x),
                      saved_queries()$results_meta[[.x]]$query
                    )
                  )

              } else {
                query_code_deps <- dependencies %>%
                  purrr::map(~ {
                    rlang::call2(
                      .fn = "=",
                      rlang::sym(.x),
                      saved_queries()$results_meta[[.x]]$query
                    )
                  })
              }

              # then append final query
              query_code <- c(query_code_deps,
                              query_code)

              # add indicator columns, showing which codes came from which query(/queries)
              for (dep in dependencies) {
                print(dep)
                x$result[[dep]] <-
                  dplyr::case_when(x$result$code %in% saved_queries()$results[[dep]]$code ~ 1)
              }
            }

            x$query_code <- query_code
          }
        }

        x
      })

      query_result_type <- eventReactive(query_result(), {
        if (identical(query_result(), FALSE)) {
          x <- "empty_query"
        } else if (identical(names(query_result()), c("result", "error"))) {
          x <- "query_result_empty_or_invalid"
        } else {
          x <- "query_result"
        }

        x
      })

      observeEvent(query_result_type(), {
        updateTabsetPanel(inputId = "query_result_tabs", selected = query_result_type())
      })

      output$result_empty_or_invalid_message <- renderText({
        req(query_result_type() == "query_result_empty_or_invalid")

        if (!rlang::is_empty(query_result()$error)) {
          validate(paste0("Error! ", query_result()$error$message))
        }

        "No codes found matching search criteria"
      })

      output$result_summary <- renderText({
        req(query_result_type() == "query_result")

        result <- paste0("N results: ",
                         nrow(query_result()$result),
                         ".")

        message(result)

        print(result)
      })

      output$result <- reactable::renderReactable({
        req(query_result_type() == "query_result")

        app_reactable(query_result()$result)
      })

      output$result_query <- renderPrint({
        req(query_result_type() == "query_result")

        query_result()$query_code %>%
          purrr::walk(print)
      })

      # Download query ----------------------------------------------------------

      output$download <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "_", "codelist.html")
        },
        content = function(file) {

          # See also https://stackoverflow.com/a/74948301

          # setup
          TEMPFILE_NAME <- tempfile()
          TEMPFILE_QMD <- paste0(TEMPFILE_NAME, ".qmd")
          TEMPFILE_HTML <- paste0(TEMPFILE_NAME, ".html")

          # write qmd report
          report_template <-
            '---
title: "{TITLE}"
subtitle: "{SUBTITLE}"
author: Me
date: today
date-format: medium
format:
  html:
    code-fold: true
    embed-resources: true
    code-tools:
      source: true
      toggle: true
      caption: Code
---

```{{r}}
library(codemapper)
library(htmltools)

options({QUERY_OPTIONS})
```
# Query

```{{r}}
#| code-fold: false
{QUERY_CODE}
```

# Codelist

```{{r}}

htmltools::browsable(
  tagList(
    tags$button(
      tagList(fontawesome::fa("download"), "Download as CSV"),
      onclick = "{ONCLICK}"
    ),

    reactable::reactable(
      RESULT,
      filterable = TRUE,
      searchable = TRUE,
      resizable = TRUE,
      paginationType = "jump",
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100, 200),
      elementId = "codelist-download"
    )
  )
)
```
'

          report_template |>
            stringr::str_glue(
              TITLE = "My codelist",
              SUBTITLE = "My codelist subtitle",
              QUERY_OPTIONS = rlang::expr_text(query_result()$query_options),
              QUERY_CODE = paste(query_result()$query_code, sep = "", collapse = "\n"),
              ONCLICK = "Reactable.downloadDataCSV('codelist-download', 'codelist.csv')"
            ) |>
            print() |>
            writeLines(con = TEMPFILE_QMD)

          # render report
          quarto::quarto_render(TEMPFILE_QMD, execute_dir = ".")

          file.copy(TEMPFILE_HTML, file)
        }
      )


      # Code type ---------------------------------------------------------------

      observe({
        shinyjs::toggleState(ns("code_type"),
                             condition = !is.null(input$qb),
                             asis = TRUE)
      })

      # Saved queries --------------------------------------------------------------

      dag_igraph <- eventReactive(saved_queries(), {
        if (nrow(saved_queries()$dag$nodes) > 0 &
            nrow(saved_queries()$dag$edges) > 0) {
          # ascertain dependencies using igraph package
          dag <-
            igraph::graph_from_data_frame(
              d = saved_queries()$dag$edges,
              directed = TRUE,
              vertices = saved_queries()$dag$nodes
            )
        } else {
          dag <- NULL
        }

        dag
      })

      # update query builder in response to any of (i) code type change (ii)
      # updated list of saved queries (iii) finish updating a saved query (either
      # cancel or save the update)
      observeEvent(
        list(
          input$code_type,
          saved_queries(),
          input$btn_save_updated_query,
          input$cancel_query_update
        ),
        {
          # update qbr saved query filter
          new_saved_query_filter <- empty_saved_query_filter

          new_saved_query_filter$values <-
            as.list(saved_queries()$objects[[input$code_type]])

          new_saved_query_filter$operators <- list(input$code_type)

          new_description_contains_filter <- description_contains_filter
          new_description_contains_filter$operators <-
            list(input$code_type)

          new_codes_filter <- codes_filter
          new_codes_filter$operators <- list(input$code_type)

          new_map_codes_filter <- map_codes_filter
          new_map_codes_filter$operators <-
            get_available_map_from_code_types(available_maps = available_maps,
                                              to = input$code_type)

          new_map_children_filter <- map_children_filter
          new_map_children_filter$operators <-
            get_available_map_from_code_types(available_maps = available_maps,
                                              to = input$code_type)

          new_child_codes_filter <- child_codes_filter
          new_child_codes_filter$operators <- list(input$code_type)

          jqbr::updateQueryBuilder(
            inputId = "qb",
            setFilters = list(
              new_description_contains_filter,
              new_codes_filter,
              new_child_codes_filter,
              new_map_codes_filter,
              new_map_children_filter,
              new_saved_query_filter
            ),
            setRules = update_qb_operator_code_type(input$qb, input$code_type),
            destroy = TRUE
          )
        }
      )

      ### Save current query ------------------------------------------------------

      # show 'save query' button only if a query name has been entered
      observeEvent(input$save_query_name, {
        duplicate_query_name <-
          (input$save_query_name %in% saved_queries()$objects[[input$code_type]]) &
          (input$save_query_name != "")

        shinyFeedback::feedbackWarning("save_query_name",
                                       duplicate_query_name,
                                       "Query with this name already exists")
        req(!duplicate_query_name)

        tab <-
          ifelse(
            shiny::isTruthy(input$save_query_name) & !duplicate_query_name,
            "tab_save_query_button_show",
            "tab_save_query_button_hide"
          )

        updateTabsetPanel(inputId = "tabs_show_save_query_button", selected = tab)
      })

      # click save query button
      observeEvent(input$save_query,
                   label = "add_to_saved_queries",
                   handlerExpr = {
                     update_saved_queries(
                       query = input$save_query_name,
                       query_result = query_result,
                       saved_queries = saved_queries,
                       code_type = query_result()$code_type
                     )

                     # reset query name input
                     updateTextInput(inputId = "save_query_name",
                                     value = "")
                   })


      ### Show saved queries ------------------------------------------------------

      observe(label = "update_saved_queries_checkboxes",
              x = {
                # update saved queries check boxes
                updateSelectizeInput(inputId = "saved_queries_checkboxes",
                                     choices = saved_queries()$objects)
              })

      # update qbr load_query input tab and options
      observeEvent(
        eventExpr = saved_queries(),
        label = "update_qbr_load_query_input",
        ignoreInit = TRUE,
        handlerExpr = {
          tab <- ifelse(
            shiny::isTruthy(names(saved_queries()$objects)),
            "tab_load_saved_query_show",
            "tab_load_saved_query_hide"
          )

          updateTabsetPanel(inputId = "tabs_load_saved_query", selected = tab)
        }
      )

      ### Load/update saved queries -----------------------------------------------

      observeEvent(input$btn_qb_load_saved_query,
                   ignoreInit = TRUE,
                   handlerExpr = {
                     showModal(
                       modalDialog(
                         selectInput(
                           ns("select_qb_load_saved_query"),
                           label = "Select saved query",
                           choices = saved_queries()$objects,
                           multiple = FALSE,
                         ),
                         title = "Load or modify existing query?",
                         footer = tagList(
                           actionButton(ns("btn_cancel_query_load_update"), "Cancel"),
                           actionButton(ns("btn_load_query"), "Load", class = "btn-lg btn-success"),
                           actionButton(ns("btn_update_query"), "Update", class = "btn btn-danger")
                         )
                       )
                     )
                   })

      observeEvent(input$btn_load_query, {
        selected_saved_query <- input$select_qb_load_saved_query

        updateRadioButtons(
          inputId = "code_type",
          selected = saved_queries()$dag$nodes %>%
            dplyr::filter(.data[["id"]] == !!selected_saved_query) %>%
            dplyr::pull(.data[["group"]])
        )

        jqbr::updateQueryBuilder(
          inputId = "qb",
          setRules = get(selected_saved_query,
                         envir = saved_queries()$results_meta)$qb
        )
        removeModal()
        showNotification(paste0("Loaded ", selected_saved_query))
      })

      observeEvent(input$btn_cancel_query_load_update, {
        removeModal()
      })

      output$currently_updating_query <- renderText({
        req(input$select_qb_load_saved_query)
        paste0(
          "Updating saved query: ",
          input$select_qb_load_saved_query,
          " (",
          input$code_type,
          ")"
        )
      })

      observeEvent(input$btn_update_query, {
        selected_saved_query <- input$select_qb_load_saved_query


        available_saved_queries <-
          as.list(saved_queries()$objects[[input$code_type]]) %>%
          purrr::discard(\(x) x == selected_saved_query) %>%
          as.character()

        # ensure saved query filter only shows upstream dependencies for selected
        # query
        if (!is.null(dag_igraph())) {
          dependencies <- find_node_dependencies(graph = dag_igraph(),
                                                 node = selected_saved_query,
                                                 mode = "in")

          available_saved_queries <- subset(available_saved_queries,!available_saved_queries %in% dependencies)
        }

        # new filter
        new_saved_query_filter <- empty_saved_query_filter

        if (length(available_saved_queries) > 0) {
          new_saved_query_filter$values <-
            as.list(available_saved_queries)
        }

        # finally
        updateRadioButtons(
          inputId = "code_type",
          selected = saved_queries()$dag$nodes %>%
            dplyr::filter(.data[["id"]] == !!selected_saved_query) %>%
            dplyr::pull(.data[["group"]])
        )

        updateTabsetPanel(inputId = "tabs_select_code_type",
                          selected = "tab_select_code_type_hide")

        jqbr::updateQueryBuilder(
          inputId = "qb",
          setFilters = list(
            description_contains_filter,
            codes_filter,
            child_codes_filter,
            map_codes_filter,
            map_children_filter,
            new_saved_query_filter
          ),
          setRules = get(
            input$select_qb_load_saved_query,
            envir = saved_queries()$results_meta
          )$qb
        )

        updateTabsetPanel(inputId = "query_result_tabs", selected = "empty_query")

        updateTabsetPanel(inputId = "tabs_save_or_update_query", selected = "tab_update_query_button_show")

        removeModal()

        showNotification(paste0("Updating ", input$select_qb_load_saved_query))
      })

      observeEvent(input$btn_save_updated_query, {
        # update saved queries
        update_saved_queries(
          query = input$select_qb_load_saved_query,
          query_result = query_result,
          saved_queries = saved_queries,
          code_type = input$code_type
        )

        updateTabsetPanel(inputId = "query_result_tabs", selected = "empty_query")

        updateTabsetPanel(inputId = "tabs_save_or_update_query", selected = "tab_save_query_input_show")

        updateTabsetPanel(inputId = "tabs_select_code_type",
                          selected = "tab_select_code_type_show")
      })

      observeEvent(input$cancel_query_update,
                   {
                     updateTabsetPanel(inputId = "query_result_tabs", selected = "empty_query")

                     updateTabsetPanel(inputId = "tabs_save_or_update_query", selected = "tab_save_query_input_show")

                     updateTabsetPanel(inputId = "tabs_select_code_type",
                                       selected = "tab_select_code_type_show")
                   })

      ### Remove saved queries ----------------------------------------------------

      observe(label = "show_hide_tabs_remove_saved_queries",
              x = {
                tab <- ifelse(
                  shiny::isTruthy(input$saved_queries_checkboxes),
                  "tab_remove_saved_queries_show",
                  "tab_remove_saved_queries_hide"
                )
                updateTabsetPanel(inputId = "tabs_remove_saved_queries", selected = tab)
              })

      updated_dag <- eventReactive(input$remove_saved_queries,
                                   label = "determine_which_saved_queries_to_remove",
                                   valueExpr = {
                                     # determine dependencies
                                     if (!is.null(dag_igraph())) {
                                       # ascertain dependencies using igraph package
                                       dependencies <-
                                         input$saved_queries_checkboxes %>%
                                         purrr::set_names() %>%
                                         purrr::map(~ find_node_dependencies(graph = dag_igraph(),
                                                                             node = .x)) %>%
                                         purrr::compact()

                                       nodes_to_remove <-
                                         c(
                                           input$saved_queries_checkboxes,
                                           purrr::reduce(dependencies, c, .init = NULL)
                                         ) %>%
                                         unique()

                                       # finally
                                       new_dependencies <- list(
                                         nodes = saved_queries()$dag$nodes %>%
                                           dplyr::filter(!.data[["id"]] %in% nodes_to_remove),
                                         edges = saved_queries()$dag$edges %>%
                                           dplyr::filter(
                                             !.data[["to"]] %in% nodes_to_remove,
                                             !.data[["from"]] %in% nodes_to_remove
                                           )
                                       )
                                     } else {
                                       nodes_to_remove <- input$saved_queries_checkboxes
                                       new_dependencies <-
                                         list(
                                           nodes = saved_queries()$dag$nodes %>%
                                             dplyr::filter(!.data[["id"]] %in% nodes_to_remove),
                                           edges = data.frame()
                                         )
                                       dependencies <- list()
                                     }

                                     # return updated DAG
                                     list(
                                       new_dependencies = new_dependencies,
                                       nodes_to_remove = nodes_to_remove,
                                       dependencies = dependencies
                                     )
                                   })

      observeEvent(updated_dag(), label = "remove_from_saved_queries",
                   handlerExpr = {
                     # confirm dialog box if removing selected queries will also
                     # impact other queries
                     if (!rlang::is_empty(updated_dag()$dependencies)) {
                       modal_confirm <- modalDialog(
                         paste0(
                           "The following saved queries will also be deleted: ",
                           paste(
                             subset(
                               updated_dag()$nodes_to_remove,!updated_dag()$nodes_to_remove %in% input$saved_queries_checkboxes
                             ),
                             sep = "",
                             collapse = ", "
                           ),
                           "."
                         ),
                         title = "Dependencies detected for selected queries",
                         footer = tagList(
                           actionButton(ns("cancel"), "Cancel"),
                           actionButton(ns("ok"), "Delete", class = "btn btn-danger")
                         )
                       )

                       showModal(modal_confirm)
                     } else {
                       # otherwise, simply remove selected queries
                       remove_saved_queries(updated_dag = updated_dag,
                                            saved_queries = saved_queries)
                     }
                   })

      observeEvent(input$ok, {
        remove_saved_queries(updated_dag = updated_dag,
                             saved_queries = saved_queries)
        showNotification("Saved queries deleted")
        removeModal()
      })

      observeEvent(input$cancel, {
        removeModal()
      })


      ### Render DAG -----------------------------------------------------------

      output$dag_visnetwork <-
        visNetwork::renderVisNetwork(
          visNetwork::visNetwork(
            nodes = saved_queries()$dag$nodes,
            edges = saved_queries()$dag$edges,
            width = "100%"
          ) |>
            visNetwork::visEdges(arrows = "to") |>
            visNetwork::visNodes(shadow = list(enabled = TRUE, size = 10)) |>
            visNetwork::visHierarchicalLayout(direction = "LR",
                                              sortMethod = "directed") |>
            visNetwork::visInteraction(
              zoomView = TRUE,
              dragView = TRUE,
              dragNodes = TRUE
            ) |>
            visNetwork::visGroups() |>
            visNetwork::visLegend() |>
            visNetwork::visOptions(selectedBy = list(variable = "group"))
        )

      output$dag_nodes <- renderTable(saved_queries()$dag$nodes)
      output$dag_edges <- renderTable(saved_queries()$dag$edges)
    })
  }

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
        available_code_types <- unique(c(names(saved_queries()$objects)), names(saved_lookups()))

        selected <- input$code_type

        updateSelectInput(inputId = "code_type",
                          choices = CODE_TYPE_TO_LKP_TABLE_MAP %>%
                            dplyr::filter(.data[["code"]] %in% !!available_code_types) %>%
                            dplyr::select(tidyselect::all_of(c(
                              "code_label", "code"
                            ))) %>%
                            tibble::deframe() %>%
                            as.list(),
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

lookupCodesInput <- function(id, available_code_types) {
  ns <- NS(id)

  tagList(shinyjs::useShinyjs(),
          tabsetPanel(id = ns("mainpanel"),
                      tabPanel(title = "Search", icon = icon("magnifying-glass"),
                               fluidRow(
            column(
              4,
              selectInput(
                ns("code_type"),
                "Code type",
                choices = CODE_TYPE_TO_LKP_TABLE_MAP %>%
                  dplyr::filter(.data[["code"]] %in% !!available_code_types) %>%
                  dplyr::select(tidyselect::all_of(c(
                    "code_label", "code"
                  ))) %>%
                  tibble::deframe() %>%
                  as.list()
              ),
              textAreaInput(ns("codes_input"), "Input", resize = "vertical"),
              radioButtons(ns("map_to"),
                           "Map to",
                           choices = "(Don't map)"),
              actionButton(ns("look_up"), "Look up")
            ),
            column(
              8,
              textInput(ns("save_as"), "Save as"),
              shinyjs::disabled(actionButton(ns(
                "save_recognised"
              ), "Save")),
              tabsetPanel(
                id = ns("codelist_tabs"),
                tabPanel("Recognised",
                         csvDownloadButton(ns("recognised_codes"), filename = paste0(Sys.Date(), "_", "recognised_codes.csv")),
                         reactable::reactableOutput(ns(
                           "recognised_codes"
                         ))),
                tabPanel("Unrecognised",
                         csvDownloadButton(ns("unrecognised_codes"), filename = paste0(Sys.Date(), "_", "unrecognised_codes.csv")),
                         reactable::reactableOutput(ns(
                           "unrecognised_codes"
                         )))
              )
            )
          )),
          tabPanel(
            "Saved lookups",
            icon = icon("table-list"),
            selectInput(ns("select_saved_lookup_code_type"), "Code type", choices = NULL),
            selectInput(ns("select_saved_lookup"), "Saved lookup", choices = NULL),
            actionButton(ns("select_saved_lookup_button"), "Select"),
            actionButton(ns("remove_lookup"), "Remove"),
            reactable::reactableOutput(ns("saved_lookup_reactable"))
          ),
          tabPanel(
            "Advanced settings",
            icon = icon("gears"),
            selectColFiltersInput(ns("lookup_advanced_settings"),
                                  display_filters = FALSE)
          )))
}

lookupCodesServer <-
  function(id,
           available_maps,
           saved_lookups = reactiveVal(list())) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      col_filters <-
        selectColFiltersServer("lookup_advanced_settings", confirmation_modal = FALSE)

      codes_input_cleaned <- reactive(
        input$codes_input %>%
          stringr::str_split_1("\n") %>%
          stringr::str_trim(side = "both") %>%
          subset(.,
                 . != "")
      )

      observe({
        # require input to look up
        shinyjs::toggleState(ns("look_up"),
                             condition = isTruthy(codes_input_cleaned()),
                             asis = TRUE)
      })

      observe({
        updateRadioButtons(
          inputId = "map_to",
          choices = c(
            "(Don't map)",
            get_available_map_from_code_types(available_maps = available_maps,
                                              to = input$code_type)
          )
        )
      })

      codelist <-
        eventReactive(input$look_up,
                      ignoreInit = TRUE,
                      valueExpr = {
                        code_type <- input$code_type

                        recognised <- lookup_codes(
                          codes = codes_input_cleaned(),
                          code_type = input$code_type,
                          preferred_description_only = TRUE,
                          standardise_output = TRUE,
                          unrecognised_codes = "warning",
                          col_filters = col_filters()
                        )

                        if (input$map_to != "(Don't map)") {
                          recognised <- map_codes(
                            codes = recognised,
                            to = input$map_to,
                            unrecognised_codes = "warning",
                            reverse_mapping = "warning",
                            col_filters = col_filters()
                          )

                          code_type <- input$map_to
                        }

                        unrecognised <- lookup_codes(
                          codes = codes_input_cleaned(),
                          code_type = input$code_type,
                          preferred_description_only = TRUE,
                          standardise_output = TRUE,
                          .return_unrecognised_codes = TRUE,
                          col_filters = col_filters()
                        )

                        list(
                          recognised = recognised,
                          unrecognised = unrecognised,
                          code_type = code_type
                        )
                      })

      observe({
        # require input to look up
        shinyjs::toggleState(
          ns("save_recognised"),
          condition = (nrow(codelist()$recognised) > 0) &
            isTruthy(input$save_as),
          asis = TRUE
        )
      })

      output$recognised_codes <-
        reactable::renderReactable(
          app_reactable(codelist()$recognised)
        )

      output$unrecognised_codes <-
        reactable::renderReactable(
            app_reactable(data.frame(Input = codelist()$unrecognised))
        )

      observeEvent(input$save_recognised, {
        new_saved_lookups <- saved_lookups()

        new_saved_lookups[[codelist()$code_type]][[input$save_as]] <-
          codelist()$recognised

        saved_lookups(new_saved_lookups)
      })

      # view saved lookups tab
      observe({
        choices <- names(saved_lookups())

        if (is.null(choices)) {
          choices <- ""
        }

        updateSelectInput(inputId = "select_saved_lookup_code_type",
                          choices = choices)
      })

      observe({
        choices <- names(saved_lookups()[[input$select_saved_lookup_code_type]])

        if (is.null(choices)) {
          choices <- ""
        }

        updateSelectInput(inputId = "select_saved_lookup",
                          choices = choices)
      })

      selected_lookup <- eventReactive(input$select_saved_lookup_button, {

        req(isTruthy(input$select_saved_lookup_code_type))
        req(isTruthy(input$select_saved_lookup))

        list(codelist = saved_lookups()[[input$select_saved_lookup_code_type]][[input$select_saved_lookup]],
             code_type = input$select_saved_lookup_code_type,
             name = input$select_saved_lookup)
      })

      observe({
        shinyjs::toggleState(
          id = ns("select_saved_lookup_button"),
          condition = isTruthy(input$select_saved_lookup_code_type) &
            isTruthy(input$select_saved_lookup),
          asis = TRUE
        )
      })

      observe({
        shinyjs::toggleState(
          id = ns("remove_lookup"),
          condition = isTruthy(input$select_saved_lookup_code_type) &
            isTruthy(input$select_saved_lookup),
          asis = TRUE
        )
      })

      observe({
        shinyjs::toggle(
          id = ns("saved_lookup_reactable"),
          condition = (selected_lookup()$code_type %in% names(saved_lookups())) &
            (selected_lookup()$name %in% names(saved_lookups()[[selected_lookup()$code_type]])),
          asis = TRUE
        )
      })

      output$saved_lookup_reactable <- reactable::renderReactable({
        req(isTruthy(selected_lookup()))

        app_reactable(selected_lookup()$codelist)
        })

      # remove saved lookup
      observeEvent(input$remove_lookup, {
        new_saved_lookup <- saved_lookups()

        new_saved_lookup[[input$select_saved_lookup_code_type]][[input$select_saved_lookup]] <-
          NULL

        new_saved_lookup <- purrr::compact(new_saved_lookup)

        if (!isTruthy(names(new_saved_lookup))) {
          new_saved_lookup <- list()
        }

        saved_lookups(new_saved_lookup)
      })

    })}

# PRIVATE -----------------------------------------------------------------

## Utils -------------------------------------------------------------------

#' Determine which saved queries are down or upstream of a given saved query
#'
#' @param graph An igraph object
#' @param node A string
#' @param mode Either "out" (for downstream dependencies) or "in" (for upstream
#'   dependencies)
#' @param node_rm If `TRUE` (default), remove `node` from output
#'
#' @return A character vector of nodes that are upstream of `node`
#' @noRd
find_node_dependencies <- function(graph,
                                   node,
                                   mode = "out",
                                   node_rm = TRUE) {
  stopifnot(rlang::is_string(node))
  stopifnot(igraph::is.dag(graph))
  stopifnot(mode %in% c("out", "in"))

  result <- igraph::all_simple_paths(graph = graph,
                                     from = node,
                                     mode = mode) %>%
    purrr::map( ~ as.list(.x) %>%
                  purrr::imap( ~ .y)) %>%
    purrr::flatten() %>%
    as.character() %>%
    unique()

  if (node_rm) {
    result <- subset(result,!result == node)
  }

  result
}

#' Update `saved_queries`
#'
#' Add to `saved_queries` or update an existing saved query. Steps:
#'
#' - Determine whether a new query is being created, or whether an existing query is being updated.
#' - Save current query
#' - Recalculate DAG and dependency order
#' - If updating a previously saved query, recalculate downstream dependencies.
#'
#' @param query_result Reactive.
#' @param saved_queries Reactive.
#' @param query character
#' @param code_type character
#'
#' @return Called for side effects within an observer.
#' @noRd
update_saved_queries <- function(query,
                                 query_result,
                                 code_type,
                                 saved_queries) {
  stopifnot(is.reactive(query_result))
  stopifnot(is.reactive(saved_queries))

  # Determine whether query is new or already exists in saved_queries
  existing_query <- query %in% saved_queries()$objects[[code_type]]

  # Save current query (results and metadata)

  ## results
  current_query_result <-
    query_result()$result[, 1:3] # save first 3 cols only

  new_saved_queries <- saved_queries()$results

  assign(query, current_query_result, envir = new_saved_queries)

  ## meta
  current_query_meta <- list(
    query = query_result()$query,
    result_summary = nrow(query_result()$result),
    qb = query_result()$qb
  )

  new_saved_queries_meta <- saved_queries()$results_meta

  assign(query, current_query_meta, envir = new_saved_queries_meta)

  ## dependencies
  nodes <- data.frame(id = query,
                      label = query,
                      group = code_type)
  nodes <- dplyr::bind_rows(saved_queries()$dag$nodes,
                            nodes)

  if (existing_query) {
    nodes <- nodes %>%
      dplyr::distinct(.data[["id"]],
                      .keep_all = TRUE)
  }

  edges <-
    data.frame(from = get_qbr_saved_queries(query_result()$qb))

  # determine DAG order - add to `nodes` df
  if (nrow(edges) > 0) {
    edges$to <- query
    edges <- dplyr::bind_rows(saved_queries()$dag$edges,
                              edges)

    if (existing_query) {
      edges <- edges %>%
        dplyr::distinct()
    }

    dag_igraph <- igraph::graph_from_data_frame(d = edges,
                                                directed = TRUE,
                                                vertices = nodes)

    dep_order <- igraph::topo_sort(graph = dag_igraph,
                                   mode = "out") %>%
      as.list() %>%
      purrr::imap( ~ .y) %>%
      as.character()

    dep_order <- data.frame(id = dep_order)
    dep_order$order <- rownames(dep_order)

    nodes <- nodes %>%
      dplyr::select(-tidyselect::any_of("order")) %>%
      dplyr::full_join(dep_order,
                       by = "id")

    # if existing query, update downstream dependencies
    if (existing_query) {
      upstream_deps <- find_node_dependencies(
        graph = dag_igraph,
        node = query,
        mode = "in",
        node_rm = TRUE
      )

      for (x in upstream_deps) {
        updated_result <-
          withr::with_options(list(codemapper.code_type = code_type),
                              eval(get(
                                x = x, envir = saved_queries()$results_meta
                              )$query))

        assign(x, updated_result, envir = new_saved_queries)
      }
    }

  } else {
    nodes$order <- rownames(nodes)
    edges <- saved_queries()$dag$edges
  }

  new_dependencies <- list(nodes = nodes,
                           edges = edges)

  # finally
  saved_queries(
    list(
      objects = get_objects_from_nodes(nodes),
      results = new_saved_queries,
      results_meta = new_saved_queries_meta,
      dag = new_dependencies
    )
  )
}

#' Remove selected queries from `saved_queries`
#'
#' Updates `saved_queries` (reactive value) and its corresponding check box
#' input.
#'
#' @param updated_dag Reactive. List with items 'new_dependencies',
#'   'nodes_to_remove' and 'dependencies'. Used to determine how `saved_queries`
#'   should be updated.
#' @param saved_queries Reactive.
#'
#' @return Called for side effects within an observer.
#' @noRd
remove_saved_queries <- function(updated_dag,
                                 saved_queries) {
  stopifnot(is.reactive(updated_dag))
  stopifnot(is.reactive(saved_queries))

  # update `saved_queries()`

  ## results
  new_saved_queries <- saved_queries()$results

  rm(list = updated_dag()$nodes_to_remove,
     envir = new_saved_queries)

  ## meta
  new_saved_queries_meta <- saved_queries()$results_meta

  rm(list = updated_dag()$nodes_to_remove,
     envir = new_saved_queries_meta)

  ## finally
  if (isTruthy(updated_dag()$new_dependencies$nodes$group)) {
    new_objects <-
      get_objects_from_nodes(updated_dag()$new_dependencies$nodes)
  } else {
    new_objects <- list("None")
  }

  saved_queries(
    list(
      objects = new_objects,
      results = new_saved_queries,
      results_meta = new_saved_queries_meta,
      dag = updated_dag()$new_dependencies
    )
  )

  # update saved queries check boxes
  updateCheckboxGroupInput(inputId = "saved_queries_checkboxes",
                           choices = ls(new_saved_queries))
}

# return vector of saved query names from a qbr input
get_qbr_saved_queries <- function(x) {
  recurse <- function(x) {
    if (is.list(x) &
        identical(names(x),
                  c("id", "field", "type", "input", "operator", "value"))) {
      switch(
        x$id,
        "description" = NULL,
        "child_codes" = NULL,
        "codes" = NULL,
        "saved_query" = x$value,
        "map_children" = NULL,
        "map_codes" = NULL,
        stop("Unrecognised filter!")
      )

    } else if (is.list(x)) {
      purrr::map(x, get_qbr_saved_queries)
    } else {
      NULL
    }
  }

  unlist(recurse(x))
}

# function to convert rules to expressions
convert_rules_to_expr <- function(x) {
  if (is.list(x) &
      identical(names(x),
                c("id", "field", "type", "input", "operator", "value"))) {
    switch(
      x$id,
      "description" = rlang::call2(.fn = "DESCRIPTION",
                                   x$value),
      "saved_query" = as.symbol(x$value),
      "child_codes" = rlang::call2(.fn = "CHILDREN",
                                   x$value),
      "codes" = rlang::call2(.fn = "CODES",
                             x$value),
      "map_codes" = rlang::call2(.fn = "MAP",
                                 x$value,
                                 from = x$operator),
      "map_children" = rlang::call2(
        .fn = "MAP",
        rlang::call2(.fn = "CHILDREN",
                     x$value,
                     code_type = x$operator)
      ),
      stop("Unrecognised filter!")
    )

  } else if (is.list(x)) {
    purrr::map(x, convert_rules_to_expr)
  } else {
    x
  }
}



# function to apply set operations to rule sets
apply_setops <- function(x) {
  if (is.list(x) &
      "rules" %in% names(x)) {
    setop <- switch(
      x$condition,
      AND = "%AND%",
      OR = "%OR%",
      NOT = "%NOT%",
      ... = stop("Invalid operator")
    )

    x$rules %>%
      purrr::reduce( ~ rlang::call2(.fn = setop,
                                    apply_setops(.x),
                                    apply_setops(.y)))
  } else if (is.list(x)) {
    # stop("Invalid input")
    x[[1]]
  } else {
    x
  }
}

# function to process qbr rules
custom_qbr_translation <- function(qbr_rules) {
  # process qb rules
  qbr_rules %>%
    purrr::map(convert_rules_to_expr) %>%
    apply_setops()
}


# function to update code_type operator
update_qb_operator_code_type <- function(x, code_type) {
  if (is.list(x) &
      identical(names(x),
                c("id", "field", "type", "input", "operator", "value"))) {
    if (x$id %in% c("map_codes", "map_children")) {
      x$operator <- NULL
    } else {
      x$operator <- code_type
    }

    x

  } else if (is.list(x)) {
    purrr::map(x, update_qb_operator_code_type, code_type)
  } else {
    x
  }
}

#' Helped for updating saved query reactive value objects
#'
#' @param nodes A data frame
#'
#' @return A named list for use with `choices` argument to `selectizeInput()`
#' @noRd
get_objects_from_nodes <- function(nodes) {
  nodes$group %>%
    unique() %>%
    purrr::set_names() %>%
    purrr::map( ~ nodes %>%
                  dplyr::filter(.data[["group"]] == !!.x) %>%
                  dplyr::pull(.data[["id"]]) %>%
                  as.list())
}

get_available_map_from_code_types <- function(available_maps, to) {
  available_maps %>%
    dplyr::filter(.data[["to"]] == !!to) %>%
    dplyr::pull(.data[["from"]]) %>%
    as.list()
}

## jqbr filters and operators --------------------------------------------------------------------

# qbr filters
empty_saved_query_filter <- list(
  id = "saved_query",
  label = "Saved query",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("read2"),
  description = "I'm a description"
)

child_codes_filter <- list(
  id = "child_codes",
  label = "Children",
  type = "string",
  operators = list("read2"),
  description = "I'm a description"
)

description_contains_filter <- list(
  id = "description",
  label = "Description",
  type = "string",
  operators = list("read2"),
  description = "I'm a description"
)

codes_filter <- list(
  id = "codes",
  label = "Codes",
  type = "string",
  operators = list("read2"),
  description = "I'm a description"
)

map_codes_filter <- list(
  id = "map_codes",
  label = "Map codes",
  type = "string",
  operators = list(),
  description = "I'm a description"
)

map_children_filter <- list(
  id = "map_children",
  label = "Map children",
  type = "string",
  operators = list(),
  description = "I'm a description"
)

filters <- list(
  description_contains_filter,
  codes_filter,
  map_codes_filter,
  map_children_filter,
  child_codes_filter,
  empty_saved_query_filter
)

code_type_operators <- CODE_TYPE_TO_LKP_TABLE_MAP %>%
  dplyr::pull(.data[["code"]]) %>%
  purrr::map(\(x) list(
    type = x,
    nb_inputs = 1,
    multiple = FALSE,
    apply_to = "string"
  ))

operators <- c(code_type_operators,
               list(
                 list(
                   type = "equals",
                   nb_inputs = 1,
                   multiple = FALSE,
                   apply_to = "string"
                 ),
                 list(
                   type = "from ICD-10",
                   optgroup = "Map",
                   nb_inputs = 1,
                   multiple = FALSE,
                   apply_to = "string"
                 )
               ))

## reactable ---------------------------------------------------------------

csvDownloadButton <-
  function(id,
           filename = paste0(Sys.date(), "_", "codelist.csv"),
           label = "Download as CSV") {
    tags$button(
      tagList(icon("download"), label),
      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
    )
  }

app_reactable <- function(df) {
  reactable::reactable(
    df,
    filterable = TRUE,
    searchable = TRUE,
    resizable = TRUE,
    paginationType = "jump",
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100, 200)
  )
}
