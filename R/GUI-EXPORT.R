export_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Processing scripts"),
    shiny::fluidRow(
      shiny::column(
        12,
        "Use the buttons below to create a script that will reproduce the processing done with this wizard."
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::uiOutput(ns("rstudioUI"), inline=TRUE),
        shiny::actionButton(ns("copy_script"), "Copy processing script", style="margin-top:20px"),
        shiny::downloadButton(ns("save_script"), "Save processing script", style="margin-top:20px")
      )
    )
  )
}

export_server <- function(input, output, session, data, data_name, data_filename, script, ...) {
  shiny::observeEvent(input$copy_script, {
    jms.classes::clipboard_copy(script())
  })

  output$save_script <- shiny::downloadHandler(
    filename=function() {
      paste0(tools::file_path_sans_ext(data_filename()), "-nmr-processing.R")
    },
    content=function(file) {
      writeLines(script(), file)
    }
  )

  output$rstudioUI <- shiny::renderUI({
    # documentNew was introduced in version 1.1.640
    if (rstudioapi::isAvailable("1.1.640")) {
      shiny::actionButton(session$ns("openRstudio"), "Open script in RStudio", style="background: #77dd77; margin-top:20px;")
    }
  })

  shiny::observeEvent(input$openRstudio, {
    gui_try_show_error({
      rstudioapi::documentNew(script())
    }, "making a new document")
  })

  export_script <- shiny::reactive({
    sprintf(
      "# Processed Data can be exported using the following command:\n# export(%s, '%s')\n\n# See ?NMR.Utils for details on other methods to process the data (including fitting and animations)",
      data_name(),
      paste0(tools::file_path_sans_ext(data_filename()), "-processed-data.csv")
    )
  })

  # Exclude from bookmarking
  shiny::setBookmarkExclude(c("copy_script", "save_script", "openRstudio"))

  list(
    script=export_script
  )
}
