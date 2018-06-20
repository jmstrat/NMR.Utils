interactive_import_mod_UI <- function(id) {
  shiny::fluidPage(h4('Under construction'))
}


interactive_import_mod <- function(input, output, session) {
    imported_data <- reactive({
    list(
      data = nmr2d.data.object(),
      data_name = 'insitu_data',
      nmr_paths = c(real='...', imaginary='...', acqu='...', atmc='...'),
      echem_path = '...',
      script_input = '...'
    )
  })

    #NOTE Should have a save to globalEnv button

  return(imported_data)
}

#' Creates a GUI for importing data
#'
#' @export
interactiveImport <- function() {
  if(!requireNamespace("shiny", quietly=TRUE)) stop('Interactive importing requires the shiny package to be installed')

  server <- function(input, output, session) {
    shiny::callModule(interactive_import_mod, "import")
  }

  ui <- interactive_import_mod_UI("import")
  shiny::shinyApp(ui, server)
}
