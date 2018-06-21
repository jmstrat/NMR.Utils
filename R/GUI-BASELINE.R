interactive_baseline_mod_UI <- function(id) {
  shiny::fluidPage(h4('Under construction'))
}


interactive_baseline_mod <- function(input, output, session, data, data_name) {
  baseline_data <- reactive({
    list(
      data = data(),
      parameters = '...',
      script_input = '# Baseline correction must currently be done manually. See ?jms.classes::make_background'
    )
  })

  return(baseline_data)
}

#' Creates a GUI for correcting the baseline of NMR data
#'
#' @param nmr The NMR data
#' @export
interactiveBaseline <- function(nmr) {
  if(!requireNamespace("shiny", quietly=TRUE)) stop('Interactive baseline correction requires the shiny package to be installed')

  server <- function(input, output, session) {
    data_name = shiny::reactive({deparse(substitute(data))})
    data = shiny::reactive({nmr})
    shiny::callModule(interactive_baseline_mod, "baseline", data, data_name)
  }

  ui <- interactive_baseline_mod_UI("baseline")
  shiny::shinyApp(ui, server)
}
