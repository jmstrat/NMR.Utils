interactive_plotting_mod_UI <- function(id) {
  shiny::fluidPage(shiny::h4('Under construction'))
}


interactive_plotting_mod <- function(input, output, session, data, data_name) {
  plot_data <- shiny::reactive({
    list(
      parameters = '...',
      script_input = '# Plotting must currently be done manually. See ?plot.nmr2dinsitu.data.object'
    )
  })

  return(plot_data)
}

#' Creates a GUI for generating plots of insitu data
#'
#' @param nmr The NMR data
#' @export
interactivePlotting <- function(nmr) {
  if(!requireNamespace("shiny", quietly=TRUE)) stop('Interactive plotting requires the shiny package to be installed')

  server <- function(input, output, session) {
    data_name = shiny::reactive({deparse(substitute(data))})
    data = shiny::reactive({nmr})
    shiny::callModule(interactive_plotting_mod, "plot", data, data_name)
  }

  ui <- interactive_plotting_mod_UI("plot")
  shiny::shinyApp(ui, server)
}
