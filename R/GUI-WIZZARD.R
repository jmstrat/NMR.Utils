wizzard_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::navbarPage('Insitu NMR Processing Wizzard',id = ns('tabs'),
                      shiny::tabPanel('Import', interactive_import_mod_UI(ns('import'))),
                      shiny::tabPanel('Phasing', interactive_phase_mod_UI(ns('phase'))),
                      shiny::tabPanel('Baseline', interactive_baseline_mod_UI(ns('baseline'))),
                      shiny::tabPanel('Plot', interactive_plotting_mod_UI(ns('plot'))),
                      shiny::tabPanel('Export',
                                      h4('Under construction -- buttons to save png, pdf. Inputs for width, height, res'),
                                      shiny::actionButton(ns("copy_script"), "Copy processing script"),
                                      shiny::downloadButton(ns('save_script'), 'Save processing script'))
    )
  )
}

wizzard_mod <- function(input, output, session) {
  import_parameters <- shiny::callModule(interactive_import_mod, "import")

  import_data <- shiny::reactive({import_parameters()$data})
  data_name <- shiny::reactive({import_parameters()$data_name})
  data_filename <- shiny::reactive({
    path = import_parameters()$nmr_paths[[1]]
    if(is.null(path)) return()
    basename(import_parameters()$nmr_paths[[1]])
  })

  import_script <- shiny::reactive({import_parameters()$script_input})


  should_check_complex <- shiny::reactive({input$tabs == 'Phasing'})

  phased_data_parameters <- shiny::callModule(interactive_phase_mod, "phase",
                                              import_data, data_name,
                                              complex_error_dismissable=TRUE,
                                              check_complex_reactive=should_check_complex)

  phased_data <- shiny::reactive({makeReal(phased_data_parameters()$data)})
  phased_script <- shiny::reactive({phased_data_parameters()$script_input})

  baseline_data_parameters <- shiny::callModule(interactive_baseline_mod, "baseline", phased_data, data_name)

  baseline_data <- shiny::reactive({baseline_data_parameters()$data})
  baseline_script <- shiny::reactive({baseline_data_parameters()$script_input})

  plot_parameters <- shiny::callModule(interactive_plotting_mod, "plot", baseline_data, data_name)
  plot_script <- shiny::reactive({plot_parameters()$script_input})

  hash_line_length = 50
  hash_line <- function(n) paste0(rep_len("#", n), collapse='')
  hash_header = function(x) {
    l = nchar(x)
    lpad = ceiling((hash_line_length - l - 2) / 2)
    rpad = hash_line_length - lpad - l - 2
    paste0(hash_line(lpad), ' ', x, ' ', hash_line(rpad))
  }

  script <- shiny::reactive({
    sprintf("library(NMR.Utils)\nlibrary(Echem.Data)\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s",
            hash_header('IMPORT'),
            import_script(),
            hash_header('PHASE'),
            phased_script(),
            hash_header('BASELINE'),
            baseline_script(),
            hash_header('PLOT'),
            plot_script()
    )
  })

  shiny::observeEvent(input$copy_script, {
    jms.classes::clipboard_copy(script())
  })

  output$save_script <- shiny::downloadHandler(
    filename = paste0(tools::file_path_sans_ext(data_filename()), '-nmr-processing.R'),
    content = function(file) {writeLines(script(), file)})

}


#' Wizzard for processing insitu data
#'
#' @export
insitu_gui <- function() {
  if(!requireNamespace("shiny", quietly=TRUE)) stop('Interactive processing requires the shiny package to be installed')
  if(!requireNamespace("shinyFiles", quietly=TRUE)) stop('Interactive processing requires the shinyFiles package to be installed')
  if(!requireNamespace("shinyBS", quietly=TRUE)) stop('Interactive processing requires the shinyBS package to be installed')


  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  shiny::addResourcePath('www', system.file('www', package='jms.classes'))
  server <- function(input, output, session) {
    shiny::callModule(wizzard_mod, "wizzard")
  }
  ui <- wizzard_mod_UI("wizzard")
  shiny::shinyApp(ui, server)
}
