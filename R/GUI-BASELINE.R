interactive_baseline_mod_UI <- function(id) {
  ns = shiny::NS(id)
  shiny::fluidPage(shiny::h4('Under construction'),
                   shiny::tags$script(shiny::HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.code);});')),
                   shiny::tags$head(shiny::tags$style(".shiny-notification {height: 50px; width: 400px; position:fixed; top: 5px; right: 5px;}")),
                   shiny::sidebarLayout(
                     shiny::sidebarPanel(shiny::uiOutput(ns('scan_slider')),
                                         shiny::uiOutput(ns('zoom_slider')),
                                         "some sort of list of different baselines??",
                                         shiny::tags$head(shiny::tags$style(shiny::HTML(".irs-from, .irs-to, .irs-min, .irs-max, .irs-single {visibility: hidden !important;}")))
                     ),
                     shiny::mainPanel(
                       shiny::h4(shiny::textOutput(ns('scan_title'))),
                       shiny::fluidRow(align='center', shiny::div('Interacive plot instructions....')),
                       shiny::plotOutput(ns("spectrum"),
                                         height = '600px',
                                         width = '100%',
                                         click = ns('click'),
                                         dblclick = ns('dblclick'),
                                         brush = shiny::brushOpts(id = ns('brush'), direction = "x", resetOnNew = TRUE)
                       ),
                       shiny::fluidRow(align='center', "some sort of descriptive output..."),
                       shiny::hr(),
                       shiny::fluidRow(align='center',
                                       "buttons to do stuff go here..."
                       )
                     )
                   )
  )
}


interactive_baseline_mod <- function(input, output, session, data, data_name) {

  ##### WARNING DIALOGS ####
  observe({
    if(length(data()) == 0) {
      shiny::showModal(shiny::modalDialog(
        title = "No data to phase!",
        "Please import some data and try again.",
        size = 'l'
      ))
      return()
    }
  })


  ##### RANGES ####

  xrange <- shiny::reactive({
    if(length(data()) == 0) return(c(NA, NA))
    rev(range(data()[,1]))
  })
  yrange <- shiny::reactive({
    if(length(data()) == 0) return(c(NA, NA))
    grDevices::extendrange(range(data()), f=0.05)
  })

  nscans = shiny::reactive({
    n = ncol(data())
    if(n > 1) n = n - 1 # x column
    n
  })

  init_yrange <- shiny::reactive({
    if(length(data()) == 0) return(c(NA, NA))
    r = grDevices::extendrange(range(data()[,2]), f=0.05)
    r[[2]] = r[[2]] * 0.1
    r
  })


  ##### CURRENT PARAMETERS ####

  current_scan <- shiny::reactive({
    scan <- input$scan
    if(is.null(scan)) return(1)
    return(scan)
  })

  yzoom <- shiny::reactive({
    yr=input$yzoom
    if(is.null(yr)) return(init_yrange())
    return(yr)
  })

  shiny::observeEvent(input$click, {print('click')})
  shiny::observeEvent(input$dblclick, {print('dbl click')})
  shiny::observe({
    if(is.null(input$brush)) {
      print("brush off")
    } else {
      print("brush on")
    }
  })

  ##### OUTPUT ####

  output$scan_slider <- shiny::renderUI({
    shiny::sliderInput(session$ns("scan"), "Scan", 1, nscans() ,1)
  })

  output$zoom_slider <- shiny::renderUI({
    shiny::sliderInput(session$ns("yzoom"), "Y Zoom", yrange()[[1]], yrange()[[2]], init_yrange(), ticks=F)
  })

  output$scan_title <- shiny::renderText(sprintf("Scan #%s", current_scan()))

  output$spectrum <- shiny::renderPlot({
    if(length(data()) == 0) return()
    yr=yzoom()
    if(is.null(yr) || any(is.na(yr))) return()

    plot(data()[,c(1,current_scan() + 1)],type='l',xlim=xrange(),xaxs='i',yaxs='i',yaxt='n',ylab='Intensity',ylim=yr)

  })

  baseline_data <- shiny::reactive({
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
