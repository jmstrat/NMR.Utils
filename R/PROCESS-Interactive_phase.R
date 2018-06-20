interactive_phase_mod_UI <- function(id) {
  shiny::fluidPage(shiny::uiOutput(shiny::NS(id)('ui')))
}

interactive_phase_mod_renderUI <- function(ns, nscans, xrange, yrange, init_yrange, init_apk0_range, init_apk1_range) {
  shiny::fluidPage(shiny::tags$script(shiny::HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.code);});')),
                   shiny::tags$head(shiny::tags$style(".shiny-notification {height: 50px; width: 400px; position:fixed; top: 5px; right: 5px;}")),
                   shiny::sidebarLayout(
                     shiny::sidebarPanel(shiny::sliderInput(ns("scan"), "Scan", 1, nscans() ,1),
                                         shiny::sliderInput(ns("p0"), "P0", -180, 180, 0),
                                         shiny::sliderInput(ns("p1"), "P1", -180, 180, 0),
                                         shiny::sliderInput(ns("pivot"), "Pivot", xrange()[[2]], xrange()[[1]], init_apk0_range()[['max']]),
                                         shiny::sliderInput(ns("yzoom"), "Y Zoom", yrange()[[1]], yrange()[[2]], init_yrange(), ticks=F),
                                         shiny::fluidRow(
                                           shiny::column(10, shiny::sliderInput(ns("apk0"), "APK P0 Range", 0, 1, init_apk0_range()[['range']], ticks=F)),
                                           shiny::column(2, shiny::actionButton(ns('apk0_button'), 'Set', style='margin-top:35px;', disabled=TRUE))
                                         ),
                                         shiny::fluidRow(
                                           shiny::column(10, shiny::sliderInput(ns("apk1"), "APK P1 Range", 0, 1, init_apk1_range()[['range']], ticks=F)),
                                           shiny::column(2, shiny::actionButton(ns('apk1_button'), 'Set', style='margin-top:35px;', disabled=TRUE))
                                         ),
                                         shiny::includeCSS(system.file('www/pretty-checkbox.css', package='NMR.Utils')),
                                         shiny::tags$div(class="pretty p-switch p-fill", style='font-size: 30px; padding-top:20px;',
                                                         shiny::tags$input(type='checkbox', id=ns('show_apk'), checked='checked'),
                                                         shiny::tags$div(class='state p-primary',
                                                                         shiny::tags$label(
                                                                           shiny::div('Show APK Ranges - P0 blue, P1 green', style='font-size: 15px; font-weight: bold; position: relative; top: -5px;')
                                                                         )
                                                         )
                                         ),
                                         shiny::tags$head(shiny::tags$style(shiny::HTML(".irs-from, .irs-to, .irs-min, .irs-max, .irs-single {visibility: hidden !important;}")))
                     ),
                     shiny::mainPanel(
                       shiny::h4(shiny::textOutput(ns('scan_title'))),
                       shiny::fluidRow(align='center', shiny::div('Double click to set pivot. Drag to set P0 / P1 range.')),
                       shiny::plotOutput(ns("spectrum"),
                                         height = '600px',
                                         width = '100%',
                                         dblclick = ns('dblclick'),
                                         brush = shiny::brushOpts(id = ns('brush'), direction = "x", resetOnNew = TRUE)
                       ),
                       shiny::fluidRow(align='center',shiny::textOutput(ns('phase_params'))),
                       shiny::hr(),
                       shiny::fluidRow(align='center',
                                       shiny::actionButton(ns('do_apk'), 'APK Single'),
                                       shiny::actionButton(ns('do_apk_all'), 'APK All'),
                                       shiny::actionButton(ns('copy_r'), 'Copy phase parameters as R input'),
                                       shiny::actionButton(ns('copy_tab'), 'Copy phase parameters as table'),
                                       shiny::downloadButton(ns('export_csv'), 'Export phase parameters as CSV')
                       )
                     )
                   )
  )
}


interactive_phase_mod <- function(input, output, session, data, data_name, complex_error_dismissable=FALSE) {

  observe({
    footer = if(complex_error_dismissable) modalButton('Dismiss') else ''
    if(!any_complex(data()))
      showModal(modalDialog(
        title = "Data are not complex!",
        "Real data cannot be phased.",
        size = 'l',
        footer = footer
      ))
  })

  xrange <- reactive({rev(range(data()[,1]))})
  yrange <- reactive({grDevices::extendrange(range(makeReal(data())), f=0.05)})

  nscans = reactive({ncol(data())-1})

  init_yrange <- reactive({
    r = grDevices::extendrange(range(Re(data()[,2])), f=0.05)
    r[[2]] = r[[2]] * 0.1
    r
  })

  init_apk_range <- function(initial) {
    x_total_range = diff(xrange())
    apk_real_range =  xrange()[[2]] - (1-initial) * x_total_range
    x = data()[,1]
    x_in_apk = x < apk_real_range[[1]] & x > apk_real_range[[2]]
    peak_x = x[x_in_apk][which.max(Re(data()[x_in_apk, 2]))]
    apk_real_range = peak_x + x_total_range * 0.05 * c(-1, 1)
    list(range = (apk_real_range - xrange()[[2]]) / x_total_range + 1,
         max = peak_x)
  }


  init_apk0_range = reactive({init_apk_range(c(0.5, 1))})
  init_apk1_range = reactive({init_apk_range(c(0, 0.5))})


  output$ui <- shiny::renderUI(interactive_phase_mod_renderUI(session$ns, nscans, xrange, yrange, init_yrange, init_apk0_range, init_apk1_range))

  shiny::observe({
    shiny::updateSliderInput(session, 'pivot', value=input$dblclick$x)
  })

  shiny::observe({
    if(is.null(input$brush)) {
      jms.classes:::disableInput('apk0_button', session)
      jms.classes:::disableInput('apk1_button', session)
    } else {
      jms.classes:::enableInput('apk0_button', session)
      jms.classes:::enableInput('apk1_button', session)
    }
  })

  shiny::observeEvent(input$apk0_button, {
    brush = input$brush
    if(!is.null(brush)) {
      new_range = 1 - (c(brush$xmax, brush$xmin) - xrange()[[2]]) / (diff(xrange())*-1)
      shiny::updateSliderInput(session, 'apk0', value=new_range)
    }
  })

  shiny::observeEvent(input$apk1_button, {
    brush = input$brush
    if(!is.null(brush)) {
      new_range = 1 - (c(brush$xmax, brush$xmin) - xrange[[2]]) / (diff(xrange())*-1)
      shiny::updateSliderInput(session, 'apk1', value=new_range)
    }
  })

  output$phase_params <- shiny::renderText({sprintf("P0: %s; P1: %s; Pivot: %s", input$p0, input$p1, input$pivot)})

  apk0 <- shiny::reactive({(1 - input$apk0) * diff(xrange())*-1 + xrange()[[2]]})
  apk1 <- shiny::reactive({(1 - input$apk1) * diff(xrange())*-1 + xrange()[[2]]})

  phased_parameters <- shiny::reactiveValues(last_scan = 1)

  observe({
    s = input$scan
    if(is.null(s)) return() # Before UI rendered
    s = as.character(s)

    if(s %in% names(phased_parameters)) {
      if(input$scan != phased_parameters$last_scan) {
        shiny::updateSliderInput(session, 'p0', value=phased_parameters[[s]][['p0']])
        shiny::updateSliderInput(session, 'p1', value=phased_parameters[[s]][['p1']])
        shiny::updateSliderInput(session, 'pivot', value=phased_parameters[[s]][['pivot']])
        phased_parameters$last_scan <- input$scan
      } else {
        phased_parameters[[s]][['p0']] = input$p0
        phased_parameters[[s]][['p1']] = input$p1
        phased_parameters[[s]][['pivot']] = input$pivot
      }
    } else {
      phased_parameters[[s]] = list()
      phased_parameters[[s]][['p0']] = input$p0
      phased_parameters[[s]][['p1']] = input$p1
      phased_parameters[[s]][['pivot']] = input$pivot
    }
  })

  output$scan_title <- shiny::renderText(sprintf("Scan #%s", input$scan))

  output$spectrum <- shiny::renderPlot({
    ls = as.character(phased_parameters$last_scan)
    p0 = phased_parameters[[ls]][['p0']]
    p1 = phased_parameters[[ls]][['p1']]
    pivot = phased_parameters[[ls]][['pivot']]

    if( ! isolate(p0 == input$p0 && p1 == input$p1 && pivot == input$pivot)) return()
    phased=.single_phase(data()[,c(1,phased_parameters$last_scan+1)], p0, p1, pivot)
    phased=makeReal(phased)
    yr=input$yzoom
    plot(phased,type='l',xlim=xrange(),xaxs='i',yaxs='i',yaxt='n',ylab='Intensity',ylim=yr)
    abline(v=pivot, col='red')

    if(input$show_apk) {
      rect(apk0()[[1]], yr[[1]], apk0()[[2]], yr[[2]], col=rgb(0,0,1, alpha=0.2))
      rect(apk1()[[1]], yr[[1]], apk1()[[2]], yr[[2]], col=rgb(0,1,0, alpha=0.2))
    }
  }
  )

  shiny::observeEvent(input$do_apk, {
    values = apk_values(data()[,c(1,input$scan+1)], apk0(), apk1(), input$pivot, c(-180, 180), c(-180, 180))
    if(!is.null(values[['p0']])) shiny::updateSliderInput(session, 'p0', value=values[['p0']])
    if(!is.null(values[['p1']])) shiny::updateSliderInput(session, 'p1', value=values[['p1']])
  })

  shiny::observeEvent(input$do_apk_all, {
    shiny::withProgress(message = 'APK', min=0, max=nscans(), value = 1, {
      fun <- function(x) incProgress(1, detail = paste("scan ", x))
      values = apkpseudo2d_values(data(), apk0(), apk1(), input$pivot, c(-180, 180), c(-180, 180), .progress=fun)
      shiny::withProgress(message = 'APK - storing values', min=0, max=nscans(), value = 1, {
        for(i in 1:nscans()) {
          phased_parameters[[as.character(i)]][['p0']] <- values[i, 'p0']
          phased_parameters[[as.character(i)]][['p1']] <- values[i, 'p1']
          phased_parameters[[as.character(i)]][['pivot']] <- input$pivot
          incProgress(1, detail = paste("scan ", i))
        }
      })
      # shiny::updateSliderInput(session, 'p0', value=values[input$scan, 'p0'])
      # shiny::updateSliderInput(session, 'p1', value=values[input$scan, 'p1'])
    })
  })

  phasing_matrix <- reactive({
    pp <- shiny::reactiveValuesToList(phased_parameters)
    include = ! names(pp) == 'last_scan'
    mat = matrix(unlist(pp[include]), ncol=3, byrow=T, dimnames=list(names(pp[include]), c('p0', 'p1', 'pivot')))
    mat[order(as.numeric(rownames(mat))),]
  })

  shiny::observeEvent(input$copy_r, {
    df_input = paste(deparse(phasing_matrix()), collapse='\n')
    df_input = sprintf("phasing_parameters = %s\n%s = phase(%s, phasing_parameters)", df_input, data_name(), data_name())
    jms.classes::clipboard_copy(df_input)
  })

  shiny::observeEvent(input$copy_tab, {
    df_tab = capture.output(print.data.frame(as.data.frame(phasing_matrix())))
    jms.classes::clipboard_copy(df_tab)
  })

  output$export_csv <- shiny::downloadHandler(
    filename = 'Phase_parameters.csv',
    content = function(file) {write.csv(phasing_matrix(), file)})

}

#' Creates a GUI for phasing data
#'
#' @param nmr Complex NMR data to be phased
#' @param allow_real Allow the error message that is shown if real data are provided to be dismissed
#' @export
interactivePhase <- function(nmr, allow_real=FALSE) {
  if(!requireNamespace("shiny", quietly=TRUE)) stop('Interactive phasing requires the shiny package to be installed')

  server <- function(input, output, session) {
    data_name = shiny::reactive({deparse(substitute(data))})
    data = shiny::reactive({nmr})
    shiny::callModule(interactive_phase_mod, "phase", data, data_name, complex_error_dismissable=allow_real)
  }
  ui <- interactive_phase_mod_UI("phase")
  shiny::shinyApp(ui, server)
}
