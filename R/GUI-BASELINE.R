interactive_baseline_mod_UI <- function(id) {
  ns = shiny::NS(id)
  shiny::fluidPage(shiny::h4('Under construction -- no data will be saved or exported at this time! Some UI elements may not work.'),
                   shiny::tags$script(shiny::HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.code);});')),
                   shiny::tags$head(shiny::tags$style(".shiny-notification {height: 50px; width: 400px; position:fixed; top: 5px; right: 5px;}")),
                   shiny::sidebarLayout(
                     shiny::sidebarPanel(shiny::uiOutput(ns('scan_slider')),
                                         shiny::uiOutput(ns('zoom_slider')),
                                         DT::dataTableOutput(ns('baselines_table')),
                                         shiny::tags$head(shiny::tags$style(shiny::HTML(".irs-from, .irs-to, .irs-min, .irs-max, .irs-single {visibility: hidden !important;}")))
                     ),
                     shiny::mainPanel(
                       shiny::h4(shiny::textOutput(ns('scan_title'))),
                       shiny::fluidRow(align='center', shiny::div('Click to add a point. Double click to Remove.')),
                       shiny::plotOutput(ns("spectrum"),
                                         height = '600px',
                                         width = '100%',
                                         click = ns('click'),
                                         dblclick = ns('dblclick')
                                         # https://github.com/rstudio/shiny/issues/947
                                         # brush = shiny::brushOpts(id = ns('brush'), direction = "y", resetOnNew = TRUE)
                       ),
                       shiny::fluidRow(align='center', "TODO: copy r, table buttons"),
                       shiny::hr(),
                       shiny::fluidRow(align='center',
                                       shiny::actionButton(ns('store'), 'Store'),
                                       shiny::actionButton(ns('apply_future'), 'Apply to following scans'),
                                       shiny::actionButton(ns('apply_all'), 'Apply to all scans'),
                                       shiny::actionButton(ns('reset'), 'Reset this scan'),
                                       shiny::actionButton(ns('reset_all'), 'Reset all scans')
                       ),
                       shiny::hr(),
                       shiny::fluidRow(align='center',
                                       shiny::actionButton(ns('copy_r'), 'Copy baseline parameters as R input'),
                                       shiny::actionButton(ns('copy_tab'), 'Copy baseline parameters as table'),
                                       shiny::downloadButton(ns('export_csv'), 'Export baseline parameters as CSV')
                       )
                     )
                   )
  )
}


interactive_baseline_mod <- function(input, output, session, data, data_name, check_no_data=function() TRUE) {

  ##### WARNING DIALOGS ####
  observe({
    if(!check_no_data()) return()
    if(nrow(data()) == 0) {
      shiny::showModal(shiny::modalDialog(
        title = "No data!",
        "Please import some data and try again.",
        size = 'l'
      ))
      return()
    }
  })

  # TODO instructions button and modal popup


  ##### RANGES ####

  xrange <- shiny::reactive({
    if(nrow(data()) == 0) return(c(NA, NA))
    rev(range(data()[,1]))
  })
  yrange <- shiny::reactive({
    if(nrow(data()) == 0) return(c(NA, NA))
    grDevices::extendrange(range(data()), f=0.05)
  })

  nscans = shiny::reactive({
    n = ncol(data())
    if(n > 1) n = n - 1 # x column
    n
  })

  init_yrange <- shiny::reactive({
    if(nrow(data()) == 0) return(c(NA, NA))
    r = grDevices::extendrange(range(data()[,2]), f=0.05)
    r[[2]] = r[[2]] * 0.1 # Zoom into the baseline
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

  shiny::observeEvent(input$click, {
    # TODO: check y
    baseline_parameters$current = append(baseline_parameters$current, input$click$x)
  })

  shiny::observeEvent(input$dblclick, {
    bsl_x <- current_baseline_x()
    x <- input$dblclick$x
    nearest <- which.min(abs(bsl_x-x))
    if(abs(bsl_x[[nearest]]-x) < abs(diff(xrange()))*0.05) {
      # TODO: check y
      baseline_parameters$current = bsl_x[-nearest]
    }
  })

  # Cannot differentiate click and brush
  # https://github.com/rstudio/shiny/issues/947
  # shiny::observe({
  #   if(!is.null(input$brush)) {
  #     shiny::updateSliderInput(session, 'yzoom', value=input$brush$y)
  #   }
  # })

  ##### BASELINES ####

  baselines <- matrix(NA, 0, 5, dimnames=list(c(), c('x', 'Scans', 'Use buttons', 'Update buttons', 'Clear buttons')))
  baseline_parameters <- shiny::reactiveValues(current=c(), baselines=baselines) # baselines, current


  # TODO: make the buttons work...
  new_row <- function(scans) {
    n = nrow(baseline_parameters$baselines)
    new <- list(
      current_baseline_x(),
      scans,
      as.character(shiny::actionButton(session$ns(paste0('bsl_use_', n)),'Use',counter=0,onclick = paste0('this.setAttribute(\"counter\",parseInt(this.getAttribute(\"counter\"))+1);Shiny.onInputChange(\"',session$ns('bsl_apply'),'\",  this.id+\"_\"+this.getAttribute(\"counter\"))'))),
      as.character(shiny::actionButton(session$ns(paste0('bsl_update_', n)),'Update',counter=0,onclick = paste0('this.setAttribute(\"counter\",parseInt(this.getAttribute(\"counter\"))+1);Shiny.onInputChange(\"',session$ns('bsl_update'),'\",  this.id+\"_\"+this.getAttribute(\"counter\"))'))),
      as.character(shiny::actionButton(session$ns(paste0('bsl_clear_', n)),'Clear',counter=0,onclick = paste0('this.setAttribute(\"counter\",parseInt(this.getAttribute(\"counter\"))+1);Shiny.onInputChange(\"',session$ns('bsl_clear'),'\",  this.id+\"_\"+this.getAttribute(\"counter\"))')))
    )
    baseline_parameters$baselines <- rbind(baseline_parameters$baselines, new, deparse.level = 0)
  }

  # In practical use, the for loop here is faster than sapply et al.
  find_matching_baseline <- function() {
    baselines <- baseline_parameters$baselines
    n <- nrow(baselines)
    if(n == 0) return(NA)
    cur <- current_baseline_x()
    for(i in 1:n) {
      if(identical(cur, baselines[i,1]$x)) {
        return(i)
      }
    }
    return(NA)
  }

  add_scans_to_baseline <- function(bsl_idx, scans) {
    scans = scans[!scans %in% baseline_parameters$baselines[bsl_idx, 2][[1]]]
    baseline_parameters$baselines[[bsl_idx, 2]] <- sort(c(baseline_parameters$baselines[bsl_idx, 2][[1]], scans))
  }

  remove_scans_from_all_baselines <- function(remove_scans) {
    scan_list <- baseline_parameters$baselines[, 2, drop=FALSE]
    if(!length(scan_list)) return()
    new_scan_list <- lapply(scan_list, function(x) x[!x %in% remove_scans])
    baseline_parameters$baselines[, 2] <- new_scan_list
    empty <- sapply(new_scan_list, length) == 0
    if(all(empty)) {
      baseline_parameters$baselines <- baselines
      print(baselines)
      return()
    }
    baseline_parameters$baselines <- baseline_parameters$baselines[!empty, , drop=FALSE]
  }

  store_baseline_for_scans <- function(scans) {
    remove_scans_from_all_baselines(scans)
    match <- find_matching_baseline()
    if(!is.na(match)) {
      add_scans_to_baseline(match, scans)
    } else {
      new_row(scans)
    }
  }

  shiny::observeEvent(input$store, {store_baseline_for_scans(c(current_scan()))})

  shiny::observeEvent(input$apply_all, {store_baseline_for_scans(1:nscans())})

  shiny::observeEvent(input$apply_future, {store_baseline_for_scans(current_scan():nscans())})

  shiny::observeEvent(input$reset, {remove_scans_from_all_baselines(current_scan())})

  shiny::observeEvent(input$reset_all, {remove_scans_from_all_baselines(1:nscans())})

  current_baseline_number <- shiny::reactive({
    scan_list <- baseline_parameters$baselines[, 2, drop=FALSE]
    if(!length(scan_list)) return(NA)
    match <- which(sapply(scan_list, function(x) current_scan() %in% x))
    if(length(match) == 1) return(match)
    return(NA)
  })

  # Update baseline when changing scan if we have a stored baseline for that scan
  shiny::observeEvent(current_scan(), {
    bsl_idx <- current_baseline_number()
    if(!is.na(bsl_idx)) baseline_parameters$current <- baseline_parameters$baselines[[bsl_idx, 1]]
  })

  current_baseline_x <- shiny::reactive({baseline_parameters$current})


  ##### OUTPUT ####

  output$scan_slider <- shiny::renderUI({
    shiny::sliderInput(session$ns("scan"), "Scan", 1, nscans() ,1)
  })

  output$zoom_slider <- shiny::renderUI({
    shiny::sliderInput(session$ns("yzoom"), "Y Zoom", yrange()[[1]], yrange()[[2]], init_yrange(), ticks=F)
  })

  output$scan_title <- shiny::renderText(sprintf("Scan #%s", current_scan()))

  output$baselines_table <- DT::renderDataTable({
    bsl_idx <- current_baseline_number()

    mat <- baseline_parameters$baselines
    bsl_x <- mat[,1, drop=F]
    scans <- mat[,2, drop=F]

    mat[,1] = sapply(bsl_x, length)
    mat[,2] <- sapply(scans, function(x) paste0(jms.classes::numeric_to_string_ranges(x), collapse=', '))
    if(nrow(mat)>1) storage.mode(mat) <- 'character' # renderDataTable has issues if storage mode is list

    mat <- DT::datatable(as.data.frame(mat), # Needed to allow highlighting as has rownames attribute
                         class='compact',
                         escape = c(1,2), # 0 based
                         colnames = c('# Points', "Scans", "", "", ""),
                         autoHideNavigation=TRUE,
                         selection='none',
                         options = list(
                           searching = FALSE,
                           lengthChange=FALSE,
                           ordering=FALSE,
                           processing = FALSE,
                           paging=FALSE
                         )
    )

    if(is.na(bsl_idx)) return(mat)

    #Highlight row
    DT::formatStyle(mat,
      0,
      target = 'row',
      backgroundColor = DT::styleEqual(bsl_idx, 'yellow')
    )
  })

  output$spectrum <- shiny::renderPlot({
    shiny::withProgress(message = 'Preparing plot', value = 1, {
      if(nrow(data()) == 0) return()
      yr=yzoom()
      if(is.null(yr) || any(is.na(yr))) return()

      xy <- data()[,c(1,current_scan() + 1)]
      plot(xy, xlim=xrange(), ylim=yr)

      baseline_x <- current_baseline_x()
      if(length(baseline_x) == 0) return()
      baseline <- make_background(xy, baseline_x, returnFunc = TRUE)
      x = xy[,1]
      y_bsl <- baseline(x)
      lines(x, y_bsl, col='red')
      points(baseline_x, baseline(baseline_x), col='red', pch=16, cex=3)

      lines(x, data()[,current_scan() + 1] - y_bsl, col='gray')
    })
  })

  ##### EXPORT ####

  export_table <- shiny::reactive({baseline_parameters$baselines[,c(1,2)]})

  shiny::observeEvent(input$copy_tab, {
    df_tab = capture.output(print.data.frame(as.data.frame(export_table())))
    jms.classes::clipboard_copy(df_tab)
  })

  output$export_csv <- shiny::downloadHandler(
    filename = 'Baseline_parameters.csv',
    content = function(file) {write.csv(export_table(), file)})


  baseline_data <- shiny::reactive({
    list(
      data = data(),
      parameters = '...', # create a reactive baseline_parameter_matrix() or something based on baseline_parameters$baselines.
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
