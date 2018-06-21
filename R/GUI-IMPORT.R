interactive_import_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(".shiny-notification {height: 50px; width: 400px; position:fixed; top: 5px; right: 5px;}")),
    shiny::fluidRow(shiny::column(10, "Use topspin's totxt function. Or \"save --> as text file\"")),
    shiny::fluidRow(
      shiny::column(10,shiny::textInput(ns("nmr_real_text"), label=NULL, placeholder = "Path to 2D NMR txt file", width='100%')),
      shiny::column(2,jms.classes::fileChooserUI(ns('nmr_real'),label="Select File",title='Please choose the 2D NMR txt file',multiple=FALSE,style="width:100%; margin-top: 0px;"))
    ),

    shiny::fluidRow(shiny::column(10, "Same as above, but with 90 degrees added to p0. Optional, but required for phasing.")),
    shiny::fluidRow(
      shiny::column(10,shiny::textInput(ns("nmr_im_text"), label=NULL, placeholder = "Path to Imaginary 2D NMR txt file", width='100%')),
      shiny::column(2,jms.classes::fileChooserUI(ns('nmr_im'),label="Select File",title='Please choose the Imaginary 2D NMR txt file',multiple=FALSE,style="width:100%; margin-top: 0px;"))
    ),

    shiny::fluidRow(shiny::column(10, "Path to the acquisition directory. Optional. TODO dirChoose!")),
    shiny::fluidRow(
      shiny::column(10,shiny::textInput(ns("nmr_acqu_text"), label=NULL, placeholder = "Path to acquisition directory", width='100%')),
      shiny::column(2,jms.classes::fileChooserUI(ns('nmr_acqu'),label="Select File",title='Please choose the acquisition directory',multiple=FALSE,style="width:100%; margin-top: 0px;"))
    ),

    shiny::fluidRow(shiny::column(10, "Path to the ATMC log file. Optional.")),
    shiny::fluidRow(
      shiny::column(10,shiny::textInput(ns("nmr_atmc_text"), label=NULL, placeholder = "Path to ATMC log file", width='100%')),
      shiny::column(2,jms.classes::fileChooserUI(ns('nmr_atmc'),label="Select File",title='Please choose the ATMC log file',multiple=FALSE,style="width:100%; margin-top: 0px;"))
    ),

    shiny::fluidRow(shiny::column(10, "Path to the echem file.")),
    shiny::fluidRow(
      shiny::column(10,shiny::textInput(ns("echem_text"), label=NULL, placeholder = "Path to echem file", width='100%')),
      shiny::column(2,jms.classes::fileChooserUI(ns('echem'),label="Select File",title='Please choose the echem file',multiple=FALSE,style="width:100%; margin-top: 0px;"))
    ),

    shiny::fluidRow(
      shiny::column(12,
                    shiny::numericInput(ns('max_scans'), 'Number of scans performed (0 = all)', 0),
                    shiny::numericInput(ns('offset_hours'), 'Hours per scan (if no ATMC log)', 0)
      )
    ),

    shiny::fluidRow(shiny::column(12,
                                  shiny::actionButton(ns("import"), "Import"),
                                  shiny::actionButton(ns("save"), "Save to .GlobalEnv"),
                                  shiny::actionButton(ns("copy_script"), "Copy import script")
    )),

    shiny::hr(),

    shiny::fluidRow(
      shiny::column(4, shiny::plotOutput(ns('echem_plot'))),
      shiny::column(4, shiny::plotOutput(ns('nmr_plot'))),
      shiny::column(4, shiny::verbatimTextOutput(ns('acqu_text')))
    )
  )
}


interactive_import_mod <- function(input, output, session) {

  nmr_real_file <- callModule(jms.classes::fileChooser, "nmr_real",state=function() TRUE,filetypes=c('txt'))
  nmr_im_file <- callModule(jms.classes::fileChooser, "nmr_im",state=function() TRUE,filetypes=c('txt'))
  nmr_acqu_dir <- callModule(jms.classes::fileChooser, "nmr_acqu",state=function() TRUE,filetypes=c())
  nmr_atmc_file <- callModule(jms.classes::fileChooser, "nmr_atmc",state=function() TRUE,filetypes=c('txt'))
  echem_file <- callModule(jms.classes::fileChooser, "echem",state=function() TRUE,filetypes=Echem.Data::supported_file_extensions())

  shiny::observe({shiny::updateTextInput(session, 'nmr_real_text', value=nmr_real_file())})
  shiny::observe({shiny::updateTextInput(session, 'nmr_im_text', value=nmr_im_file())})
  shiny::observe({shiny::updateTextInput(session, 'nmr_acqu_text', value=nmr_acqu_dir())})
  shiny::observe({shiny::updateTextInput(session, 'nmr_atmc_text', value=nmr_atmc_file())})
  shiny::observe({shiny::updateTextInput(session, 'echem_text', value=echem_file())})

  imported_data_list <- shiny::reactiveValues(data=nmr2dinsitu.data.object(), data_name = 'insitu_data', script_input='')

  observeEvent(input$save, {assign('insitu_nmr_data', imported_data_list$data, envir=.GlobalEnv)})

  observeEvent(input$copy_script, {jms.classes::clipboard_copy(imported_data_list$script_input)})

  observeEvent(input$import, {
    withProgress(message = 'Importing data', value = 1, {
      im_file = if(input$nmr_im_text != '') input$nmr_im_text else NA
      aq_dir = if(input$nmr_acqu_text != '') input$nmr_acqu_text else NA

      data = read.nmr(input$nmr_real_text, imaginary_file = im_file, acqus = aq_dir)

      if(input$max_scans > 0) {
        data = reduceScans(data, c(1, input$max_scans))
      }

      if(input$nmr_atmc_text != '') {
        offsets = read.ATMC(input$nmr_atmc_text)
        data = storeOffsets(data, offsets)
      } else if(input$offset_hours > 0) {
        offsets = noATMoffsets(data, input$offset_hours)
        data = storeOffsets(data, offsets)
      }

      if(input$echem_text != '') {
        echem = Echem.Data::read.echem(input$echem_text)
        data = associate_echem_with_nmr(data, echem)
      }

      imported_data_list$data <- data

      imported_data_list$nmr_paths = c(real=input$nmr_real_text,
                                       imaginary=input$nmr_im_text,
                                       acqu=input$nmr_acqu_text,
                                       atmc=input$nmr_atmc_text)
      imported_data_list$echem_path = input$echem_text


      script = paste0("insitu_nmr_data = read.nmr(\n'", input$nmr_real_text, "'")
      if(input$nmr_im_text != '') {
        script = paste0(script, ",\nimaginary_file = '", input$nmr_im_text, "'")
      }

      if(input$nmr_acqu_text != '') {
        script = paste0(script, ",\nacqus = '", input$nmr_acqu_text, "'")
      }

      script = paste0(script, '\n)\n')

      #TODO: how to handle no atm offsets??
      if(input$nmr_atmc_text != '') {
        script = paste0(script, "offsets = read.ATMC('", input$nmr_atmc_text ,"')\n")
      } else {
        off = if(input$offset_hours > 0) input$offset_hours else 1
        script = paste0(script, 'offsets = noATMoffsets(insitu_nmr_data, ', off, ')\n')
      }

      script = paste0(script, 'insitu_nmr_data = storeOffsets(insitu_nmr_data, offsets)\n')

      script = paste0(script, 'insitu_nmr_data = reduceScans(insitu_nmr_data, c(1, ', ncol(imported_data_list$data)-1, '))\n')

      if(input$echem_text != '') {
        script = paste0(script, "echem_data = read.echem(\n'", input$echem_text, "'\n)\n")
        script = paste0(script, "insitu_nmr_data = associate_echem_with_nmr(insitu_nmr_data , echem_data)\n")
      }

      imported_data_list$script_input = script
    })
  })

  imported_data <- shiny::reactive({shiny::reactiveValuesToList(imported_data_list)})

  output$echem_plot <- shiny::renderPlot({
    nmr = imported_data_list$data
    echem = attr(nmr, 'echem')
    if(!is.null(echem)) plot(echem)
  })

  output$nmr_plot <- shiny::renderPlot({
    nmr = imported_data_list$data
    if(!length(nmr) == 0) plot(nmr[,1:2])
  })

  output$acqu_text <- shiny::renderText({
    nmr = imported_data_list$data
    output = ""
    atts = attributes(nmr)
    att_names = names(atts)

    attribute_list = list(
      nuc1='Nucleus',
      pulprog='Pulse program',
      ns='ns',
      date='date'
    )

    for(k in names(attribute_list)) {
      if(k %in% att_names) {
        output = sprintf("%s\n%s = %s\n", output, attribute_list[[k]], atts[[k]])
      }
    }

    echem = attr(nmr, 'echem')
    atts = attributes(echem)
    att_names = names(atts)

    attribute_list = list(
      Type='Material',
      Crate='C-rate',
      Capacity='Capacity',
      Cycles='Cycles',
      OCV='OCV',
      `Characteristic Mass`='Characteristic Mass',
      Cycler='Cycler',
      channel='Cycler channel'
    )

    for(k in names(attribute_list)) {
      if(k %in% att_names) {
        output = sprintf("%s\n%s = %s\n", output, attribute_list[[k]], atts[[k]])
      }
    }

    return(output)
  })


  return(imported_data)
}

#' Creates a GUI for importing data
#'
#' @export
interactiveImport <- function() {
  if(!requireNamespace("shiny", quietly=TRUE)) stop('Interactive importing requires the shiny package to be installed')
  if(!requireNamespace("shinyFiles", quietly=TRUE)) stop('Interactive importing requires the shinyFiles package to be installed')
  if(!requireNamespace("shinyBS", quietly=TRUE)) stop('Interactive importing requires the shinyBS package to be installed')
  shiny::addResourcePath('www', system.file('www', package='jms.classes'))

  server <- function(input, output, session) {
    shiny::callModule(interactive_import_mod, "import")
  }

  ui <- interactive_import_mod_UI("import")
  shiny::shinyApp(ui, server)
}
