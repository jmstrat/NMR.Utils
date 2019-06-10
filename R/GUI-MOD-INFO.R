data_info_mod_UI <- function(id) {
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(align='center',
                    shiny::h3('Selected Data Parameters')
    ),
    shiny::fluidRow(align='center',
                    shiny::column(4, shiny::h4('NMR Acquisition'), shiny::uiOutput(ns('nmr_attributes'))),
                    shiny::column(4, shiny::h4('NMR Processing'), shiny::uiOutput(ns('processing_attributes'))),
                    shiny::column(4, shiny::h4('Echem'), shiny::uiOutput(ns('echem_attributes')))
    )
  )
}


scrollable <- function(...) {
  shiny::pre(..., style="max-height: 200px; overflow-y: auto;")
}


data_info_mod <- function(input, output, session, data) {
  hasData <- shiny::reactive({
    return((!is.null(data())) && nrow(data())>0)
  })

  hasEchem <- shiny::reactive({
    !is.null(attr(data(),'echem'))
  })

  output$nmr_attributes <- shiny::renderUI({
    if(!hasData()) {
      return(shiny::fluidRow(align='center',shiny::column(12,shiny::h6("No NMR data available"))))
    }
    output = list()
    NMRdisplayAttributes = list(
      c('Acquisition Date', 'date'),
      c('Path', 'filepath'),
      c('Pulse Program','pulprog'),
      c('O1', 'o1'),
      c('O1 (ppm)','o1p'),
      c('Nucleus','nuc1'),
      c('sfo1', 'sfo1')
    )
    for(at in NMRdisplayAttributes) {
      atVal = attr(data(), at[[2]])
      if(!is.null(atVal)) {
        output = append(
          output,
          list(shiny::fluidRow(align='left',shiny::column(12,shiny::h6(at[[1]]))),scrollable(atVal))
        )
      }
    }
    output
  })

  output$processing_attributes <- shiny::renderUI({
    if(!hasData()) {
      return(shiny::fluidRow(align='center',shiny::column(12,shiny::h6("No NMR data available"))))
    }
    output = list()
    procDisplayAttributes = list(
      c('Processing Date', 'date'),
      c('Title','Title'),
      c('Line Broadening','lb'),
      c('Referenced Carrier Frequency (ppm)','car')
    )
    proc = attr(data(), 'proc')
    if(is.null(proc)) return()
    proc = proc$proc
    # These are stored as an attribute on data, but are calculated from processing params.
    # Just temporarily add them to the proc object for convinience
    proc[['Title']] <- attr(data(), 'Title')
    proc[['car']] <- attr(data(), 'Carrier Frequency ppm')
    for(at in procDisplayAttributes) {
      atVal = proc[[at[[2]]]]
      if(!is.null(atVal)) {
        output = append(
          output,
          list(shiny::fluidRow(align='left',shiny::column(12,shiny::h6(at[[1]]))),scrollable(atVal))
        )
      }
    }
    output
  })

  output$echem_attributes <- shiny::renderUI({
    if(!hasEchem()) {
      return(shiny::fluidRow(align='center',shiny::column(12,shiny::h6("No echem data available"))))
    }
    output = list()
    echemDisplayAttributes = list(
      c('Measurement Date', 'date'),
      c('File', 'filename'),
      c('Material', 'Type'),
      c('C-rate', 'Crate'),
      c('Capacity', 'Capacity'),
      c('OCV','OCV'),
      c('Characteristic Mass', 'Characteristic Mass'),
      c('Cycler','Cycler'),
      c('Channel', 'channel')
    )
    echem = attr(data(), 'echem')
    for(at in echemDisplayAttributes) {
      atVal = attr(echem, at[[2]])
      if(!is.null(atVal)) {
        output = append(
          output,
          list(shiny::fluidRow(align='left',shiny::column(12,shiny::h6(at[[1]]))),scrollable(atVal))
        )
      }
    }
    output
  })
}


