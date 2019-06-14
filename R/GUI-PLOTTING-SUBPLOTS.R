# TODO Modules can return data:
#      reactive passed to annotator and used to align annotations


registeredExtraInsituPlotTypes <- list()

#' Register an interface for drawing a subplot within \code{\link{interactivePlotting}}
#'
#' Note that this function is not persistent. You must re-register in every R session you wish to use this plot type.
#'
#' @param name Name of the plot type
#' @param uiFunction Shiny module UI function that takes an id parameter and returns a \code{\link[shiny]{tagList}}.
#' @param serverFunction Shiny module server function that takes \code{data} and \code{data_name} \code{\link[shiny]{reactive}} parameters and returns a \code{\link[base]{list}} with items: \cr
#' \describe{
#'  \item{plotFunc}{Function to draw the plot. Must be compatible with \code{\link{layout}}.}
#'  \item{script}{\code{\link[shiny]{reactive}} that returns a string containing the code to draw the plot}
#' }
#' @export
#' @seealso  \link{http://shiny.rstudio.com/articles/modules.htm}
register_insitu_subplot_module <- function(name, uiFunction, serverFunction) {
  registeredExtraInsituPlotTypes[[name]] <<- list(
    name=name,
    ui=uiFunction,
    server=serverFunction
  )
}

extra_plot_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("plotTypeSelectorUI")),
    shiny::hr(),
    shiny::uiOutput(ns("plotOptionsUI"))
  )
}

extra_plot_mod <- function(input, output, session, data, data_name) {
  plotTypes <- list()
  if (length(registeredExtraInsituPlotTypes)) {
    for (i in 1:length(registeredExtraInsituPlotTypes)) {
      plotType <- registeredExtraInsituPlotTypes[[i]]
      plotTypes[[plotType$name]] <- list(
        ui=plotType$ui,
        result=shiny::callModule(plotType$server, paste0("plotType", i), data, data_name)
      )
    }
  }

  output$plotTypeSelectorUI <- shiny::renderUI({
    if (length(plotTypes) == 0) {
      return(
        shiny::tagList(
          shiny::h3("No additional plot types have been registered."),
          shiny::span("See ?register_insitu_subplot_module")
        )
      )
    }
    selected <- NULL
    if ("Placeholder" %in% names(plotTypes)) {
      selected <- "Placeholder"
    }
    shiny::tagList(
      shiny::selectInput(
        session$ns("plotTypeSelector"),
        "Choose a type for this plot",
        names(plotTypes),
        selected=selected
      ),
      shiny::span("Add additional custom plot types to this list with the command
", shiny::code("register_insitu_subplot_module"))
    )
  })

  output$plotOptionsUI <- shiny::renderUI({
    if (is.null(input$plotTypeSelector)) {
      return()
    }
    w <- which(input$plotTypeSelector == names(plotTypes))
    (plotTypes[[w]])$ui(session$ns(paste0("plotType", w)))
  })

  selectedPlotParameters <- shiny::reactive({
    if (is.null(input$plotTypeSelector)) {
      return()
    }
    (plotTypes[[input$plotTypeSelector]])$result
  })

  #### State saving ####
  setBookmarkSuspendedOutput("plotTypeSelectorUI", "plotOptionsUI")

  return(selectedPlotParameters)
}
