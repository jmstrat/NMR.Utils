patchedClickablePlot_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("plotUI"))
}

defaultOverlay <- function() {
  Plotting.Utils::new_plot(c(0, 1), c(0, 1))
}

patchedClickablePlot <- function(input, output, session, plotRenderer, width, height, overlayFunc=defaultOverlay) {
  output$overlay <- shiny::renderPlot({
    par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0))
    overlayFunc()
  }, bg="transparent")

  output$plotUI <- shiny::renderUI({
    shiny::div(
      style=sprintf("position:relative; width: %scm; height: %scm", width(), height()),
      shiny::div(
        style="position:absolute; top: 0; left:0;",
        shiny::plotOutput(session$ns("plot"), width=paste0(width(), "cm"), height=paste0(height(), "cm"))
      ),
      shiny::div(
        style="position:absolute; top: 0; left:0; z-index: 10;",
        shiny::plotOutput(session$ns("overlay"),
          width=paste0(width(), "cm"), height=paste0(height(), "cm"),
          click=clickOpts(id=session$ns("click"), clip=F),
          dblclick=clickOpts(id=session$ns("dblclick"), clip=F),
          hover=hoverOpts(id=session$ns("hover"), delay=50, clip=FALSE, delayType="throttle")
        )
      )
    )
  })

  output$plot <- plotRenderer

  shiny::setBookmarkExclude(c("click", "dblclick", "hover"))

  list(
    click=shiny::reactive({
      cl <- input$click
      if (is.null(cl)) {
        return()
      }
      list(
        x=(cl$x - cl$domain$left) / (cl$domain$right - cl$domain$left),
        y=(cl$y - cl$domain$bottom) / (cl$domain$top - cl$domain$bottom)
      )
    }),
    dblclick=shiny::reactive({
      cl <- input$dblclick
      if (is.null(cl)) {
        return()
      }
      list(
        x=(cl$x - cl$domain$left) / (cl$domain$right - cl$domain$left),
        y=(cl$y - cl$domain$bottom) / (cl$domain$top - cl$domain$bottom)
      )
    }),
    hover=shiny::reactive({
      cl <- input$hover
      if (is.null(cl)) {
        return()
      }
      list(
        x=(cl$x - cl$domain$left) / (cl$domain$right - cl$domain$left),
        y=(cl$y - cl$domain$bottom) / (cl$domain$top - cl$domain$bottom)
      )
    })
  )
}
