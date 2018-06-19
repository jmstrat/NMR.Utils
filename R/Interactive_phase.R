
#' @export
interactivePhase <- function(data,xrange) {
  if(missing(xrange)) xrange=rev(range(data[,1]))
  shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(shiny::sliderInput("scan", "scan", 1,ncol(data)-1,1),
                          shiny::sliderInput("p0", "p0", -1000, 1000, 0),
                          shiny::sliderInput("p1", "p1", -1000, 1000, 0),
                          shiny::sliderInput("pivot", "pivot", xrange[[2]], xrange[[1]], mean(xrange)),
                          shiny::sliderInput("yzoom", "yzoom", 1, 10, 1,step=0.1)),
      shiny::mainPanel(shiny::plotOutput("spectrum"))
    )
  ),
  server = function(input, output) {
    output$spectrum <- shiny::renderPlot({
      phased=phase(data[,c(1,input$scan+1)],input$p0,input$p1,input$pivot)
      phased=makeReal(phased)
      yr=c(min(phased[,2]),max(phased[,2]/input$yzoom))
      plot(phased,type='l',xlim=xrange,xaxs='i',yaxs='i',yaxt='n',ylab='Intensity',ylim=yr)
      abline(v=input$pivot, col='red')
      }
    )
  }
)
}
