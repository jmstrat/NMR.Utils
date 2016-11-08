
interactivePhase <- function(data,xrange) {
  load_or_install('shiny')
  shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(sliderInput("scan", "scan", 1,ncol(data)-1,1),sliderInput("p0", "p0", -1000, 1000, 0),sliderInput("p1", "p1", -1000, 1000, 0),sliderInput("pivot", "pivot", xrange[[2]], xrange[[1]], mean(xrange)),sliderInput("yzoom", "yzoom", 1,10,1,step=0.1)),
      mainPanel(plotOutput("spectrum"))
    )
  ), 
  server = function(input, output) {
    output$spectrum <- renderPlot({ 
      phased=phase(data[,c(1,input$scan+1)],input$p0,input$p1,input$pivot)
      phased=makeReal(phased)
      yr=c(min(phased[,2]),max(phased[,2]/input$yzoom))
      plot(phased,type='l',xlim=xrange,xaxs='i',yaxs='i',yaxt='n',ylab='Intensity',ylim=yr) }
    )
  }
)
}