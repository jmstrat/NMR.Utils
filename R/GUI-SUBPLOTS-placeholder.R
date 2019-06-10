placeholder_plot_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3('Placeholder plot.'),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(ns('marginB'), 'Bottom margin (inches)', 0.6, 0, step=0.05, width='100%')
      ),
      shiny::column(
        6,
        shiny::numericInput(ns('marginT'), 'Top margin (inches)', 0.05, 0, step=0.05, width='100%')
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(ns('marginL'), 'Left margin (inches)', 0.2, 0, step=0.05, width='100%')
      ),
      shiny::column(
        6,
        shiny::numericInput(ns('marginR'), 'Right margin (inches)', 0.05, 0, step=0.05, width='100%')
      )
    ),
    shiny::checkboxInput(ns('box'), 'Show Border?', TRUE),
    shiny::textInput(ns('label'), 'Label', 'Placeholder')
  )
}

placeholder_plot_mod <- function(input, output, session, data, data_name) {

  plotFunc <- function() {
    o.par <- par(mai=c(
      input$marginB,
      input$marginL,
      input$marginT,
      input$marginR
    ))
    Plotting.Utils::new_plot(c(0,1),c(0,1))
    if(!is.null(input$box) && input$box) box()
    text(0.5, 0.5, input$label)
  }

  script <- shiny::reactive({
    box=''
    if(input$box) {
      box='\n# Add a surrounding box\nbox()'
    }
    label=''
    if(input$label != '') {
      label=sprintf('\n# Add a label centred in the plot\ntext(0.5, 0.5, "%s")',input$label)
    }
    sprintf(
      '# Set the subplot margins
par(mai=c(%s, %s, %s, %s))
# Create a blank plot
plot.new()%s%s',
      input$marginB,
      input$marginL,
      input$marginT,
      input$marginR,
      box,
      label
    )
  })

  return(list(
    plotFunc = plotFunc,
    script = script
  ))
}

register_insitu_subplot_module('Placeholder', placeholder_plot_mod_UI, placeholder_plot_mod)
