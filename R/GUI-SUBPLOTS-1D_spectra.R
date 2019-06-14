oneD_spectra_plot_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Plot one or more scans from the 2D dataset on a separate plot."),
    shiny::uiOutput(ns("selectizeUI")),
    shiny::selectizeInput(
      ns("colours"), "Plot colours",
      multiple=TRUE, choices=colours()
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::uiOutput(ns("xlim1UI"))
      ),
      shiny::column(
        6,
        shiny::uiOutput(ns("xlim2UI"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::uiOutput(ns("ylim1UI"))
      ),
      shiny::column(
        6,
        shiny::uiOutput(ns("ylim2UI"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(ns("offset"), "Offset between scans", 0, width="100%")
      ),
      shiny::column(
        6,
        shiny::checkboxInput(ns("showppm"), "Show ppm axis?", TRUE, width="100%")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          ns("marginB"),
          "Bottom margin (inches)",
          0.6, 0,
          step=0.05,
          width="100%"
        )
      ),
      shiny::column(
        6,
        shiny::numericInput(
          ns("marginT"),
          "Top margin (inches)",
          0.05, 0,
          step=0.05,
          width="100%"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          ns("marginL"),
          "Left margin (inches)",
          0.2, 0,
          step=0.05,
          width="100%"
        )
      ),
      shiny::column(
        6,
        shiny::numericInput(
          ns("marginR"),
          "Right margin (inches)",
          0.05, 0,
          step=0.05,
          width="100%"
        )
      )
    )
  )
}

oneD_spectra_plot_mod <- function(input, output, session, data, data_name) {
  rndDown <- function(x, adj=0) {
    l10 <- floor(log10(abs(x)))
    if (l10 < 0) l10 <- 0
    fac <- 10^(l10 + adj)
    floor(x / fac) * fac
  }

  rndUp <- function(x, adj=0) {
    fac <- 10^(floor(log10(abs(x))) + adj)
    ceiling(x / fac) * fac
  }

  xrange <- shiny::reactive({
    if (nrow(data()) == 0) {
      return(c(NA, NA))
    }
    r <- rev(range(as.numeric(data()[, 1])))
    # Give some nicer defaults
    c(rndUp(r[[1]]), rndDown(r[[2]]))
  })

  yrange <- shiny::reactive({
    if (nrow(data()) == 0) {
      return(c(NA, NA))
    }
    r <- range(data())
    # Give some nicer defaults
    c(rndDown(r[[1]]), rndUp(r[[2]]))
  })

  selected_plot_col_numbers <- shiny::reactive({
    selectedPlots <- input$selectize
    if (is.null(selectedPlots)) {
      return()
    }
    as.numeric(selectedPlots) + 1 # for x col
  })

  selected_data_with_offsets <- shiny::reactive({
    selectedPlots <- selected_plot_col_numbers()
    if (is.null(selectedPlots)) {
      return()
    }
    data()[, c(1, selectedPlots)] + input$offset * (1:length(selectedPlots) - 1)
  })


  # Render these server side so we can set appropriate defaults
  output$xlim1UI <- shiny::renderUI({
    shiny::numericInput(
      session$ns("xlim1"),
      "Left axis limit",
      xrange()[[1]], 0,
      step=0.05,
      width="100%"
    )
  })

  output$xlim2UI <- shiny::renderUI({
    shiny::numericInput(
      session$ns("xlim2"),
      "Right axis limit",
      xrange()[[2]], 0,
      step=0.05,
      width="100%"
    )
  })

  output$ylim1UI <- shiny::renderUI({
    shiny::numericInput(
      session$ns("ylim1"),
      "Lower y limit",
      yrange()[[1]], 0,
      step=0.05,
      width="100%"
    )
  })

  output$ylim2UI <- shiny::renderUI({
    shiny::numericInput(
      session$ns("ylim2"),
      "Upper y limit",
      yrange()[[2]], 0,
      step=0.05,
      width="100%"
    )
  })

  output$selectizeUI <- shiny::renderUI({
    shiny::selectizeInput(
      session$ns("selectize"),
      "Scans to plot",
      choices=1:(ncol(data()) - 1),
      multiple=TRUE
    )
  })


  plot_colours <- shiny::reactive({
    nmrdata <- selected_data_with_offsets()
    if (is.null(nmrdata) || length(nmrdata) == 0) {
      return()
    }
    n <- ncol(nmrdata) - 1
    if (n == 0) {
      return()
    }

    if (is.null(input$colours)) {
      cols <- "black"
    } else {
      cols <- input$colours
    }
    jms.classes::expand_args(1:n, cols)[[2]]
  })

  plotFunc <- function() {
    nmrdata <- selected_data_with_offsets()
    if (is.null(nmrdata) || length(nmrdata) == 0) {
      plot.new()
      return()
    }

    r <- c(input$ylim1, input$ylim2)
    n <- ncol(nmrdata) - 1

    par(mai=c(
      input$marginB,
      input$marginL,
      input$marginT,
      input$marginR
    ))

    cols <- plot_colours()

    if (input$showppm) {
      plot(
        nmrdata[, 1:2],
        ylim=r, xlim=c(input$xlim1, input$xlim2),
        col=cols[[1]], div=5
      )
    } else {
      plot(
        nmrdata[, 1:2],
        ylim=r, xlim=c(input$xlim1, input$xlim2),
        col=cols[[1]], axes=F
      )
    }


    if (n > 1) {
      for (i in 2:n) {
        lines(nmrdata[, c(1, i + 1)], col=cols[[i]])
      }
    }
  }

  # Ensure we never return null
  val <- function(x) {
    if (is.null(x)) {
      return(NA)
    }
    x
  }

  script <- shiny::reactive({
    columns <- selected_plot_col_numbers() - 1
    n <- length(columns)

    if (n == 0) {
      return("# No data was plotted here!
# Create a blank plot to advance the layout
plot.new()")
    }

    # Only include this section of code if we need to draw more than one scan
    laterScans <- ""
    if (n > 1) {
      laterScans <- sprintf("\n\n# Draw the remaining scans
for (i in 2:%s) {
   lines(nmrdata[, c(1, i+1)], col=cols[[i]])
}\n", n)
    }


    if (input$showppm) {
      # If we are showing a ppm axis, set the minor tick frequency to 5 * major
      axisArgument <- "div=5"
    } else {
      # Otherwise disable the axis drawing
      axisArgument <- "axes=FALSE"
    }


    sprintf(
      "# Set the subplot margins
par(mai=c(%s, %s, %s, %s))

# Get the data
selectedPlots = %s
nmrdata = %s[, c(1, selectedPlots + 1)] + %s * (1:length(selectedPlots) - 1)

# Choose the colours
cols = %s

# Draw the axes and first scan
plot(nmrdata[, 1:2], ylim=c(%s, %s), xlim=c(%s, %s), col=cols[[1]], %s)%s",

      val(input$marginB),
      val(input$marginL),
      val(input$marginT),
      val(input$marginR),
      paste(deparse(columns), collapse=" "),
      data_name(),
      val(input$offset),
      paste(deparse(plot_colours()), collapse=" "),
      val(input$ylim1),
      val(input$ylim2),
      val(input$xlim1),
      val(input$xlim2),
      axisArgument,
      laterScans
    )
  })

  #### State saving ####
  setBookmarkSuspendedOutput("xlim1UI", "xlim2UI", "ylim1UI", "ylim2UI", "selectizeUI")

  return(list(
    plotFunc=plotFunc,
    script=script
  ))
}

register_insitu_subplot_module("1D spectra", oneD_spectra_plot_mod_UI, oneD_spectra_plot_mod)
