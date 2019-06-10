insitu_plot_mod_UI <- function(id) {
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(htmltools::HTML(sprintf('#%s {margin-bottom: 20px;}', ns('insituTabs')))),
    shiny::tabsetPanel(
      type = "pills",
      id = ns('insituTabs'),

      shiny::tabPanel(
        title='Data',
        shiny::div(style='text-align: justify; display: inline-block; margin-bottom: 5px;', htmltools::HTML("<b>Undersample</b> – values above 0 reduce the number of data points to speed up plotting (automatically disabled when exporting).")),
        shiny::sliderInput(ns("undersample"), NULL, 0, 100 ,20),
        shiny::hr(),
        shiny::div(style='text-align: justify; margin-top: 10px; margin-bottom: 10px;', htmltools::HTML('<b>Rebin</b> - ensure a minimum separation between scans. Leave step blank to disable.')),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::div(style='display: block; margin-bottom: 5px; font-weight: 700;', "Minimum step"),
            shiny::div(style='display: inline-block; margin-bottom: 5px; font-size: 0.75em;', id=ns('rebinunit'), class="shiny-html-output"),
            shiny::numericInput(ns('rebinminstep'), NULL, NA)
          ),
          shiny::column(
            6,
            shiny::div(style='display: inline-block; margin-bottom: calc(1px + 1.75em); font-weight: 700;', "Method"),
            shiny::selectInput(ns('rebinmethod'), NULL, c('Discard','Average Before', 'Average After'), 'Discard')
          )
        )
      ),

      shiny::tabPanel(
        title='Axes',
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::div('Controls whether the y limits correspond to the y values at which the time axis begins / ends, or the y values at which the lowset and highest intensities in the dataset are found.',
                       style="text-align: justify; margin-bottom: 20px;")
          ),
          shiny::column(
            3,
            jms.classes::toggleInput(ns('ylimreltotime'), '', TRUE, 'Time', 'NMR')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(ns('ylim1'), 'Lower y limit (%)', 0, step=1, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(ns('ylim2'), 'Upper y limit (%)', 65, step=1, width='100%')
          )
        ),
        shiny::uiOutput(ns('offsetUI')),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput(ns('xlim1UI'))
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns('xlim2UI'))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput(ns('uppervrangeUI'))
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns('lowervrangeUI'))
          )
        ),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(ns('xaxismline'), 'Distance between x axes and labels', -0.5, step=0.1, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(ns('xaxislabelmline'), 'Distance between x axes and titles', 1.7, step=0.1, width='100%')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(ns('yaxismline'), 'Distance between y axis and labels', -0.5, step=0.1, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(ns('yaxislabelmline'), 'Distance between y axis and title', 1.7, step=0.1, width='100%')
          )
        ),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::checkboxInput(ns('ppm'), 'Show ppm axis labels?', TRUE, width='100%')
          ),
          shiny::column(
            6,
            shiny::checkboxInput(ns('tickdir'), 'Ticks point outwards', TRUE, width='100%')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(ns('upperTickLimit'), 'Left ppm limit for axis ticks', NA, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(ns('lowerTickLimit'), 'Right ppm limit for axis ticks', NA, width='100%')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(ns('upperLabelLimit'), 'Left ppm limit for axis labels', NA, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(ns('lowerLabelLimit'), 'Right ppm limit for axis labels', NA, width='100%')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(ns('forcedInterval'), 'ppm tick Interval', NA, width='100%')
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns('forcedVIntervalUI'))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::checkboxInput(ns('forcePrint'), 'Force printing of all axis labels', FALSE, width='100%')
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns('centreTimeUI'))
          )
        )
      ),

      shiny::tabPanel(
        title='Colouring',
        ### COLOUR ###
        shiny::checkboxInput(ns('colour'), 'Colour data based on intensity', TRUE, width='100%'),
        shiny::uiOutput(ns('colourUI')),
        restorableColourInput(ns('defaultcolour'), 'Default line colour', value='black'),
        shiny::numericInput(ns('lwd'), 'Line thickness', 0.5, 0, step=0.05, width='100%'),

        ### SHADE ###
        shiny::checkboxInput(ns('shade'), 'Shade under data?', FALSE, width='100%'),
        shiny::uiOutput(ns('shadeUI'))
      ),

      shiny::tabPanel(
        title='Margins',
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
            shiny::numericInput(ns('marginR'), 'Right margin (inches)', 0.5, 0, step=0.05, width='100%')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput(ns('separationUI'))
          )
        )
      ),

      shiny::tabPanel(
        title='Misc',
        shiny::checkboxInput(ns('ytrunc'), 'Truncate strong peaks?', FALSE, width='100%'),
        y_trunc_mod_UI(ns('ytruncUI'))
      )
    )
  )
}


insitu_plot_mod <- function(input, output, session, data, data_name, customLayoutMatrix, isVisible) {

  shiny::observeEvent(isVisible(), {
    # We need these to "preload" the tabs, but we don't want to preload before the plotting page
    # is visible otherwise we will slow everything down whilst the data are changing
    shiny::outputOptions(output, "rebinunit", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "xlim1UI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "xlim2UI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "offsetUI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "lowervrangeUI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "uppervrangeUI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "forcedVIntervalUI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "separationUI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "centreTimeUI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "colourUI", suspendWhenHidden = !isVisible())
    shiny::outputOptions(output, "shadeUI", suspendWhenHidden = !isVisible())
  })

  rndDown <- function(x, adj=0) {
    l10 = floor(log10(abs(x)))
    if(l10 < 0) l10=0
    fac = 10^(l10+adj)
    floor(x / fac) * fac
  }

  rndUp <- function(x, adj=0) {
    fac = 10^(floor(log10(abs(x)))+adj)
    ceiling(x / fac) * fac
  }

  xrange <- shiny::reactive({
    if(nrow(data()) == 0) return(c(NA, NA))
    r = rev(range(data()[,1]))

    #Give some nicer defaults
    oom = floor(log10(abs(r)))
    fac = 10^(max(oom) - 1)
    r2 <- c(ceiling(r[[1]] / fac) * fac, floor(r[[2]] / fac) * fac)

    if(abs(diff(r2)) < 1)
      r
    else
      r2
  })

  yrange_no_offsets <- shiny::reactive({
    shiny::withProgress(message = 'Loading data', value = 1, {
      if(nrow(data()) == 0) return(c(NA, NA))
      r = range(data())

      #Give some nicer defaults
      c(rndDown(r[[1]]), rndUp(r[[2]]))
    })
  })

  default_offset <- shiny::reactive({
    n = ncol(data())
    if(n < 2) return(NA)
    signif(max(data()[,c(2:n)])/n^1.5, 3)
  })

  yrange <- shiny::reactive({
    if(is.null(input$offset)) return()
    data = data()
    offsets=as.numeric(colnames(data)[-1]) * input$offset
    if(input$ylimreltotime) {
      # ylim2 corresponds to range of offsets
      r = range(offsets)

      upper = r[[2]] / (input$ylim2 / 100)
      lower = min(data) - (input$ylim1 / 100) * (upper - r[[1]])
    } else {
      # ylim1/2 are relative to entire range
      r = range(data + offsets)
      upper = r[[2]] / (input$ylim2 / 100)
      lower = r[[1]] - (input$ylim1 / 100) * (upper - r[[1]])
    }
    if(is.finite(lower) && is.finite(upper))
      c(lower, upper)
    else
      c(NA, NA)
  })

  echem_v_range <- shiny::reactive({
    echem=attr(data(),'echem')
    if(is.null(echem)) {
      return(c(NA,NA))
    }
    r = range(echem)
    #Give some nicer defaults
    c(rndDown(r[[1]], -1), rndUp(r[[2]], -1))
  })

  hasEchem <- shiny::reactive({
    !is.null(attr(data(),'echem'))
  })

  ############################
  ############ UI ############
  ############################

  output$rebinunit <- shiny::renderUI({
    echem <- attr(data(),'echem')
    if(is.null(echem)) {
      unit='Time / h'
    } else {
      unit=jms.classes:::expressionToHTML(jms.classes::xlab(echem))
    }
    htmltools::HTML(unit)
  })

  output$xlim1UI <- shiny::renderUI({
    shiny::numericInput(session$ns('xlim1'), 'Left ppm limit', xrange()[[1]], 0, step=0.05, width='100%')
  })

  output$xlim2UI <- shiny::renderUI({
    shiny::numericInput(session$ns('xlim2'), 'Right ppm limit', xrange()[[2]], 0, step=0.05, width='100%')
  })

  output$offsetUI <- shiny::renderUI({
    shiny::numericInput(session$ns('offset'), 'Offset between scans', default_offset(), 0, step=0.05, width='100%')
  })

  output$lowervrangeUI <- shiny::renderUI({
    if(!hasEchem()) return()
    shiny::numericInput(session$ns('lowervrange'), 'Right voltage limit', echem_v_range()[[1]], 0, step=0.1, width='100%')
  })

  output$uppervrangeUI <- shiny::renderUI({
    if(!hasEchem()) return()
    shiny::numericInput(session$ns('uppervrange'), 'Left voltage limit', echem_v_range()[[2]], 0, step=0.1, width='100%')
  })

  output$forcedVIntervalUI <- shiny::renderUI({
    if(!hasEchem()) return()
    shiny::numericInput(session$ns('forcedVInterval'), 'Voltage tick Interval', NA, width='100%')
  })

  output$separationUI <- shiny::renderUI({
    if(!hasEchem()) return()
    shiny::numericInput(session$ns('separation'), 'Separation between echem and NMR (inches)', 0.1, 0, step=0.01, width='100%')
  })

  output$centreTimeUI <- shiny::renderUI({
    if(!hasEchem()) return()
    shiny::checkboxInput(session$ns('centreTime'), 'Centre time axis title to ticks', TRUE, width='100%')
  })

  output$colourUI <- shiny::renderUI({
    if(input$colour) {
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(session$ns('colourxlim1'), 'Left ppm limit for colouring', xrange()[[1]], 0, step=0.05, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(session$ns('colourxlim2'), 'Right ppm limit for colouring', xrange()[[2]], 0, step=0.05, width='100%')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(session$ns('colourylim1'), 'Lower intensity limit for colouring', yrange_no_offsets()[[1]], 0, step=0.05, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(session$ns('colourylim2'), 'Upper intensity limit for colouring', yrange_no_offsets()[[2]], 0, step=0.05, width='100%')
          )
        ),
        restorableColourInput(session$ns('nacolour'), 'Out of bounds colour', value='black')
      )
    }
  })

  output$shadeUI <- shiny::renderUI({
    if(input$shade){
      shiny::fluidRow(
        shiny::column(
          6,
          restorableColourInput(session$ns('shadecolour'), 'Shade colour', value='#bebebe')
        ),
        shiny::column(
          6,
          shiny::numericInput(session$ns('shadeMin'), 'Lower intensity limit for shading', yrange_no_offsets()[[1]], -Inf, width='100%')
        )
      )
    }
  })

  ytruncArgs <- shiny::callModule(y_trunc_mod, "ytruncUI", shiny::reactive({input$ytrunc}), yrange)

  ############################
  ############################
  ############################

  usingCustomLayout <- shiny::reactive({
    !is.null(customLayoutMatrix())
  })

  undersampledData <- shiny::reactive({
    if(is.null(input$undersample)) return(data())
    undersample(data(), input$undersample)
  })

  rebinMethod <- shiny::reactive({
    methods <- c(
      `Discard`='discard',
      `Average Before`='average_before',
      `Average After`='average_after'
    )
    methods[[input$rebinmethod]]
  })

  rebinnedUndersampledData <- shiny::reactive({
    plotData = undersampledData()
    if(!is.null(input$rebinminstep) && !is.na(input$rebinminstep)) {
      return(rebin(plotData, input$rebinminstep, method=rebinMethod()))
    }
    return(plotData)
  })

  doPlot <- function(final=FALSE) {
    plotData = NULL

    gui_try_show_error({
      if(final) {
        plotData = data()
        if(!is.null(input$rebinminstep) && !is.na(input$rebinminstep)) {
          plotData = rebin(plotData, input$rebinminstep, method=rebinMethod())
        }
      } else {
        plotData = rebinnedUndersampledData()
      }
    }, 'preparing data for plot')

    if(is.null(plotData)) {
      return()
    }
    if(nrow(plotData) == 0 || ncol(plotData) == 0) {
      jms.classes::log.error('No data available: rows: %s, cols: %s', nrow(plotData), ncol(plotData))
      shiny::showNotification(shiny::div(htmltools::HTML('<b>Error: No data to plot!</b>')), duration = 10,
                              type = "error", id='error')
      stop("No data to plot!")
    }
    if(is.null(yrange())) stop("No data to plot!")

    par(mai=c(
      input$marginB,
      input$marginL,
      input$marginT,
      input$marginR
    ))

    echemAxes = c(1,4)
    sepPos = c(4, 2)
    if(usingCustomLayout()) {
      echemCol = which(customLayoutMatrix() == 2, T)
      nmrCol = which(customLayoutMatrix() == 1, T)
      if(length(echemCol) && length(nmrCol) && echemCol[[1,2]] < nmrCol[[1,2]]) {
        echemAxes = c(1,2)
        sepPos = c(2,4)
      }
    }

    xlim <- c(input$xlim1, input$xlim2)
    if(any(is.null(xlim))) {
      shiny::showNotification(shiny::div(htmltools::HTML("<b>Error: No x-range</b><br/>(This shouldn't happen)")), duration = 10,
                              type = "error", id='error', session = session)
    }

    plotArgs = list(
      plotData,

      xlim=c(input$xlim1, input$xlim2),
      ylim=yrange(),
      V_range=c(input$lowervrange, input$uppervrange),

      plot_offset=input$offset,
      plot.colour=input$colour,

      plot.colour.ranges=list(c(input$colourxlim1, input$colourxlim2)),
      plot.colour.yranges=list(c(input$colourylim1, input$colourylim2)),

      col_na=input$nacolour,
      colour_scheme=c('blue','green','yellow','magenta','red'),
      colour.legend=FALSE, colour.legend.show.zero=TRUE,
      damp_colour_scaling_percent = NA,
      col=input$defaultcolour,
      lwd=input$lwd,
      shade_under=input$shade,
      shade_col=input$shadecolour,
      shade_min_intensity = input$shadeMin,
      show_ppm_label=input$ppm,
      ticksOut=input$tickdir,
      lowerTickLimit = input$lowerTickLimit,
      lowerLabelLimit = input$lowerLabelLimit,
      upperTickLimit = input$upperTickLimit,
      upperLabelLimit = input$upperLabelLimit,
      forcedInterval = input$forcedInterval,
      forcedVInterval = input$forcedVInterval,
      forcePrint = input$forcePrint,

      xaxismline = input$xaxismline,
      xaxislabelmline = input$xaxislabelmline,
      yaxismline = input$yaxismline,
      yaxislabelmline = input$yaxislabelmline,
      centreTimeTitle = input$centreTime,
      echemAxes = echemAxes,
      separation.pos = sepPos,
      separation=input$separation
    )

    ytruncArgs = ytruncArgs$plotArgs()
    for(arg in names(ytruncArgs)) {
      plotArgs[[arg]] = ytruncArgs[[arg]]
    }

    if(!hasEchem()) {
      plotArgs$V_range <-NULL
      plotArgs$forcedVInterval <-NULL
      plotArgs$separation <-NULL
      plotArgs$centreTimeTitle <- NULL
      plotArgs$yaxismline <- NULL
      plotArgs$yaxislabelmline <- NULL
      plotArgs$echemAxes <- NULL
      plotArgs$separation.pos <- NULL
    }


    if(hasEchem() && usingCustomLayout()) {
      plotArgs[["use.default.layout"]] = FALSE
    }

    gui_try_show_error({
      suppressWarnings(do.call('plot', plotArgs))
    }, 'drawing insitu plot')
  }

  script_input <- shiny::reactive({
    val <- function(x) {
      if(is.null(x)) {
        return(NA)
      }
      x
    }

    col <- function(x) {
      x <- val(x)
      if(is.na(x)) {
        return(NA)
      }
      sprintf("'%s'", x)
    }

    additionalPlottingNote = "\n\n# It is possible to add anotations to the echem trace using the nearest function
# For example, to add points at 1, 2, and 3 hours to the echem profile uncomment the next 3 lines
# time = c(1, 2, 3)
# V = Plotting.Utils::nearest(echem_data, Test_Time.s.=time*3600)$Voltage.V
# points(V, time, col=c('red', 'blue', 'green'), pch=16, cex=2)"

    commentEchem = ''
    echemNote = ''
    customLayoutParam = ''
    rescaleFunc = 'set_nmr <- '
    if(!hasEchem()) {
      commentEchem = '# '
      echemNote = '\n# Some options are only available when echem data are available.
# Unavailable parameters have been commented out in the following command.\n'
      additionalPlottingNote = ''
      rescaleFunc = ''
    } else if (usingCustomLayout()) {
      customLayoutParam = ',\n  use.default.layout=FALSE'
    }

    rebinneddataname = val(data_name())
    rebin = ''
    if(!is.null(input$rebinminstep) && !is.na(input$rebinminstep)) {
      rebin = sprintf('\n# Rebin the data\nrebinned_data = rebin(%s, %s, method="%s")\n', rebinneddataname, input$rebinminstep, rebinMethod())
      rebinneddataname = 'rebinned_data'
    }


    echemSideParams = ''
    if(hasEchem() && usingCustomLayout()) {
      echemCol = which(customLayoutMatrix() == 2, T)
      nmrCol = which(customLayoutMatrix() == 1, T)
      if(length(echemCol) && length(nmrCol) && echemCol[[1,2]] < nmrCol[[1,2]]) {
        echemSideParams = ',\n  echemAxes=c(1, 2),\n  separation.pos=c(2, 4)'
      }
    }


    sprintf(
"# To undersample the data whilst testing, uncomment the next line (do not leave this enabled for the final plot)
# %s = undersample(%1$s, %s)
%s
# Set the plot margins
par(mai=c(%s, %s, %s, %s))
%s
# For a full description of these options, and some advanced options not included here
# see ?plot.nmr2d.data.object
%splot(
  %s,

  # Axis limits
  xlim=c(%s, %s),
  ylim=c(%s, %s),
  %sV_range=c(%s, %s),

  # Offset between adjacent scans
  plot_offset=%s,

  # Intensity colouring
  plot.colour=%s,
  plot.colour.ranges=list(c(%s, %s)),
  plot.colour.yranges=list(c(%s, %s)),
  col_na=%s, # Outside y-range
  colour_scheme=c('blue','green','yellow','magenta','red'),
  colour.legend=FALSE, colour.legend.show.zero=TRUE,
  damp_colour_scaling_percent=NA,

  # Colour of lines that are excluded from intensity colouring (outside x-range / colouring disabled)
  col=%s,
  # Thickness of lines
  lwd=%s,

  # Shading options
  shade_under=%s,
  shade_col=%s,
  shade_min_intensity=%s,

  %s,

  # Can disable this for more complex layouts
  show_ppm_label=%s,

  # Tick mark settings
  ticksOut=%s,
  lowerTickLimit=%s,
  lowerLabelLimit=%s,
  upperTickLimit=%s,
  upperLabelLimit=%s,
  forcedInterval=%s,
  %15$sforcedVInterval=%s,
  forcePrint=%s,
  xaxismline=%s,
  xaxislabelmline=%s,
  %15$syaxismline=%s,
  %15$syaxislabelmline=%s,
  %15$scentreTimeTitle=%s,

  # Distance between NMR and echem in inches
  %15$sseparation=%s%s%s
)%s",
      val(data_name()),
      val(input$undersample),
      rebin,
      val(input$marginB),
      val(input$marginL),
      val(input$marginT),
      val(input$marginR),
      echemNote,
      rescaleFunc,
      rebinneddataname,
      val(input$xlim1), val(input$xlim2),
      val(yrange()[[1]]), val(yrange()[[2]]),
      commentEchem,
      val(input$lowervrange), val(input$uppervrange),
      val(input$offset),
      val(input$colour),
      val(input$colourxlim1), val(input$colourxlim2),
      val(input$colourylim1), val(input$colourylim2),
      col(input$nacolour),
      col(input$defaultcolour),
      val(input$lwd),
      val(input$shade),
      col(input$shadecolour),
      val(input$shadeMin),

      ytruncArgs$scriptArgs(),

      val(input$ppm),
      val(input$tickdir),
      val(input$lowerTickLimit),
      val(input$lowerLabelLimit),
      val(input$upperTickLimit),
      val(input$upperLabelLimit),
      val(input$forcedInterval),
      val(input$forcedVInterval),
      val(input$forcePrint),
      val(input$xaxismline),
      val(input$xaxislabelmline),
      val(input$yaxismline),
      val(input$yaxislabelmline),
      val(input$centreTime),
      val(input$separation),
      customLayoutParam,
      echemSideParams,
      additionalPlottingNote
    )
  })


  return(list(
    plotFunc = doPlot,
    script = script_input
  ))
}
