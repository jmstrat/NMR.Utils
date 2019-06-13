interactive_plotting_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    jms.classes::jsCodeHandler(),
    NotificationStyle(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h3("Plotting Options"),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(ns("ps"), "Point size (pt)", 12, step=1, width="100%")
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns("layoutButtonUI"))
          )
        ),
        # TODO -- if these change force a custom layout:
        shiny::div("These options may not work unless a custom layout is specified.", style="margin-bottom: 10px;"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(ns("cex"), "Text magnification", 1, step=0.1, width="100%")
          ),
          shiny::column(
            6,
            shiny::numericInput(ns("cexaxis"), "Axis label magnification", 1, step=0.1, width="100%")
          )
        ),
        shiny::br(),
        shiny::tabsetPanel(
          id=ns("plotOptionsTabs"),
          shiny::tabPanel(
            "Insitu Plot",
            shiny::br(),
            insitu_plot_mod_UI(ns("insituPlot"))
          )
        )
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          align="center",
          shiny::h3("Plot Preview"),
          patchedClickablePlot_UI(ns("clickable"))
        ),
        shiny::hr(),
        shiny::h3("Plot Annotations"),
        annotator_mod_UI(ns("annotator")),
        shiny::div(
          style="margin-top:20px; margin-bottom: 10px; text-align: justify;",
          "The preview displayed before adding may look different under some circumstances."
        ),
        shiny::fluidRow(
          shiny::column(2, jms.classes::toggleInput(ns("anoPreview"), "Preview", TRUE, "On", "Off")),
          shiny::column(2, jms.classes::toggleInput(ns("anoCoords"), "Coordinates", FALSE, "On", "Off"))
        ),
        shiny::hr(),
        shiny::fluidRow(
          align="center",
          jms.classes::help_button(ns("helpButton")),
          shiny::actionButton(ns("copy_r"), "Copy plotting parameters as R input"),
          shiny::downloadButton(ns("export_png"), "Export image as a PNG")
        ),
        shiny::hr(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title="Scan Table",
            shiny::fluidRow(align="center", shiny::column(12, DT::dataTableOutput(ns("scanTbl"))))
          ),
          shiny::tabPanel(
            title="Info",
            data_info_mod_UI(ns("info"))
          )
        )
      )
    )
  )
}


interactive_plotting_help <- function() {
  shiny::modalDialog(
    style="text-align: justify;",
    title="Help",
    size="l",
    easyClose=TRUE,
    shiny::p("The following steps describe a sensible approach to navigate the plotting
             interface:"),
    shiny::tags$ol(
      shiny::tags$li("The default plot size is 16 × 10 cm. Change this by clicking the
                     \"Customise plot layout\" button at the top of the options list.
                     You may also add additional sub plots here."),
      shiny::tags$li("Set the ppm axis range as desired (typically slightly smaller than
                     the processing range)."),
      shiny::tags$li("Adjust the offset between scans. This can be considered akin to the
                     \"angle\" the plot is viewed at: 0 is equivalent to viewing along
                     the time axis, ∞ to viewing down the intensity axis. Typically this
                     depends on the number of scans, with more scans needing a larger offset."),
      shiny::tags$li("Adjust the upper y limit as desired."),
      shiny::tags$li("Select a colouring option, choices are:"),
      shiny::tags$ul(
        shiny::tags$li("Intensity based colouring. This is the defualt choice and generally
                       looks good for most data."),
        shiny::tags$ol(
          shiny::tags$li("Restrict colouring to be within the x-range of data that is
                         changing as a function of time by changing the left and right
                         axis limits."),
          shiny::tags$li("Adjust the intensity scale to cover the y-range of data that
                         is changing using the upper and lower intensity limits."),
          shiny::tags$li("The out of bounds colour corresponds to intensities that fall
                         outside this range"),
          shiny::tags$li("The default line colour corresponds to points that fall outside
                         the left and right axis limits.")
        ),
        shiny::tags$li("Shading. This fills in the gap between a scan and its baseline,
                       adding a 3D effect when looking down on the data from above.
                       Typically only looks good if the plot offset is high."),
        shiny::tags$ol(
          shiny::tags$li('Turn off "Colour data based on intensity",'),
          shiny::tags$li('Turn on "Shade under data".'),
          shiny::tags$li("Change the lower intensity limit to adjust the baseline for shading."),
          shiny::tags$li("Set the shade colour and default line colour as desired.")
        ),
        shiny::tags$li("Monochromatic"),
        shiny::tags$ol(
          shiny::tags$li('Turn off "Colour data based on intensity".'),
          shiny::tags$li("Set the default line colour as desired.")
        )
      ),
      shiny::tags$li("Set appropriate options for any additional subplots added."),
      shiny::tags$li("Change additional axis parameters if desired (e.g. tick intervals)."),
      shiny::tags$li("Adjust the margins to ensure all axis labels are visible with minimal
                     whitespace at the edges of the image, and the separation between plots
                     looks good.")
    ),
    shiny::p("There are other options not described here that can also be adjusted if desired.
             Once the plot looks good, export the R script and run it to produce the plot.")
  )
}

interactive_plotting_mod <- function(input, output, session, data, data_name, visible=function() TRUE, ...) {
  plotSize <- reactiveValues(width=16, height=10)

  shiny::callModule(data_info_mod, "info", data)
  ##### WARNING DIALOGS ####
  observe({
    if (!visible()) {
      return()
    }
    if (nrow(data()) == 0) {
      shiny::showModal(shiny::modalDialog(
        title="No data!",
        "Please import some data and try again.",
        size="l"
      ))
      return()
    }
  })
  #########

  echem_data <- shiny::reactive({
    data <- attr(data(), "echem")
    if (is.null(data)) {
      return()
    }
    x <- jms.classes::xcol(data)
    y <- jms.classes::ycol(data)
    jms.classes::xcol(data) <- y
    jms.classes::ycol(data) <- x

    xs <- jms.classes::xscale(data)
    ys <- jms.classes::yscale(data)
    jms.classes::xscale(data) <- ys
    jms.classes::yscale(data) <- xs

    data
  })

  hasEchem <- shiny::reactive({
    !is.null(echem_data())
  })

  ############################
  ############ UI ############
  ############################
  customPlots <- shiny::reactiveValues()
  ncustomtabs <- 0
  shiny::observeEvent(customLayout$nPlots, {
    n <- customLayout$nPlots - 2 # NMR & Echem
    d <- n - ncustomtabs

    if (d == 0) {
      return()
    } else if (d > 0) {
      # Add tabs

      for (i in 1:d) {
        id <- paste0("extraPlot", ncustomtabs + 1)
        tab <- shiny::tabPanel(
          paste0("Plot #", ncustomtabs + 3),
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::br(),
              extra_plot_mod_UI(session$ns(id))
            )
          )
        )
        if (!id %in% names(customPlots)) {
          customPlots[[id]] <- shiny::callModule(extra_plot_mod, id, data, data_name)
        }
        shiny::appendTab("plotOptionsTabs", tab)
        ncustomtabs <<- ncustomtabs + 1
      }
    } else {
      d <- d * -1
      for (i in 1:d) {
        name <- paste0("Plot #", ncustomtabs + 2)
        shiny::removeTab("plotOptionsTabs", name)
        ncustomtabs <<- ncustomtabs - 1
      }
    }
  })

  shiny::observeEvent(input$helpButton, {
    shiny::showModal(interactive_plotting_help())
  })

  output$layoutButtonUI <- shiny::renderUI({
    if (!hasEchem()) {
      return()
    }
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::tags$label(`for`=session$ns("useCustomLayout"), htmltools::HTML("&nbsp")), # Empty label for alignment
        shiny::actionButton(session$ns("useCustomLayout"), "Customise plot layout", width="100%")
      )
    )
  })


  display_cols <- 6
  output$scanTbl <- DT::renderDataTable({
    if (!hasEchem()) {
      return()
    }
    scans <- 1:(ncol(data()) - 1)
    yaxs <- signif(as.numeric(colnames(data())[-1]), 3)
    df <- data.frame(x=scans, y=yaxs)
    n <- nrow(df)
    r <- n %% display_cols
    if (r > 0) {
      df[(n + 1):(n + display_cols - (r)), ] <- NA
    }

    chunks <- split(df, (seq(nrow(df)) - 1) %/% ceiling(nrow(df) / display_cols))
    df <- do.call(cbind, chunks)
    colnames(df) <- rep.int(
      c(
        "Scan",
        htmltools::HTML(
          jms.classes:::expressionToHTML(xlab(echem_data()))
        )
      ),
      display_cols
    )
    table <- DT::datatable(
      df,
      class="compact hover",
      escape=FALSE,
      rownames=FALSE,
      options=list(
        paging=FALSE,
        ordering=FALSE,
        searching=FALSE,
        info=FALSE,
        columnDefs=list(list(className="dt-center", targets="_all"))
      )
    )
    table <- DT::formatStyle(table, 1, `border-left`="1px solid black")
    DT::formatStyle(table, seq(2, display_cols * 2, 2), `border-right`="1px solid black")
  })

  ############################
  ####### TOGGLEABLE UI ######
  ############################

  customLayout <- reactiveValues(matrix=NULL, nPlots=NULL, input=NULL)

  shiny::observeEvent(input$useCustomLayout, {
    shiny::showModal(
      shiny::modalDialog(
        size="l",
        shiny::div(
          style="min-height: 600px;",
          shiny::h3("Customise plot size"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                session$ns("plotWidth"),
                "Width of plot (cm)",
                plotSize$width, 5,
                step=1,
                width="100%"
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                session$ns("plotHeight"),
                "Height of plot (cm)",
                plotSize$height, 5,
                step=1,
                width="100%"
              )
            )
          ),
          shiny::h3("Customise plot layout"),
          shiny::p("It is currently expected that the echem plot is the same
                    height as the NMR plot and to its right.",
            style="text-align: justify;"
          ),
          shiny::p("Additional plots may be added, they will initially be shown
                    as placeholders (with 0 margins) in the plot behind this
                    window. You can customise these plots using the tabs at the
                    top of the options list.", style="text-align: justify;"),
          shiny::p("This tool does not support overlapping plots (e.g. insets).
                    Any such layouts will need to be manually defined in the
                    plotting script.", style="text-align: justify;"),
          shiny::uiOutput(session$ns("layoutChooserUI"))
        ),
        footer=shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("modalok"), "OK")
        )
      )
    )
  })

  hasRendered <- FALSE
  output$layoutChooserUI <- shiny::renderUI({
    if (is.null(customLayout$input)) {
      default_layout <- list(
        c(0, 0, 3, 1),
        c(3, 0, 1, 1)
      )
    } else {
      default_layout <- customLayout$input
    }

    # It seems the first time we display the input (in a modal) its layout is incorrect
    # Crude hack to "fix" it by redrawing after a second
    if (!hasRendered) {
      hasRendered <<- TRUE
      shiny::invalidateLater(1000, session)
    }
    jms.classes::layoutInput(session$ns("layout"), default_layout, c("NMR", "Echem"), T, 2)
  })


  shiny::observeEvent(input$modalok, {
    data <- input$layout
    customLayout$matrix <- data$matrix
    customLayout$nPlots <- data$nPlots
    customLayout$input <- data$input

    if (!is.null(input$plotWidth)) {
      plotSize$width <- input$plotWidth
    }
    if (!is.null(input$plotHeight)) {
      plotSize$height <- input$plotHeight
    }
    shiny::removeModal()
  })

  usingCustomLayout <- shiny::reactive({
    !is.null(customLayout$matrix)
  })

  insituPlot <- shiny::callModule(
    insitu_plot_mod, "insituPlot",
    data, data_name,
    shiny::reactive({
      customLayout$matrix
    }), visible
  )

  ############################
  ######## ANNOTATIONS #######
  ############################

  np <- function() {
    n <- customLayout$nPlots
    if (is.null(n)) {
      if (hasEchem()) {
        n <- 2
      } else {
        n <- 1
      }
    }
    n
  }
  pn <- function() {
    n <- 1:np()
    n[[1]] <- "NMR"
    if (np() > 1) {
      n[[2]] <- "Echem"
    }
    n
  }
  pm <- function() {
    m <- customLayout$matrix
    if (is.null(m)) {
      if (hasEchem()) {
        return(matrix(c(1, 1, 1, 2), 1, 4))
      } else {
        return(matrix(1))
      }
    }
    return(m)
  }

  opar <- NULL

  outputplot <- shiny::renderPlot({
    doPlot(final=FALSE)
    annotator$updatendc()
    opar <<- par(c(
      "cex", "cex.axis", "cex.lab", "cex.main", "cex.sub", "col", "col.axis",
      "col.lab", "col.main", "col.sub", "crt", "family", "fg", "font",
      "font.axis", "font.lab", "font.main", "font.sub", "lend", "lheight",
      "ljoin", "lmitre", "lty", "lwd", "mex", "pch", "ps", "pty", "srt"
    ))
  })

  overlay <- shiny::reactive({
    gui_try_show_error({
      par(opar)
      if (!is.null(input$anoPreview) && input$anoPreview) {
        annotator$preview()
      }
      if (!is.null(input$anoCoords) && input$anoCoords) {
        xy <- annotator$ndccoordinates()
        x <- grconvertX(xy[[1]], from="ndc")
        y <- grconvertY(xy[[2]], from="ndc")
        points(x, y, pch=4, col="red", cex=2)
      }
      annotator$focus()
    }, "drawing overlay")
  })

  clickers <- shiny::callModule(patchedClickablePlot, "clickable", outputplot,
    shiny::reactive({
      plotSize$width
    }), shiny::reactive({
      plotSize$height
    }),
    overlayFunc=overlay
  )

  annotator <- shiny::callModule(annotator_mod, "annotator",
    clickers$click, clickers$dblclick, clickers$hover,
    plotName=pn,
    nPlots=np,
    layoutMatrix=pm
  )

  ############################
  ########## PLOTTING ########
  ############################

  doPlot <- function(final=FALSE) {
    jms.classes::log.info("Drawing plot preview")
    shiny::withProgress(message="Preparing plot", value=1, {
      if (usingCustomLayout()) {
        jms.classes::log.debug("Using a custom layout")
        layout(customLayout$matrix)
      }
      # Set the global options
      par(
        ps=input$ps,
        cex=input$cex,
        cex.axis=input$cexaxis
      )
      jms.classes::log.debug("Drawing NMR + Echem")
      # draw the main plot
      set_nmr <- insituPlot$plotFunc(final=final)

      gui_try_show_error({
        # annotations for main plot
        if (hasEchem()) {
          annotator$annotateFunc(2, data=echem_data)
          if (!is.null(set_nmr)) {
            # i.e. we didn't error during the plot
            set_nmr()
            annotator$annotateFunc(1)
          }
        } else {
          annotator$annotateFunc(1)
        }
      }, "annotating insitu plot")

      if (usingCustomLayout()) {
        extraPlots <- customLayout$nPlots - 2
        if (extraPlots <= 0) {
          return()
        }

        placeholder <- function(i) {
          o.par <- par(mar=rep.int(0, 4))
          on.exit(par(o.par))
          plot.new()
          box()
          text(0.5, 0.5, sprintf("Plot #%s", i + 2))
        }

        cp <- names(customPlots)
        l <- length(cp)
        for (i in 1:extraPlots) {
          jms.classes::log.debug("Drawing additional plot %s / %s", i, extraPlots)
          if (i <= l) {
            plotMod <- (customPlots[[cp[[i]]]])()
            if ("plotFunc" %in% names(plotMod)) {
              gui_try_show_error({
                plotMod$plotFunc()
              }, paste0("drawing plot ", i + 2))

              gui_try_show_error({
                annotator$annotateFunc(i + 2)
              }, paste0("annotating plot ", i + 2))

              next
            }
          }
          jms.classes::log.debug("No plotting function found for plot %s / %s, drawing a placeholder instead", i, extraPlots)
          placeholder(i)
          annotator$annotateFunc(i + 2)
        }
      }
    })
    jms.classes::log.debug("Finished drawing all plots")
  }

  ############################
  ############################
  ############################


  script_input <- shiny::reactive({
    jms.classes::log.info("Compiling plotting script")
    script <- sprintf(
      "# The following code was automatically generated using the interactivePlotting command

# Output to a PNG file in the current working directory entitled Insitu_nmr_plot.png
# Warning: Changing the size of the plot will also affect the margins and labels
png('Insitu_nmr_plot.png', width=%s, height=%s, units='cm', res=300)",
      plotSize$width, plotSize$height
)

    if (!usingCustomLayout()) {
      if (hasEchem()) {
        script <- sprintf("%s

# To add more plots or otherwise change from a side=by-side layout you can use the ?layout function.
# Plot #1 will be used for NMR, #2 for echem. Higher numbered plots are then free to use for other purposes.
# You will need to pass use.default.layout=FALSE to the plot command below.
# See ?plot.nmr2dinsitu.data.object for more details.

# Set global font options
par(ps=%s, cex=%s, cex.axis=%s)", script, input$ps, input$cex, input$cexaxis)
      } else {
        script <- sprintf(
          "%s\n\n# Set global font options
par(ps=%s, cex=%s, cex.axis=%s)",
          script, input$ps, input$cex, input$cexaxis
        )
      }
    } else {
      script <- sprintf("%s\n\n# Adjust the plot layout
layout(%s)

# Set global font options
par(ps=%s, cex=%s, cex.axis=%s)

#### Insitu Plot ####", script, paste(deparse(customLayout$matrix), collapse=" "),
                        input$ps, input$cex, input$cexaxis)
    }

    script <- sprintf("%s\n\n%s\n\n", script, insituPlot$script())

    if (hasEchem()) {
      script <- sprintf(
        "%s# Echem Plot annotations\n\n%s\n\n# NMR Plot annotations\n\n# Activate the NMR plot\nset_nmr()\n\n%s",
        script, annotator$scriptFunc(2), annotator$scriptFunc(1)
      )
    } else {
      script <- sprintf(
        "%s# Annotations\n\n%s",
        script, annotator$scriptFunc(1)
      )
    }



    if (hasEchem() && !usingCustomLayout()) {
      script <- sprintf("%s\n\n# Perform any other plotting (if you are using a custom layout) here", script)
    }

    if (usingCustomLayout() && customLayout$nPlots > 2) {
      extraPlots <- customLayout$nPlots - 2
      extraPlotScripts <- list()

      cp <- names(customPlots)
      l <- length(cp)
      for (i in 1:extraPlots) {
        if (i <= l) {
          plotMod <- (customPlots[[cp[[i]]]])()
          if ("script" %in% names(plotMod)) {
            extraPlotScripts[[i]] <- sprintf(
              "#### Plot %s ####\n\n%s\n\n# Annotations\n\n%s\n",
              i + 2, plotMod$script(), annotator$scriptFunc(i + 2)
            )
            next
          }
        }
        extraPlotScripts[[i]] <- sprintf("#### Plot %s ####

# Add commands to draw plot here\n", i + 2)
      }

      extraPlotScript <- paste0(extraPlotScripts, collapse="\n")

      script <- sprintf("%s\n\n%s", script, extraPlotScript)
    }

    sprintf("%s

# Finish the plot and save the PNG file
dev.off()", script)
  })

  ############################
  ############################
  ############################

  shiny::observeEvent(input$copy_r, {
    jms.classes::clipboard_copy(script_input())
  })

  output$export_png <- shiny::downloadHandler(
    filename="Insitu_nmr_plot.png",
    content=function(file) {
      png(file, width=plotSize$width, height=plotSize$height, units="cm", res=300)
      doPlot(final=TRUE)
      dev.off()
    },
    contentType="image/png"
  )

  #### State saving ####

  # Save extra values in state$values when we bookmark
  shiny::onBookmark(function(state) {
    state$values$width <- plotSize$width
    state$values$height <- plotSize$height
    state$values$matrix <- customLayout$matrix
    state$values$nPlots <- customLayout$nPlots
    state$values$input <- customLayout$input
  })

  # Read values from state$values when we restore
  shiny::onRestore(function(state) {
    jms.classes::log.debug("Restoring plot layout")
    plotSize$width <- state$values$width
    plotSize$height <- state$values$height
    customLayout$matrix <- state$values$matrix
    customLayout$nPlots <- state$values$nPlots
    customLayout$input <- state$values$input
  })

  # Exclude from bookmarking
  shiny::setBookmarkExclude(c(
    "helpButton", "copy_r", "export_png",
    "useCustomLayout", "modalok", "plotOptionsTabs"
  ))

  #### Return ####
  list(
    script=script_input,
    packages=shiny::reactive({
      c("NMR.Utils")
    })
  )
}


#' @export
#' @rdname insitu_gui
interactivePlotting <- function(nmr) {
  if (missing(nmr) || is.null(nmr)) {
    stop("No data to plot! The nmr argument is required.

Use insitu_gui() if you wish to import and process data interactively.")
  }

  jms.classes::assert_packages("shiny", "DT", "colourpicker", purpose="Interactive plotting")

  data_name <- deparse(substitute(nmr))

  server <- function(input, output, session) {
    shiny::callModule(interactive_plotting_mod, "plot", shiny::reactive({
      nmr
    }), shiny::reactive({
      data_name
    }))
  }

  ui <- interactive_plotting_mod_UI("plot")
  shiny::shinyApp(ui, server)
}
