interactive_import_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(jms.classes::jsCodeHandler(),
    style="padding:50px 50px;",
    NotificationStyle(),
    shiny::fluidRow(
      shiny::column(10, "Path to the acquisition directory (i.e. the directory containing acqus, pdata, ...)")
    ),
    shiny::fluidRow(
      shiny::column(10, shiny::textInput(ns("nmr_acqu_text"),
        label=NULL,
        placeholder="Path to acquisition directory", width="100%"
      )),
      shiny::column(2, jms.classes::dirChooserUI(
        ns("nmr_acqu"),
        label="Select Directory",
        title="Please choose the acquisition directory",
        multiple=FALSE,
        style="width:100%; margin-top: 0px;"
      ))
    ),

    shiny::fluidRow(shiny::column(10, "Path to the ATMC log file. Optional.
                                  Used to determine the time each scan took place.")),
    shiny::fluidRow(
      shiny::column(10, shiny::textInput(ns("nmr_atmc_text"),
        label=NULL,
        placeholder="Path to ATMC log file", width="100%"
      )),
      shiny::column(2, jms.classes::fileChooserUI(
        ns("nmr_atmc"),
        label="Select File",
        title="Please choose the ATMC log file",
        multiple=FALSE,
        style="width:100%; margin-top: 0px;"
      ))
    ),

    shiny::uiOutput(ns("echemFileUI")),

    shiny::fluidRow(
      shiny::column(
        6,
        shiny::fluidRow(
          style="padding-top: 20px;",
          shiny::column(
            6,
            shiny::textInput(ns("max_scans"), "Number of last scan performed",
              placeholder="Leave blank to determine automatically"
            )
          ),
          shiny::column(
            6,
            shiny::textInput(ns("offset_hours"), "Hours per scan (if no ATMC log)")
          )
        ),

        shiny::fluidRow(
          style="padding-top: 20px;",
          shiny::column(
            12,
            shiny::div("You typically want the data range for processing to be
                        slightly larger than what you will use in the final plot,
                        if there is a much larger range than will be plotted,
                        you should reduce it here to simplify processing.",
              style="text-align: justify; width: calc(50% + 300px); max-width: 100%;"
            )
          )
        ),

        shiny::fluidRow(
          style="padding-top: 10px;",
          shiny::column(
            6,
            shiny::numericInput(ns("xlim2"), "Upper ppm limit (leave blank for default)", NA)
          ),
          shiny::column(
            6,
            shiny::numericInput(ns("xlim1"), "Lower ppm limit (leave blank for default)", NA)
          )
        ),
        shiny::uiOutput(ns("echemProcessUI"))
      ),
      shiny::fluidRow(
        style="padding-top: 20px;", align="center",
        shiny::column(3, shiny::plotOutput(ns("nmr_plot"), height="250px")),
        shiny::column(
          3,
          shiny::plotOutput(ns("echem_plot"), height="250px"),
          shiny::uiOutput(ns("capacitytimeUI"))
        )
      )
    ),
    shiny::fluidRow(
      align="center", style="padding: 20px;",
      shiny::column(
        12,
        shiny::actionButton(ns("import"), "Import", style="background: #77dd77;"),
        shiny::actionButton(ns("save"), "Save to .GlobalEnv"),
        shiny::actionButton(ns("copy_script"), "Copy import script")
      )
    ),
    shiny::hr(),
    data_info_mod_UI(ns("info"))
  )
}

interactive_import_mod <- function(input, output, session,
                                   data_name=function() {"insitu_nmr_data"}, ...) {
  nmr_acqu_dir <- callModule(jms.classes::dirChooser, "nmr_acqu", state=function() TRUE)
  nmr_atmc_file <- callModule(jms.classes::fileChooser, "nmr_atmc",
    state=function() TRUE, filetypes=c("txt")
  )

  shiny::observe({
    shiny::updateTextInput(session, "nmr_acqu_text", value=nmr_acqu_dir())
  })
  shiny::observe({
    shiny::updateTextInput(session, "nmr_atmc_text", value=nmr_atmc_file())
  })

  if (requireNamespace("Echem.Data", quietly=TRUE)) {
    echem_file <- callModule(jms.classes::fileChooser, "echem",
      state=function() TRUE,
      filetypes=Echem.Data::supported_file_extensions()
    )
    shiny::observe({
      shiny::updateTextInput(session, "echem_text", value=echem_file())
    })
    output$echemFileUI <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(shiny::column(10, "Path to the echem file.")),
        shiny::fluidRow(
          shiny::column(10, shiny::textInput(session$ns("echem_text"),
            label=NULL,
            placeholder="Path to echem file", width="100%"
          )),
          shiny::column(2, jms.classes::fileChooserUI(
            session$ns("echem"),
            label="Select File",
            title="Please choose the echem file",
            multiple=FALSE,
            style="width:100%; margin-top: 0px;"
          ))
        )
      )
    })
  }

  shiny::observe({
    if (input$nmr_atmc_text != "") {
      jms.classes:::disableInput("offset_hours", session)
    } else {
      shiny::updateTextInput(session, "offset_hours", placeholder="")
      jms.classes:::enableInput("offset_hours", session)
    }
  })

  output$capacitytimeUI <- shiny::renderUI({
    if (requireNamespace("Echem.Process", quietly=TRUE)) {
      jms.classes::toggleInput(session$ns("capacitytime"), "", FALSE, "Cap", "Time",
        style="margin-top: -25px; margin-left: 0.75cm;"
      )
    }
  })

  output$echemProcessUI <- shiny::renderUI({
    if (is.null(input$capacitytime) || !input$capacitytime) {
      return()
    }
    if (requireNamespace("Echem.Database", quietly=TRUE) &&
      (input$echem_text == "" || !suppressWarnings(is.na(as.numeric(input$echem_text))))) {
      return()
    }
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::div("Enter the mass in order to normalise the capacity. The theoretical
                     capacity for the material is only used for the calculation of some
                     additional metadata.",
            style="text-align: justify; width: calc(50% + 300px); max-width: 100%;"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(session$ns("mass"), "Characteristic Mass (mg)", 1, min=0)),
        shiny::column(
          6,
          shiny::numericInput(
            session$ns("tcap"),
            htmltools::HTML("Capacity of Material (mAhg<sup>-1</sup>)"), 1, 0
          )
        )
      )
    )
  })

  imported_data_list <- shiny::reactiveValues(
    data=nmr2dinsitu.data.object(),
    nmr_paths=c(),
    echem_path=NA,
    script_input="",
    packages=c(),
    lower=NA,
    upper=NA,
    max_scans=NA,
    offset_hours=NA,
    mass=NA,
    tcap=NA,
    capacitytime=FALSE
  )

  shiny::observeEvent(input$save, {
    assign(data_name(), imported_data_list$data, envir=.GlobalEnv)
  })

  shiny::observeEvent(input$copy_script, {
    jms.classes::clipboard_copy(imported_data_list$script_input)
  })

  shiny::observeEvent(input$import, {
    do_import(
      nmr_file=input$nmr_acqu_text,
      atmc_file=input$nmr_atmc_text,
      echem_file=input$echem_text,
      max_scans=input$max_scans,
      lower=input$xlim1,
      upper=input$xlim2,
      offset_hours=input$offset_hours,
      mass=input$mass,
      tcap=input$tcap,
      capacitytime=input$capacitytime
    )
  })

  do_import <- function(nmr_file, atmc_file, echem_file, max_scans, lower, upper, offset_hours, mass, tcap, capacitytime) {
    shiny::withProgress(message="Importing data", value=1, {
      script <- "# The following code was automatically generated using the interactiveImport command\n"

      if (!grepl("/[0-9]*/?$", nmr_file)) {
        shiny::showNotification(
          shiny::div(
            htmltools::HTML("<b>Warning: path does not appear to be an acquisition directory</b><br/>
                            Acquisition directories should end with a number. Attempting to continue anyway...")
          ),
          duration=10,
          type="warning", id="error", session=session
        )
      }

      shiny::withProgress(message="Importing NMR", value=1, {
        data <- tryCatch({
          read.nmr(nmr_file, imaginary=TRUE)
        }, error=function(e) {
          shiny::showNotification(shiny::div(htmltools::HTML("<b>Error: Could not load data!</b><br/>", e$message)),
            duration=10,
            type="error", id="error", session=session
          )
          return()
        })
        if (is.null(data)) {
          return()
        }

        imported_data_list$packages <- c("NMR.Utils")

        script <- paste0(script, data_name(), " = read.nmr('", nmr_file, "', imaginary=TRUE)\n")

        if (!is.na(as.numeric(max_scans)) && as.numeric(max_scans) > 0) {
          last_scan <- as.numeric(max_scans)
          shiny::updateTextInput(session, "max_scans", placeholder="Leave blank to determine automatically")
        } else {
          last_scan <- find_last_scan(data)
          shiny::updateTextInput(session, "max_scans", placeholder=sprintf("Using %s as last scan", last_scan))
        }
        data <- reduceScans(data, c(1, last_scan))

        if (!is.nmr2d.data.object(data)) {
          shiny::showNotification(shiny::div(htmltools::HTML("<b>Error: Data is not 2D!</b><br/>")),
            duration=10,
            type="error", id="error", session=session
          )
          return()
        }

        script <- sprintf("%1$s%2$s = reduceScans(%2$s, c(1, %3$s))\n", script, data_name(), last_scan)

        xl1 <- !is.null(lower) && !is.na(lower)
        xl2 <- !is.null(upper) && !is.na(upper)

        if (xl1 && xl2) {
          if (lower > upper) {
            shiny::showNotification(
              shiny::div(
                htmltools::HTML("<b>Warning: ppm limits are invalid!</b><br/>
                                Switching upper and lower limits and attempting to continue...")
              ),
              duration=10,
              type="warning", id="error", session=session
            )
            temp <- lower
            lower <- upper
            upper <- temp
          }

          data <- data[data$ppm > as.numeric(lower) & data$ppm < as.numeric(upper), ]
          script <- sprintf(
            "%1$s%2$s = %2$s[%2$s$ppm > %3$s & %2$s$ppm < %4$s,]\n",
            script, data_name(), as.numeric(lower), as.numeric(upper)
          )
        } else if (xl1) {
          data <- data[data$ppm > as.numeric(lower), ]
          script <- sprintf(
            "%1$s%2$s = %2$s[%2$s$ppm > %3$s,]\n",
            script, data_name(), as.numeric(lower)
          )
        } else if (xl2) {
          data <- data[data$ppm < as.numeric(upper), ]
          script <- sprintf(
            "%1$s%2$s = %2$s[%2$s$ppm < %3$s,]\n",
            script, data_name(), as.numeric(upper)
          )
        }

        if (atmc_file != "") {
          offsets <- read.ATMC(atmc_file)

          if (length(offsets) < ncol(data) - 1) {
            shiny::showNotification(
              shiny::div(
                htmltools::HTML("<b>Warning: ATMC log file does not contain enough times</b><br/>")
              ),
              duration=10,
              type="warning", id="error", session=session
            )
          }

          data <- storeOffsets(data, offsets)
          script <- paste0(script, "offsets = read.ATMC('", atmc_file, "')\n")
          script <- sprintf("%1$s%2$s = storeOffsets(%2$s, offsets)\n", script, data_name())

          shiny::updateTextInput(session, "offset_hours",
            placeholder=sprintf("Average (ATMC) = %s hours", round(mean(diff(offsets)), 2))
          )
        } else if (!is.na(as.numeric(offset_hours)) && as.numeric(offset_hours) > 0) {
          offsets <- noATMoffsets(data, as.numeric(offset_hours))
          data <- storeOffsets(data, offsets)
          script <- sprintf("%1$soffsets = noATMoffsets(%2$s, %3$s)\n", script, data_name(), offset_hours)
          script <- sprintf("%1$s%2$s = storeOffsets(%2$s, offsets)\n", script, data_name())

          shiny::updateTextInput(session, "offset_hours", placeholder="")
        } else {
          shiny::updateTextInput(session, "offset_hours", placeholder="Using default of 1 hour")
        }
      })

      if (is.null(data) || nrow(data) == 0) {
        shiny::showNotification(shiny::div(htmltools::HTML("<b>Error: No NMR Data!</b><br/>")),
          duration=10,
          type="error", id="error", session=session
        )
        return()
      }

      if (requireNamespace("Echem.Data", quietly=TRUE) && echem_file != "") {
        shiny::withProgress(message="Importing echem", value=1, {
          tryCatch({
            if (!file.exists(echem_file) && requireNamespace("Echem.Database", quietly=TRUE) &&
              !suppressWarnings(is.na(as.numeric(echem_file)))) {
              # A number was entered and the database is available, try to fetch the echem from the database
              echem <- Echem.Database::process.echem(as.integer(echem_file))
              script <- paste0(script, "echem_data = Echem.Database::process.echem(", as.integer(echem_file), ")\n")
            } else {
              echem <- Echem.Data::read.echem(echem_file)
              script <- paste0(script, "echem_data = read.echem('", echem_file, "')\n")

              if (!is.null(mass) && !is.na(mass) && !is.na(tcap)) {
                shiny::withProgress(message="Processing Echem", value=1, {
                  echem <- Echem.Process::process(echem, mass, tcap)
                  script <- paste0(script, "echem_data = Echem.Process::process(echem_data, ", mass, ", ", tcap, ")\n")
                })
              }
            }

            if (!is.null(capacitytime) && capacitytime) {
              jms.classes::xcol(echem) <- "CumulativeCapacity.Ah."
              jms.classes::xlab(echem) <- expression("Cumulative Capacity / mAhg"^-1)
              jms.classes::xscale(echem) <- 1000

              script <- paste0(script, "xcol(echem_data) <- 'CumulativeCapacity.Ah.'\nxlab(echem_data) <- expression('Cumulative Capacity / mAhg'^-1)\nxscale(echem_data) <- 1000\n")
            } else {
              jms.classes::xcol(echem) <- "Test_Time.s."
              jms.classes::xlab(echem) <- "Time / h"
              jms.classes::xscale(echem) <- 1 / 3600

              script <- paste0(script, "xcol(echem_data) <- 'Test_Time.s.'\nxlab(echem_data) <- 'Time / h'\nxscale(echem_data) <- 1/3600\n")
            }
            data <- associate_echem_with_nmr(data, echem)
            script <- sprintf("%1$s%2$s = associate_echem_with_nmr(%2$s, echem_data)\n", script, data_name())
            imported_data_list$packages <- c("NMR.Utils", "Echem.Data")
          }, error=function(e) {
            shiny::showNotification(shiny::div(htmltools::HTML("<b>Error: Could not load echem data!</b><br/>", e$message)),
              duration=10,
              type="error", id="error", session=session
            )
            return()
          })
        })
      }
      imported_data_list$data <- data
      imported_data_list$nmr_paths <- c(
        acqu=input$nmr_acqu_text,
        atmc=input$nmr_atmc_text
      )
      imported_data_list$echem_path <- echem_file

      session$userData$echem_path <- echem_file # For debug log

      imported_data_list$script_input <- script
      imported_data_list$lower <- lower
      imported_data_list$upper <- upper
      imported_data_list$max_scans <- max_scans
      imported_data_list$offset_hours <- offset_hours
      imported_data_list$mass <- mass
      imported_data_list$tcap <- tcap
      imported_data_list$capacitytime <- capacitytime
    })
  }



  margins <- c(0.7, 0.6, 0.05, 0.1)

  labelledPlot <- function(label) {
    oma.saved <- par("oma")
    par(oma=rep.int(0, 4))
    par(oma=oma.saved)
    o.par <- par(mai=margins)
    on.exit(par(o.par))
    plot.new()
    box()
    text(0.5, 0.5, label)
  }

  output$echem_plot <- shiny::renderPlot({
    nmr <- imported_data_list$data
    echem <- attr(nmr, "echem")
    if (is.null(echem)) {
      labelledPlot("No echem data imported")
    } else {
      par(mai=margins)

      class(echem) <- c("echem.data.object", "jms.data.object", "data.frame")

      scale <- jms.classes::xscale(echem)
      plot(echem)

      if (ncol(nmr) > 1) {
        # Add a dot at the time of each scan
        time <- colnames(nmr)[-1]
        time <- as.numeric(time)

        args <- list(echem)
        args[[colnames(echem)[[jms.classes::xcol(echem)]]]] <- time / scale
        v <- as.numeric(do.call(Plotting.Utils::nearest, args)[, jms.classes::ycol(echem)])

        w <- floor(seq(from=1, to=(ncol(nmr) - 1), length.out=10))
        points(time[-w], v[-w], col=rgb(0.25, 0.41, 0.88, 0.5), pch=16, cex=1)

        maxTime <- max(echem[, jms.classes::xcol(echem)]) * scale
        outOfRange <- time[time > maxTime]

        # Highlight 10 evenly spaced scans
        time <- time[w]
        v <- v[w]
        points(time, v, col=rgb(1, 0.27, 0, 0.5), pch=16, cex=2, xpd=NA)

        # Add labels offset by 0.25 inches above the points (or to the right if above falls outside the plot)
        v <- v + yinch() * 0.25
        m <- max(echem)
        gt <- v > m
        v[gt] <- v[gt] - yinch() * 0.25
        time[gt] <- time[gt] + xinch() * 0.25

        # Draw the labels with a translucent white background to make them easier to see if the fall on top of data
        Plotting.Utils::boxed.labels(time, v, w,
          col=rgb(1, 0.27, 0, 1),
          bg=rgb(1, 1, 1, 0.5), border=F, xpd=NA
        )

        if (length(outOfRange) > 0) {
          Plotting.Utils::boxed.labels(
            maxTime - xinch() * 0.1,
            m - yinch() * 0.1,
            sprintf("%s scans out of range! \u2192", length(outOfRange)),
            col=rgb(1, 0.27, 0, 1), bg=rgb(1, 1, 1, 0.5), border=F, xpd=F, adj=1, xpad=1.05
          )
        }
      }
    }
  })

  output$nmr_plot <- shiny::renderPlot({
    nmr <- imported_data_list$data
    if (!length(nmr) == 0 && nrow(nmr) > 0 && ncol(nmr) > 1) {
      par(mai=margins)
      plot(nmr[, 1:2])
      legend("topright", sprintf("#1 / %s", ncol(nmr) - 1), bty="n")
    } else {
      labelledPlot("No NMR data imported")
    }
  })

  shiny::callModule(data_info_mod, "info", shiny::reactive({
    imported_data_list$data
  }))

  data_filename <- shiny::reactive({
    path <- imported_data_list$nmr_paths[[1]]
    if (is.null(path)) {
      return()
    }
    if (dir.exists(path)) {
      # Assume bruker
      expt <- basename(dirname(path))
      exptNo <- basename(path)
      return(paste0(expt, "-", exptNo))
    } else {
      return(basename(path))
    }
  })


  #### State saving ####

  # Save extra values in state$values when we bookmark
  shiny::onBookmark(function(state) {
    state$values$nmr_paths <- imported_data_list$nmr_paths
    state$values$script_input <- imported_data_list$script_input
    state$values$echem_path <- imported_data_list$echem_path
    state$values$lower <- imported_data_list$lower
    state$values$upper <- imported_data_list$upper
    state$values$max_scans <- imported_data_list$max_scans
    state$values$offset_hours <- imported_data_list$offset_hours
    state$values$mass <- imported_data_list$mass
    state$values$tcap <- imported_data_list$tcap
    state$values$capacitytime <- imported_data_list$capacitytime
  })

  # Read values from state$values when we restore
  shiny::onRestored(function(state) {
    if (length(state$values$nmr_paths) > 0) {
      jms.classes::log.debug("Reimporting data")
      do_import(
        nmr_file=state$values$nmr_paths[[1]],
        atmc_file=state$values$nmr_paths[[2]],
        echem_file=state$values$echem_path,
        max_scans=state$values$max_scans,
        lower=state$values$lower,
        upper=state$values$upper,
        offset_hours=state$values$offset_hours,
        mass=state$values$mass,
        tcap=state$values$tcap,
        capacitytime=state$values$capacitytime
      )
    }
  })

  # Exclude from bookmarking
  shiny::setBookmarkExclude(c("nmr_acqu", "nmr_atmc", "import", "save", "copy_script", "echem"))

  #### Return ####


  list(
    data=shiny::reactive({
      imported_data_list$data
    }),
    data_filename=data_filename,
    script=shiny::reactive({
      imported_data_list$script_input
    }),
    packages=shiny::reactive({
      imported_data_list$packages
    }),
    action="Importing"
  )
}

#' @export
#' @rdname insitu_gui
interactiveImport <- function() {
  jms.classes::assert_packages("shiny", "shinyFiles", "DT", purpose="Interactive importing")

  shiny::addResourcePath("www", system.file("www", package="jms.classes"))

  server <- function(input, output, session) {
    shiny::callModule(interactive_import_mod, "import")
  }

  ui <- interactive_import_mod_UI("import")
  shiny::shinyApp(ui, server)
}
