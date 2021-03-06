interactive_phase_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    jms.classes::jsCodeHandler(),
    NotificationStyle(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("scan_slider")),
        shiny::uiOutput(ns("p0_slider")),
        shiny::uiOutput(ns("p1_slider")),
        shiny::uiOutput(ns("pivot_slider")),
        shiny::uiOutput(ns("xzoom_slider")),
        shiny::uiOutput(ns("zoom_slider")),
        shiny::fluidRow(
          shiny::column(10, shiny::uiOutput(ns("apk0_slider"))),
          shiny::column(2, shiny::actionButton(ns("apk0_button"), "Set", style="margin-top:35px;", disabled=TRUE))
        ),
        shiny::fluidRow(
          shiny::column(10, shiny::uiOutput(ns("apk1_slider"))),
          shiny::column(2, shiny::actionButton(ns("apk1_button"), "Set", style="margin-top:35px;", disabled=TRUE))
        ),
        shiny::includeCSS(system.file("www/pretty-checkbox.css", package="NMR.Utils")),
        shiny::tags$div(
          class="pretty p-switch p-fill", style="font-size: 30px; padding-top:20px;",
          shiny::tags$input(type="checkbox", id=ns("show_apk"), checked="checked"),
          shiny::tags$div(
            class="state p-primary",
            shiny::tags$label(
              shiny::div(
                "Show APK Ranges - P0 blue, P1 green",
                style="font-size: 15px; font-weight: bold; position: relative; top: -5px;"
              )
            )
          )
        ),
        shiny::tags$head(
          shiny::tags$style(
            shiny::HTML(".irs-from, .irs-to, .irs-min, .irs-max, .irs-single {visibility: hidden !important;}")
          )
        )
      ),
      shiny::mainPanel(
        shiny::h4(shiny::textOutput(ns("scan_title"))),
        shiny::fluidRow(align="center", shiny::div("Double click to set pivot. Drag to set P0 / P1 range.")),
        shiny::fluidRow(align="center", shiny::div("The P0 range should just cover the electrolyte peak; p1 the metal.
                                                   Pivot should be at the centre of the electrolyte peak.")),
        shiny::plotOutput(ns("spectrum"),
          height="600px",
          width="100%",
          dblclick=ns("dblclick"),
          brush=shiny::brushOpts(id=ns("brush"), direction="x", resetOnNew=TRUE)
        ),
        shiny::fluidRow(align="center", shiny::textOutput(ns("phase_params"))),
        shiny::hr(),
        shiny::fluidRow(
          align="center",
          shiny::actionButton(ns("do_apk"), "APK Single", style="background: #77dd77;"),
          shiny::actionButton(ns("do_apk_all"), "APK All", style="background: #77dd77;"),
          shiny::actionButton(ns("copy_r"), "Copy phase parameters as R input"),
          shiny::actionButton(ns("copy_tab"), "Copy phase parameters as table"),
          shiny::downloadButton(ns("export_csv"), "Export phase parameters as CSV")
        )
      )
    )
  )
}

interactive_phase_mod <- function(input, output, session, data, data_name, embedded=FALSE, visible=function() TRUE, ...) {

  ##### WARNING DIALOGS ####
  modal_shown <- list(shown=FALSE)

  is_complex <- shiny::reactive({
    any_complex(data())
  })

  observe({
    if (!visible()) {
      return()
    }
    if (length(data()) == 0) {
      shiny::showModal(shiny::modalDialog(
        title="No data to phase!",
        "Please import some data and try again.",
        size="l"
      ))
      return()
    }

    footer <- if (embedded) modalButton("Dismiss") else ""
    if (!is_complex() && !modal_shown$shown) {
      shiny::showModal(shiny::modalDialog(
        title="Data are not complex!",
        "Real data cannot be phased.",
        size="l",
        footer=footer
      ))
    }
    modal_shown$shown <- TRUE
  })

  shiny::observeEvent(data(), {
    modal_shown$shown <- FALSE
  })

  ##### RANGES ####

  xrange <- shiny::reactive({
    if (length(data()) == 0) {
      return(c(NA, NA))
    }
    rev(range(data()[, 1]))
  })
  yrange <- shiny::reactive({
    if (length(data()) == 0) {
      return(c(NA, NA))
    }
    grDevices::extendrange(range(makeReal(data())), f=0.05)
  })

  nscans <- shiny::reactive({
    n <- ncol(data())
    if (n > 1) n <- n - 1 # x column
    n
  })

  init_yrange <- shiny::reactive({
    if (length(data()) == 0) {
      return(c(NA, NA))
    }
    r <- grDevices::extendrange(range(Re(data()[, 2])), f=0.05)
    r[[2]] <- r[[2]] * 0.1
    r
  })

  init_apk_range <- function(initial) {
    if (length(data()) == 0) {
      return(list(range=c(NA, NA), max=NA))
    }
    x_total_range <- diff(xrange())
    apk_real_range <- xrange()[[2]] - (1 - initial) * x_total_range
    x <- data()[, 1]
    x_in_apk <- x < apk_real_range[[1]] & x > apk_real_range[[2]]
    peak_x <- x[x_in_apk][which.max(Re(data()[x_in_apk, 2]))]
    apk_real_range <- peak_x + x_total_range * 0.05 * c(-1, 1)
    list(
      range=(apk_real_range - xrange()[[2]]) / x_total_range + 1,
      max=peak_x
    )
  }


  init_apk0_range <- shiny::reactive({
    init_apk_range(c(0.5, 1))
  })
  init_apk1_range <- shiny::reactive({
    init_apk_range(c(0, 0.5))
  })

  ##### CURRENT PARAMETERS ####

  phased_parameters <- shiny::reactiveValues(preventReset=FALSE)

  current_scan <- shiny::reactive({
    scan <- input$scan
    if (is.null(scan)) {
      return(1)
    }
    return(scan)
  })

  phasing_matrix <- shiny::reactive({
    mat <- phased_parameters$phases
    mat[order(as.numeric(rownames(mat))), , drop=FALSE]
  })

  current_parameters <- shiny::reactive({
    mat <- phasing_matrix()
    if (is.null(mat)) {
      return(list())
    }
    rows_with_na <- sapply(1:nrow(mat), function(i) any(is.na(mat[i, ])))
    known_scans <- as.numeric(rownames(mat)[!rows_with_na])
    if (!current_scan() %in% known_scans && length(known_scans) > 0) {
      # Go to the nearset parameters if we don't have anything saved for the current scan
      nearest_known <- known_scans[[which.min(abs(known_scans - current_scan()))]]
      phased_parameters$phases[current_scan(), ] <- mat[nearest_known, ]
    }
    mat[current_scan(), ]
  })

  apk0 <- shiny::reactive({
    (1 - input$apk0) * diff(xrange()) * -1 + xrange()[[2]]
  })
  apk1 <- shiny::reactive({
    (1 - input$apk1) * diff(xrange()) * -1 + xrange()[[2]]
  })

  yzoom <- shiny::reactive({
    yr <- input$yzoom
    if (is.null(yr)) {
      return(init_yrange())
    }
    return(yr)
  })

  xzoom <- shiny::reactive({
    xr <- input$xzoom
    if (is.null(xr)) {
      return(xrange())
    }
    return(xr * -1)
  })

  # Reset phase params when data changes

  observeEvent(data(), {
    if (phased_parameters$preventReset) {
      phased_parameters$preventReset <- FALSE
      jms.classes::log.debug("Got new data: skipping reset of phase parameters")
      return()
    }
    jms.classes::log.debug("Got new data: resetting phase parameters")
    n <- nscans()
    if (n == 0) n <- 1
    mat <- matrix(nrow=n, ncol=3, dimnames=list(1:n, c("p0", "p1", "pivot")))
    mat[1, ] <- c(0, 0, init_apk0_range()[["max"]])
    phased_parameters$phases <- mat
  })

  # Update phase params from slider
  observeEvent(input$p0, {
    phased_parameters$phases[current_scan(), "p0"] <- input$p0
  })
  observeEvent(input$p1, {
    phased_parameters$phases[current_scan(), "p1"] <- input$p1
  })
  observeEvent(input$pivot, {
    phased_parameters$phases[current_scan(), "pivot"] <- input$pivot
  })

  # Update phase params from plot
  shiny::observeEvent(input$dblclick, {
    phased_parameters$phases[current_scan(), "pivot"] <- input$dblclick$x
  })

  # Enable / Disable set buttons depending on whether the plot has been annotated
  shiny::observe({
    if (is.null(input$brush)) {
      jms.classes:::disableInput("apk0_button", session)
      jms.classes:::disableInput("apk1_button", session)
    } else {
      jms.classes:::enableInput("apk0_button", session)
      jms.classes:::enableInput("apk1_button", session)
    }
  })

  # Just update the sliders directly as we don't need to store these values anywhere
  shiny::observeEvent(input$apk0_button, {
    brush <- input$brush
    if (!is.null(brush)) {
      new_range <- 1 - (c(brush$xmax, brush$xmin) - xrange()[[2]]) / (diff(xrange()) * -1)
      shiny::updateSliderInput(session, "apk0", value=new_range)
    }
  })

  shiny::observeEvent(input$apk1_button, {
    brush <- input$brush
    if (!is.null(brush)) {
      new_range <- 1 - (c(brush$xmax, brush$xmin) - xrange()[[2]]) / (diff(xrange()) * -1)
      shiny::updateSliderInput(session, "apk1", value=new_range)
    }
  })

  ##### OUTPUT ####

  output$scan_slider <- shiny::renderUI({
    shiny::withProgress(message="Loading Data", value=1, {
      shiny::sliderInput(session$ns("scan"), "Scan", 1, nscans(), 1)
    })
  })

  output$p0_slider <- shiny::renderUI({
    force(input$scan)
    initial <- shiny::isolate(current_parameters()[["p0"]])
    shiny::sliderInput(session$ns("p0"), "P0", -180, 180, initial, step=0.01)
  })

  output$p1_slider <- shiny::renderUI({
    force(input$scan)
    initial <- shiny::isolate(current_parameters()[["p1"]])
    shiny::sliderInput(session$ns("p1"), "P1", -180, 180, initial, step=0.01)
  })

  output$pivot_slider <- shiny::renderUI({
    force(input$scan)
    initial <- shiny::isolate(current_parameters()[["pivot"]])
    shiny::sliderInput(session$ns("pivot"), "Pivot", xrange()[[2]], xrange()[[1]], initial, step=0.01)
  })

  output$xzoom_slider <- shiny::renderUI({
    shiny::withProgress(message="Loading Data", value=1, {
      xr <- xrange() * -1
      if (any(is.na(xr))) {
        return()
      }
      shiny::sliderInput(session$ns("xzoom"), "X Zoom", xr[[1]], xr[[2]], xr, ticks=F)
    })
  })

  output$zoom_slider <- shiny::renderUI({
    shiny::withProgress(message="Loading Data", value=1, {
      shiny::sliderInput(session$ns("yzoom"), "Y Zoom", yrange()[[1]], yrange()[[2]], init_yrange(), ticks=F)
    })
  })

  output$apk0_slider <- shiny::renderUI({
    shiny::sliderInput(session$ns("apk0"), "APK P0 Range", 0, 1, init_apk0_range()[["range"]], ticks=F)
  })

  output$apk1_slider <- shiny::renderUI({
    shiny::sliderInput(session$ns("apk1"), "APK P1 Range", 0, 1, init_apk1_range()[["range"]], ticks=F)
  })


  output$scan_title <- shiny::renderText(sprintf("Scan #%s", current_scan()))

  output$phase_params <- shiny::renderText({
    sprintf("P0: %s; P1: %s; Pivot: %s", input$p0, input$p1, input$pivot)
  })

  output$spectrum <- shiny::renderPlot({
    shiny::withProgress(message="Preparing plot", value=1, {
      if (length(data()) == 0) {
        return()
      }
      yr <- yzoom()
      if (is.null(yr) || any(is.na(yr))) {
        return()
      }
      xr <- xzoom()
      if (is.null(xr) || any(is.na(xr))) {
        return()
      }
      if (is.null(input$p0) || is.null(input$p1) || is.null(input$pivot)) {
        return()
      }
      phased <- .single_phase(data()[, c(1, current_scan() + 1)], input$p0, input$p1, input$pivot)
      phased <- makeReal(phased)
      plot(phased, xlim=xr, ylim=yr)
      abline(v=input$pivot, col="red")

      if (input$show_apk) {
        apk0 <- apk0()
        if (length(apk0) == 0) {
          return()
        }
        apk1 <- apk1()
        if (length(apk1) == 0) {
          return()
        }

        rect(apk0[[1]], yr[[1]], apk0[[2]], yr[[2]], col=rgb(0, 0, 1, alpha=0.2))
        rect(apk1[[1]], yr[[1]], apk1[[2]], yr[[2]], col=rgb(0, 1, 0, alpha=0.2))
      }
    })
  })

  ##### APK ####

  shiny::observeEvent(input$do_apk, {
    values <- apk_values(
      data()[, c(1, input$scan + 1)],
      apk0(), apk1(), input$pivot,
      c(-180, 180), c(-180, 180)
    )
    values <- round(values, 2)
    if (!is.null(values[["p0"]])) phased_parameters$phases[current_scan(), "p0"] <- values[["p0"]]
    if (!is.null(values[["p1"]])) phased_parameters$phases[current_scan(), "p1"] <- values[["p1"]]
    phased_parameters$phases[current_scan(), "pivot"] <- input$pivot
  })

  shiny::observeEvent(input$do_apk_all, {
    shiny::withProgress(message="APK", min=0, max=nscans(), value=1, {
      fun <- function(x) shiny::incProgress(1, detail=paste("scan ", x))
      values <- apkpseudo2d_values(data(),
        apk0(), apk1(), input$pivot,
        c(-180, 180), c(-180, 180),
        .progress=fun
      )
      values <- round(values, 2)
      shiny::withProgress(message="APK - storing values", value=1, {
        phased_parameters$phases <- as.matrix(values)
      })
    })
  })

  ##### RESULT ####

  script_input <- shiny::reactive({
    if (is.null(complete_phasing_matrix())) {
      return("")
    }
    if (is_complex()) {
      df_input <- paste(deparse(complete_phasing_matrix()), collapse="\n")
      return(sprintf(
        "# The following code was automatically generated using the interactivePhase command\n
# Do not edit the following, if you wish to manually change the parameters,
# use the GUI or see ?apkpseudo2d for an alternate phasing mechanism\n
phasing_parameters = %1$s\n%2$s = phase(%2$s, phasing_parameters)\n%2$s = makeReal(%2$s)",
        df_input, data_name()
      ))
    } else {
      return("# Only data with complex intensities may be phased.")
    }
  })

  # Phasing matrix with gaps filled in by nearest neighbour
  complete_phasing_matrix <- shiny::reactive({
    mat <- phasing_matrix()
    if (is.null(mat)) {
      return()
    } # Return NULL if phasing matrix doesn't exist
    rows_with_na <- sapply(1:nrow(mat), function(i) any(is.na(mat[i, ])))
    if (all(rows_with_na)) {
      return()
    } # Return NULL if every row has NA
    if (any(rows_with_na)) {
      # Find closest known rows and replace
      known_scans <- as.numeric(rownames(mat)[!rows_with_na])
      na_scans <- as.numeric(rownames(mat)[rows_with_na])
      nearest_known <- sapply(na_scans, function(r) {
        known_scans[[which.min(abs(known_scans - r))]]
      })
      mat[na_scans, ] <- mat[nearest_known, ]
    }
    mat
  })

  ##### EXPORT ####

  shiny::observeEvent(input$copy_r, {
    jms.classes::clipboard_copy(script_input())
  })

  shiny::observeEvent(input$copy_tab, {
    df_tab <- capture.output(print.data.frame(as.data.frame(complete_phasing_matrix(),
      max=.Machine$integer.max
    )))
    jms.classes::clipboard_copy(df_tab)
  })

  output$export_csv <- shiny::downloadHandler(
    filename="Phase_parameters.csv",
    content=function(file) {
      write.csv(complete_phasing_matrix(), file)
    }
  )

  phased_data <- shiny::reactive({
    pdata <- data()
    if (is_complex() && length(data()) != 0) {
      mat <- complete_phasing_matrix()
      if (!is.null(mat)) {
        pdata <- phase(data(), mat)
      }
    }
    makeReal(pdata)
  })


  #### State saving ####

  # Save extra values in state$values when we bookmark
  shiny::onBookmark(function(state) {
    state$values$phases <- phased_parameters$phases
  })

  # Read values from state$values when we restore
  shiny::onRestored(function(state) {
    jms.classes::log.debug("Restoring phase")
    phased_parameters$preventReset <- TRUE
    phased_parameters$phases <- state$values$phases
  })

  # Exclude from bookmarking
  shiny::setBookmarkExclude(c(
    "apk0_button", "apk1_button", "dblclick", "brush",
    "do_apk", "do_apk_all", "copy_r", "copy_tab", "export_csv"
  ))

  #### Return ####

  list(
    data=phased_data,
    script=script_input,
    packages=shiny::reactive({
      c("NMR.Utils")
    }),
    action="Phasing"
  )
}

#' @export
#' @rdname insitu_gui
interactivePhase <- function(nmr) {
  if (missing(nmr) || is.null(nmr)) {
    stop("No data to plot! The nmr argument is required.

Use insitu_gui() if you wish to import and process data interactively.")
  }

  jms.classes::assert_packages("shiny", purpose="Interactive phasing")

  data_name <- deparse(substitute(nmr))

  server <- function(input, output, session) {
    shiny::callModule(
      interactive_phase_mod, "phase",
      shiny::reactive({nmr}), shiny::reactive({data_name})
    )
  }
  ui <- interactive_phase_mod_UI("phase")
  shiny::shinyApp(ui, server)
}
