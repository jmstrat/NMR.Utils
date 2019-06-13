interactive_baseline_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    jms.classes::jsCodeHandler(),
    NotificationStyle(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns("scan_slider")),
        shiny::uiOutput(ns("xzoom_slider")),
        shiny::uiOutput(ns("zoom_slider")),
        shiny::sliderInput(ns("bkg_avg"), "Number of points to average over (increase for noisy data)",
          1, 20, 4,
          step=1
        ),
        DT::dataTableOutput(ns("baselines_table")),
        shiny::tags$head(
          shiny::tags$style(
            shiny::HTML(".irs-from, .irs-to, .irs-min, .irs-max, .irs-single {visibility: hidden !important;}")
          )
        )
      ),
      shiny::mainPanel(
        shiny::h4(shiny::textOutput(ns("scan_title"))),
        shiny::fluidRow(align="center", shiny::div("Click to add a point. Double click to Remove.")),
        shiny::fluidRow(align="center", shiny::div("Make sure to store a baseline for every scan else
                                                   the baseline will not be subtracted in the output.")),
        shiny::plotOutput(ns("spectrum"),
          height="600px",
          width="100%",
          click=ns("click"),
          dblclick=ns("dblclick")
          # https://github.com/rstudio/shiny/issues/947
          # brush = shiny::brushOpts(id = ns('brush'), direction = "y", resetOnNew = TRUE)
        ),
        shiny::hr(),
        shiny::fluidRow(
          align="center",
          shiny::actionButton(ns("store"), "Store", style="background: #77dd77;"),
          shiny::actionButton(ns("apply_future"), "Store for following scans", style="background: #77dd77;"),
          shiny::actionButton(ns("apply_all"), "Store for all scans", style="background: #77dd77;"),
          shiny::actionButton(ns("reset"), "Reset this scan"),
          shiny::actionButton(ns("reset_all"), "Reset all scans")
        ),
        shiny::hr(),
        shiny::fluidRow(
          align="center",
          shiny::actionButton(ns("copy_r"), "Copy baseline parameters as R input"),
          shiny::actionButton(ns("copy_tab"), "Copy baseline parameters as table"),
          shiny::downloadButton(ns("export_csv"), "Export baseline parameters as CSV")
        )
      )
    )
  )
}


interactive_baseline_mod <- function(input, output, session, data,
                                     data_name, visible=function() TRUE, ...) {

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

  # TODO instructions button and modal popup


  ##### RANGES ####

  xrange <- shiny::reactive({
    if (nrow(data()) == 0) {
      return(c(NA, NA))
    }
    rev(range(data()[, 1]))
  })
  yrange <- shiny::reactive({
    if (nrow(data()) == 0) {
      return(c(NA, NA))
    }
    grDevices::extendrange(range(data()), f=0.05)
  })

  nscans <- shiny::reactive({
    n <- ncol(data())
    if (n > 1) n <- n - 1 # x column
    n
  })

  init_yrange <- shiny::reactive({
    if (nrow(data()) == 0) {
      return(c(NA, NA))
    }
    r <- grDevices::extendrange(range(data()[, 2]), f=0.05)
    r[[2]] <- r[[2]] * 0.1 # Zoom into the baseline
    r
  })


  ##### CURRENT PARAMETERS ####

  current_scan <- shiny::reactive({
    scan <- input$scan
    if (is.null(scan)) {
      return(1)
    }
    return(scan)
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

  shiny::observeEvent(input$click, {
    # TODO: check y
    baseline_parameters$current <- append(baseline_parameters$current, input$click$x)
  })

  shiny::observeEvent(input$dblclick, {
    bsl_x <- current_baseline_x()
    x <- input$dblclick$x
    nearest <- which.min(abs(bsl_x - x))
    if (abs(bsl_x[[nearest]] - x) < abs(diff(xrange())) * 0.05) {
      # TODO: check y
      baseline_parameters$current <- bsl_x[-nearest]
    }
  })

  # Cannot differentiate click and brush
  # https://github.com/rstudio/shiny/issues/947
  # shiny::observe({
  #   if(!is.null(input$brush)) {
  #     shiny::updateSliderInput(session, 'yzoom', value=input$brush$y)
  #   }
  # })

  ##### BASELINES ####

  baselines <- matrix(NA, 0, 5,
    dimnames=list(c(), c("x", "Scans", "Use buttons", "Update buttons", "Clear buttons"))
  )
  baseline_parameters <- shiny::reactiveValues(current=c(), baselines=baselines, last_id=0) # baselines, current


  use <- "bsl_use"
  update <- "bsl_update"
  clear <- "bsl_clear"

  new_row <- function(scans) {
    n <- isolate(baseline_parameters$last_id) + 1
    baseline_parameters$last_id <- n
    new <- list(
      current_baseline_x(),
      scans,
      as.character(
        shiny::actionButton(
          session$ns(paste0(use, "_", n)), "Use",
          counter=0,
          onclick=paste0(
            'this.setAttribute(\"counter\",parseInt(this.getAttribute(\"counter\"))+1);Shiny.onInputChange(\"',
            session$ns(use), '\",  this.id+\"_\"+this.getAttribute(\"counter\"))'
          )
        )
      ),
      as.character(
        shiny::actionButton(
          session$ns(paste0(update, "_", n)), "Update",
          counter=0,
          onclick=paste0(
            'this.setAttribute(\"counter\",parseInt(this.getAttribute(\"counter\"))+1);Shiny.onInputChange(\"',
            session$ns(update), '\",  this.id+\"_\"+this.getAttribute(\"counter\"))'
          )
        )
      ),
      as.character(
        shiny::actionButton(
          session$ns(paste0(clear, "_", n)), "Clear",
          counter=0,
          onclick=paste0(
            'this.setAttribute(\"counter\",parseInt(this.getAttribute(\"counter\"))+1);Shiny.onInputChange(\"',
            session$ns(clear), '\",  this.id+\"_\"+this.getAttribute(\"counter\"))'
          )
        )
      )
    )
    new_mat <- rbind(baseline_parameters$baselines, new, deparse.level=0)
    rownames(new_mat)[[nrow(new_mat)]] <- as.character(n)
    baseline_parameters$baselines <- new_mat
  }

  shiny::observeEvent(input[[use]], {
    components <- strsplit(input[[use]], "_")[[1]]
    n <- components[[length(components) - 1]]
    baseline_parameters$current <- baseline_parameters$baselines[[n, 1]]
  })

  shiny::observeEvent(input[[update]], {
    components <- strsplit(input[[update]], "_")[[1]]
    n <- components[[length(components) - 1]]
    baseline_parameters$baselines[[n, 1]] <- c(current_baseline_x())
  })

  shiny::observeEvent(input[[clear]], {
    components <- strsplit(input[[clear]], "_")[[1]]
    n <- components[[length(components) - 1]]
    mat <- baseline_parameters$baselines
    new_mat <- mat[!rownames(mat) == n, , drop=FALSE]
    if (nrow(new_mat) == 0) {
      baseline_parameters$baselines <- baselines
    } else {
      baseline_parameters$baselines <- new_mat
    }
  })


  # In practical use, the for loop here is faster than sapply et al.
  find_matching_baseline <- function() {
    baselines <- baseline_parameters$baselines
    n <- nrow(baselines)
    if (n == 0) {
      return(NA)
    }
    cur <- current_baseline_x()
    for (i in 1:n) {
      if (identical(cur, baselines[i, 1][[1]])) {
        return(i)
      }
    }
    return(NA)
  }

  add_scans_to_baseline <- function(bsl_idx, scans) {
    scans <- scans[!scans %in% baseline_parameters$baselines[bsl_idx, 2][[1]]]
    baseline_parameters$baselines[[bsl_idx, 2]] <- sort(c(baseline_parameters$baselines[bsl_idx, 2][[1]], scans))
  }

  remove_scans_from_all_baselines <- function(remove_scans) {
    scan_list <- baseline_parameters$baselines[, 2, drop=FALSE]
    if (!length(scan_list)) {
      return()
    }
    new_scan_list <- lapply(scan_list, function(x) x[!x %in% remove_scans])
    baseline_parameters$baselines[, 2] <- new_scan_list
    empty <- sapply(new_scan_list, length) == 0
    if (all(empty)) {
      baseline_parameters$baselines <- baselines
      return()
    }
    baseline_parameters$baselines <- baseline_parameters$baselines[!empty, , drop=FALSE]
  }

  store_baseline_for_scans <- function(scans) {
    remove_scans_from_all_baselines(scans)
    match <- find_matching_baseline()
    if (!is.na(match)) {
      add_scans_to_baseline(match, scans)
    } else {
      new_row(scans)
    }
  }

  shiny::observeEvent(input$store, {
    store_baseline_for_scans(c(current_scan()))
  })

  shiny::observeEvent(input$apply_all, {
    store_baseline_for_scans(1:nscans())
  })

  shiny::observeEvent(input$apply_future, {
    store_baseline_for_scans(current_scan():nscans())
  })

  shiny::observeEvent(input$reset, {
    remove_scans_from_all_baselines(current_scan())
  })

  shiny::observeEvent(input$reset_all, {
    remove_scans_from_all_baselines(1:nscans())
  })

  current_baseline_number <- shiny::reactive({
    scan_list <- baseline_parameters$baselines[, 2, drop=FALSE]
    if (!length(scan_list)) {
      return(NA)
    }
    match <- which(sapply(scan_list, function(x) current_scan() %in% x))
    if (length(match) == 1) {
      return(match)
    }
    return(NA)
  })

  # Update baseline when changing scan if we have a stored baseline for that scan
  shiny::observeEvent(current_scan(), {
    bsl_idx <- current_baseline_number()
    if (!is.na(bsl_idx)) baseline_parameters$current <- baseline_parameters$baselines[[bsl_idx, 1]]
  })

  current_baseline_x <- shiny::reactive({
    baseline_parameters$current
  })


  ##### OUTPUT ####

  output$scan_slider <- shiny::renderUI({
    shiny::withProgress(message="Loading Data", value=1, {
      shiny::sliderInput(session$ns("scan"), "Scan", 1, nscans(), 1)
    })
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

  output$scan_title <- shiny::renderText(sprintf("Scan #%s", current_scan()))

  output$baselines_table <- DT::renderDataTable({
    bsl_idx <- current_baseline_number()

    mat <- baseline_parameters$baselines
    bsl_x <- mat[, 1, drop=F]
    scans <- mat[, 2, drop=F]

    mat[, 1] <- sapply(bsl_x, length)
    mat[, 2] <- sapply(scans, function(x) paste0(jms.classes::numeric_to_string_ranges(x), collapse=", "))
    if (nrow(mat) > 1) storage.mode(mat) <- "character" # renderDataTable has issues if storage mode is list

    mat <- DT::datatable(as.data.frame(mat), # Needed to allow highlighting as has rownames attribute
      class="compact",
      escape=c(1, 2), # 0 based
      colnames=c("# Points", "Scans", "", "", ""),
      autoHideNavigation=TRUE,
      selection="none",
      options=list(
        searching=FALSE,
        lengthChange=FALSE,
        ordering=FALSE,
        processing=FALSE,
        paging=FALSE
      )
    )

    if (is.na(bsl_idx)) {
      return(mat)
    }

    # Highlight row
    DT::formatStyle(mat,
      0,
      target="row",
      backgroundColor=DT::styleEqual(bsl_idx, "yellow")
    )
  })

  output$spectrum <- shiny::renderPlot({
    shiny::withProgress(message="Preparing plot", value=1, {
      if (nrow(data()) == 0) {
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

      xy <- data()[, c(1, current_scan() + 1)]
      plot(xy, xlim=xr, ylim=yr)
      abline(h=0, col=rgb(0, 0, 255, 128, maxColorValue=255))
      mtext(0, side=2, line=0.2, at=0, las=1, col="blue")
      baseline_x <- current_baseline_x()
      if (length(baseline_x) == 0) {
        return()
      }
      baseline <- make_background(xy, baseline_x, returnFunc=TRUE, bkg_y_avg_points=input$bkg_avg)
      x <- xy[, 1]
      y_bsl <- baseline(x)
      lines(x, y_bsl, col="red")
      points(baseline_x, baseline(baseline_x), col="red", pch=16, cex=3)

      lines(x, data()[, current_scan() + 1] - y_bsl, col="gray")
    })
  })

  ##### EXPORT ####

  export_table <- shiny::reactive({
    baseline_parameters$baselines[, c(1, 2), drop=FALSE]
  })

  script_input <- shiny::reactive({
    if (is.null(export_table())) {
      return("")
    }
    df_input <- paste(deparse(export_table()), collapse="\n")
    sprintf(
      "# The following code was automatically generated using the interactiveBaseline command\n
# Do not edit the following, if you wish to manually change the parameters, use the GUI or see ?make_background\n
baseline_parameters = %1$s\n%2$s = %2$s - make_backgrounds(%2$s, expand_baseline_parameters(baseline_parameters, %3$s), bkg_y_avg_points=%4$s)",
      df_input, data_name(), nscans(), input$bkg_avg
    )
  })

  subtracted_data <- shiny::reactive({
    if (is.null(export_table())) {
      return(data())
    }
    if (length(data()) == 0 || nrow(data()) == 0) {
      return(data())
    }
    data() - make_backgrounds(data(), expand_baseline_parameters(export_table(), nscans()), bkg_y_avg_points=input$bkg_avg)
  })

  shiny::observeEvent(input$copy_r, {
    jms.classes::clipboard_copy(script_input())
  })

  fullTable <- shiny::reactive({
    expanded <- expand_baseline_parameters(export_table(), nscans())
    maxL <- max(sapply(expanded, length))
    expanded <- lapply(expanded, function(l) {
      if (is.null(l)) {
        return(rep_len(NA, maxL))
      }
      length(l) <- maxL
      l
    })
    df <- do.call(rbind.data.frame, expanded)
    colnames(df) <- paste0("x_", 1:maxL)
    df
  })

  shiny::observeEvent(input$copy_tab, {
    df_tab <- capture.output(print.data.frame(fullTable(), max=.Machine$integer.max))
    jms.classes::clipboard_copy(df_tab)
  })

  output$export_csv <- shiny::downloadHandler(
    filename="Baseline_parameters.csv",
    content=function(file) {
      write.csv(fullTable(), file, na="")
    }
  )


  #### State saving ####

  # Save extra values in state$values when we bookmark
  shiny::onBookmark(function(state) {
    state$values$current <- baseline_parameters$current
    state$values$baselines <- baseline_parameters$baselines
    state$values$last_id <- baseline_parameters$last_id
  })

  # Read values from state$values when we restore
  shiny::onRestored(function(state) {
    jms.classes::log.debug("Restoring baseline")
    baseline_parameters$current <- state$values$current
    baseline_parameters$baselines <- state$values$baselines
    baseline_parameters$last_id <- state$values$last_id
  })

  # Exclude from bookmarking
  shiny::setBookmarkExclude(c(
    "click", "dblclick", "store", "apply_future", "apply_all", "reset", "reset_all",
    "copy_r", "copy_tab", "export_csv", use, update, clear
  ))

  #### Return ####
  list(
    data=subtracted_data,
    script=script_input,
    packages=shiny::reactive({
      c("NMR.Utils")
    }),
    action="Subtracting baseline"
  )
}

#' @export
#' @rdname insitu_gui
interactiveBaseline <- function(nmr) {
  if (missing(nmr) || is.null(nmr)) {
    stop("No data to plot! The nmr argument is required.

Use insitu_gui() if you wish to import and process data interactively.")
  }

  jms.classes::assert_packages("shiny", "DT", purpose="Interactive baseline correction")

  data_name <- deparse(substitute(nmr))

  server <- function(input, output, session) {
    shiny::callModule(
      interactive_baseline_mod, "baseline",
      shiny::reactive({nmr}),
      shiny::reactive({data_name})
    )
  }

  ui <- interactive_baseline_mod_UI("baseline")
  shiny::shinyApp(ui, server)
}
