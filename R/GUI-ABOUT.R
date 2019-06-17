about_css <- ".about-heading {
width: 100%;
background: #35acea;
color: #FFFFFF;
font-size: 3em;
font-weight: bold;
padding: 0.5em;
}
.about-section {
margin: 3em;
text-align: justify;
}
.about-author td:first-child {
padding-right: 10px;
font-weight: bold;
}
.about-author td {
padding-bottom: 0.5em;
}
.about-optional {
 position: relative;
 text-align: center;
 color: #FFFFFF;
 margin: 0 1em;

 background: #35acea;
 border-radius: 25px;
 font-size: 1.2em;
 height: 25em;
}
.about-optional i {
 font-weight: bold;
 font-size: 5em;

 width: 100%;
 height: 1.5em;
 padding: 0.25em;
 border-radius: 25px 25px 0 0;
}
.about-optional.installed i {
 background:#77dd77;
}
.about-optional.not-installed i {
 background:#ff4f4f;
}
.about-optional-title {
 font-weight: bold;
 font-size: 1.8em;
}
.about-optional p {
 margin: 10px;
}
.about-optional br {
 display: block;
 margin-top: 0.5em;
 content: ' ';
}
.about-optional-package {
 position: absolute;
 width: 100%;
 margin-top: 10px;
 padding: 10px 0;
 border-radius: 0px 0px 25px 25px;
 bottom: 0;
 left: 0;
 background: #76c7f1;
 font-weight: 500;
}

.overflow {
 padding-bottom: 25vh;
}

.about-success-icon {
  color: #77dd77;
  font-weight: bold;
  font-size: 3em;
}

.about-fail-icon {
  color: #ff4f4f;
  font-weight: bold;
  font-size: 3em;
}

.about-code {
  background-color: rgba(27,31,35,.05);
  border-radius: 3px;
  font-size: 85%;
  margin: 0;
  padding: .2em .4em;
  font-family: SFMono-Regular,Consolas,Liberation Mono,Menlo,Courier,monospace;
}"

about_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    style="padding: 0;",
    shiny::tags$head(
      shiny::tags$style(htmltools::HTML(about_css))
    ),
    shiny::div(class="about-heading", "About"),
    shiny::div(
      class="about-section",
      shiny::p("Wizard for processing and ploting operando / in situ NMR experiments.
               Data can be imported, phased, baseline corrected, and plotted. The result
               can be saved as a PNG image, and / or an R script that will output the
               image and has comments indicating what it is doing, and where it can be
               customised. Examples of plots that can be produced with the wizard can
               be found in the papers referenced below."),
      shiny::tags$table(
        class="about-author",
        shiny::tags$tr(
          shiny::tags$td("Author"),
          shiny::tags$td("Josh Stratford")
        ),
        shiny::tags$tr(
          shiny::tags$td("Url"),
          shiny::tags$td(shiny::a(packageDescription("NMR.Utils")$URL,
            href=packageDescription("NMR.Utils")$URL
          ))
        ),
        shiny::tags$tr(
          shiny::tags$td("Examples"),
          shiny::tags$td(
            shiny::a(
              htmltools::HTML("Stratford, J. M.; Allan, P. K.; Grey, C. P. <i>et al.</i>
                              <i>Chem. Commun.</i> <b>2016</b>, <i>52</i>, 12430–12433."),
              href="https://dx.doi.org/10.1039/C6CC06990H"
            )
          )
        ),
        shiny::tags$tr(
          shiny::tags$td(),
          shiny::tags$td(
            shiny::a(
              htmltools::HTML("Stratford, J. M.; Mayo, M.; Allan, P. K.; Grey, C. P. <i>et al.</i>
                              <i>J. Am. Chem. Soc.</i> <b>2017</b>, <i>139</i>, 7273–7286."),
              href="https://dx.doi.org/10.1021/jacs.7b01398"
            )
          )
        )
      )
    ),
    shiny::div(class="about-heading", "Debug Mode"),
    shiny::div(
      class="about-section",
      shiny::p(
        "If you find an issue with the wizard that you cannot solve yourself, you will need
        to collect some debugging information. This is easiest to do by clicking the debug
        mode button below, which will reset the interface and enable additional logging.
        You should then repeat the steps that caused the issue, then click the retrieve
        debug info button to download the necessary information. Bug reports can be submitted at ",
        shiny::a(href=packageDescription("NMR.Utils")$BugReports, packageDescription("NMR.Utils")$BugReports)
      ),
      shiny::uiOutput(ns("debug_mode_ui"), inline=T, style="display: inline-block; margin-top: 10px; vertical-align: middle;"),
      shiny::downloadButton(ns("debug_log"), "Retrieve Debug Info", style="margin-left: 10px; margin-top: 10px;")
    ),
    shiny::div(class="about-heading", "Optional Components"),
    shiny::div(
      class="about-section",
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput(ns("optional_echem"))),
        shiny::column(4, shiny::uiOutput(ns("optional_process"))),
        shiny::column(4, shiny::uiOutput(ns("optional_database")))
      )
    ),
    shiny::div(class="about-heading", "Updates"),
    shiny::div(
      class="about-section",
      shiny::uiOutput(ns("updatesUI"))
    ),
    shiny::div(class="about-heading", "Settings"),
    shiny::div(
      class="about-section overflow",
      shiny::h4("Changing any of these settings requires the wizard to be restarted before
                they will take effect."),

      shiny::h3("Logging"),
      shiny::p("This setting controls the amount of detail in the log which is output to the
               console whilst the wizard is active. During normal use this can be safely set
               to a low level (e.g. warning) or disabled, however, if you encounter a problem
               you should set this to Debug to ensure the greatest level of detail."),
      shiny::uiOutput(ns("settings_log")),

      shiny::h3("Saving"),
      shiny::span("Warning: this is still under active development and has not been thoroughly
                  tested. It shouldn't cause any harm, however, it is possible that it may
                  fail to save or restore some settings, which you will simply have to re-enter.
                  i.e. It should \"just work\", but if you've spent hours getting your plot just
                  right, don't rely on it 100% for now, make sure to export the script or otherwise
                  note down important values as well. For this reason it is not enabled by default,
                  however, having now read this disclaimer, there's little reason not to enable it."),
      shiny::br(),
      shiny::br(),
      shiny::p('Enabling this will add "Save" and "Load" buttons to the navigation bar, which
               allow you to save and restore the current state of the wizard. This is distinct
               from the script that can be saved in the export tab, which cannot be loaded back
               into the interface.'),
      shiny::uiOutput(ns("settings_save")),

      shiny::h3("Zoom"),
      shiny::br(),
      shiny::fluidRow(
        style="margin-top: -20px;",
        shiny::column(2, shiny::uiOutput(ns("settings_zoom"))),
        shiny::column(10, shiny::div("This controls the default zoom level of the wizzard (%).",
          style="margin-top: 25px;"
        ))
      ),
      shiny::span("Warning: Setting this to any value other than 100% can cause issues with
                  interactive plots.")
    )
  )
}

optional_ui <- function(title, description, package) {
  installed <- requireNamespace(package, quietly=TRUE)

  installed_class <- ifelse(installed, "about-optional installed", "about-optional not-installed")
  installed_icon <- ifelse(installed, "check", "times")
  shiny::div(
    class=installed_class,
    shiny::icon(installed_icon),
    shiny::span(class="about-optional-title", title),
    shiny::p(description),
    shiny::br(),
    shiny::span(class="about-optional-package", sprintf("Requires the %s package", package))
  )
}

debug_mode <- new.env()
debug_mode$enabled <- FALSE
about_mod <- function(input, output, session) {
  output$optional_echem <- shiny::renderUI({
    optional_ui("Electrochemistry", "Support for plotting electrochemistry data aligned to
                the NMR plot. Biologic, Arbin, Ivium, Maccor, and Lahne data files are
                supported.", "Echem.Data")
  })

  output$optional_process <- shiny::renderUI({
    optional_ui("Processing Electrochemistry", "Support for processing electrochemistry
                data. Allows replacing the time axis with capacity.", "Echem.Process")
  })

  output$optional_database <- shiny::renderUI({
    optional_ui("Electrochemistry Metadata", "Support for fetching the path for the
                electrochemistry data, and the parameters necessary for processing
                from a database.", "Echem.Database")
  })

  loglevel_map <- list(DEBUG="Debug", INFO="Info", WARN="Warning", ERROR="Error", `FALSE`="Disabled")
  output$settings_log <- shiny::renderUI({
    if (debug_mode$enabled) {
      return(shiny::span("Unable to change this setting whilst debug mode is active", style="color: red;"))
    }

    logLevel <- jms.classes::get_persistent_setting("NMR-GUI-LOGLEVEL")
    if (is.null(logLevel)) logLevel <- "INFO"

    shiny::selectInput(session$ns("loglevel"), "",
      c("Debug", "Info", "Warning", "Error", "Disabled"),
      selected=loglevel_map[[logLevel]]
    )
  })

  output$settings_save <- shiny::renderUI({
    enabled <- jms.classes::get_persistent_setting("NMR-GUI-ENABLE-STATE")
    if (is.null(enabled)) {
      enabled <- FALSE
    } else {
      enabled <- as.logical(enabled)
    }

    jms.classes::toggleInput(session$ns("saving"), "", enabled,
      offlabel="Off", onlabel="On"
    )
  })

  output$settings_zoom <- shiny::renderUI({
    zoomLevel <- jms.classes::get_persistent_setting("NMR-GUI-ZOOM")
    if (is.null(zoomLevel)) {
      zoomLevel <- 1
    }
    shiny::numericInput(session$ns("zoom"), "", as.numeric(zoomLevel) * 100, min=10)
  })

  shiny::observe({
    if (is.null(input$saving)) {
      return()
    }
    jms.classes::set_persistent_setting("NMR-GUI-ENABLE-STATE", input$saving)
  })

  shiny::observe({
    if (is.null(input$loglevel)) {
      return()
    }
    w <- match(input$loglevel, loglevel_map)
    if (is.na(w)) {
      return()
    }

    level <- names(loglevel_map)[[w]]

    if (level == "FALSE") {
      jms.classes::jms.disable.logging()
    } else {
      jms.classes::jms.logging.threshold(level)
      jms.classes::jms.enable.logging()
    }

    jms.classes::set_persistent_setting("NMR-GUI-LOGLEVEL", level)
  })

  zoom_r <- shiny::reactive({
    input$zoom
  })
  zoom_d <- shiny::debounce(zoom_r, 1000)
  shiny::observe({
    if (!is.numeric(zoom_d())) {
      return()
    }
    jms.classes::set_persistent_setting("NMR-GUI-ZOOM", as.numeric(zoom_d()) / 100)
  })

  output$debug_mode_ui <- shiny::renderUI({
    if (debug_mode$enabled) {
      shiny::span("Debug mode is enabled!", style="padding: 5px; border: 1px solid red; border-radius: 4px;")
    } else {
      shiny::actionButton(session$ns("debug_mode"), "Debug Mode", icon=shiny::icon("bug"))
    }
  })

  shiny::observeEvent(input$debug_mode, {
    logFile <- file.path(jms.classes:::config_dir, "nmr_insitu_gui", "debug.log")
    si <- paste0(
      "------------\nSession Info\n------------\n\n", paste0(capture.output(sessionInfo()), collapse="\n"),
      "\n\n------------\n     Log\n------------\n\n"
    )
    writeLines(si, logFile)
    old <- jms.classes::jms.logging.file(logFile)
    shiny::onStop(function() {
      jms.classes::jms.logging.function(old)
      debug_mode$enabled <- FALSE
    }, session=NULL)
    jms.classes::jms.logging.threshold("DEBUG")
    jms.classes::jms.enable.logging()
    debug_mode$enabled <- TRUE
    session$reload()
  })

  output$debug_log <- shiny::downloadHandler(
    filename="debug_log.zip",
    content=function(file) {
      logFile <- file.path(jms.classes:::config_dir, "nmr_insitu_gui", "debug.log")

      if (!file.exists(logFile)) {
        shiny::showNotification(shiny::div(
          htmltools::HTML("<b>Error: Could not find debug log!</b><br/>Enable debug mode and try again.")
        ),
        duration=10,
        type="error", id="error", session=session
        )
        return()
      }
      echem_file <- session$userData$echem_path
      if (is.null(echem_file) || !file.exists(echem_file)) {
        files <- c(logFile)
      } else {
        files <- c(logFile, echem_file)
      }
      # create the zip file
      zip(file, files, flags="-j9X")
    }
  )

  is_outdated <- shiny::reactive({
    jms.classes::log.info("Checking for updates to NMR.Utils")
    shiny::withProgress(message="Checking for updates", value=1, {
      try({
        res <- remotes::dev_package_deps(system.file(package="NMR.Utils"), dependencies=FALSE)
        if (any(res$diff < 0)) {
          jms.classes::log.info("Update available")
          return(T)
        } else {
          jms.classes::log.info("NMR.Utils is up to date")
          return(F)
        }
      })
    })
  })

  checked_outdated <- FALSE
  shiny::observe({
    # Crude hack to avoid blocking startup... (if only R had async...)
    if (!checked_outdated) {
      checked_outdated <<- TRUE
      shiny::invalidateLater(2000)
      return()
    }

    outdated <- is_outdated()
    if (inherits(outdated, "try-error")) {
      return()
    }
    if (outdated) {
      shiny::showNotification(shiny::tagList(
        "An updated version of NMR.Utils is available:",
        shiny::br(),
        shiny::a(href=packageDescription("NMR.Utils")$URL, packageDescription("NMR.Utils")$URL),
        shiny::br()
      ), type="message", duration=10)
    }
  })

  opt_pkgs <- c("Echem.Data", "Echem.Process", "Echem.Database")
  all_outdated <- shiny::reactive({
    jms.classes::log.info("Checking for updates to NMR.Utils and dependencies")
    shiny::withProgress(message="Checking for updates", value=1, {
      try({
        deps <- remotes::dev_package_deps(system.file(package="NMR.Utils"), dependencies=TRUE)

        opt_installed <- sapply(opt_pkgs, requireNamespace, package, quietly=TRUE)
        if (any(opt_installed)) {
          opt_installed <- opt_pkgs[opt_installed]
          for (pkg in opt_installed) {
            deps <- rbind(deps, remotes::dev_package_deps(system.file(package=pkg)))
          }
        }

        wiz_extras <- remotes::package_deps(.wizard_package_deps)
        all <- rbind(deps, wiz_extras)
        all <- as.data.frame(all)[all$diff < 0, c("package", "installed", "available")]
        unique(all)
      })
    })
  })

  checked_all_outdated <- FALSE
  output$updatesUI <- shiny::renderUI({
    # Crude hack to avoid blocking page load (if only R had async...)
    if (!checked_all_outdated) {
      checked_all_outdated <<- TRUE
      shiny::invalidateLater(500)
      return("Checking for updates...")
    }

    pkgs <- all_outdated()

    if (inherits(pkgs, "try-error")) {
      jms.classes::log.error(pkgs)
      return(shiny::div("Unable to check for updates"))
    }

    if (nrow(pkgs) == 0) {
      return(
        shiny::div(
          style="display: flex; align-items: center;",
          shiny::icon("check", class="about-success-icon"),
          shiny::span("All necessary packages are up to date", style="margin-left: 20px;")
        )
      )
    }

    shiny::tagList(
      shiny::div(
        style="display: flex; align-items: center;",
        shiny::icon("times", class="about-fail-icon"),
        shiny::span("Some packages required for this interface are not up to date",
          style="margin-left: 20px;"
        )
      ),

      DT::dataTableOutput(session$ns("outdatedTBl"), width="50%"),
      shiny::div(
        style="margin-top: 30px; font-size: 1.5em;",
        shiny::span("Update all packages with the following command, then restart R:"),
        shiny::br(),
        shiny::span("remotes::update_packages()", class="about-code")
      )
    )
  })

  output$outdatedTBl <- DT::renderDataTable({
    df <- all_outdated()
    jms.classes::log.debug("Outdated packages: %s", paste0(df$package, collapse=", "))

    colnames(df) <- c("Package", "Installed Version", "Available Version")

    table <- DT::datatable(
      df,
      class="compact hover",
      rownames=FALSE,
      options=list(
        paging=FALSE,
        ordering=FALSE,
        searching=FALSE,
        info=FALSE,
        columnDefs=list(list(className="dt-center", targets="_all"))
      )
    )
    DT::formatStyle(table, 1, `border-right`="1px solid black")
  })

  shiny::setBookmarkExclude(c("loglevel", "saving", "zoom", "debug_mode", "debug_log"))
}
