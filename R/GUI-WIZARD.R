# This can replace the import module when we already have data
# (to start the chain)
existing_data_server <- function(data, data_variable_name) {
  function(input, output, session, ...) {
    data_reactive = shiny::reactive({data})
    data_name_reactive <- shiny::reactive({data_variable_name})
    data_filename_reactive <- shiny::reactive({
      path = attr(data, 'filepath')
      if(!is.null(path) && dir.exists(path)) {
        # Assume bruker
        expt = basename(dirname(path))
        exptNo = basename(path)
        return(paste0(expt, '-', exptNo))
      }
      filename = attr(data, 'filename')
      if(is.null(filename)) {
        return('insitu')
      }
      return(filename)
    })

    script_reactive <- shiny::reactive({
      sprintf('# Note: Data were imported as "%s" prior to interactive processing.\n# Include commands to import the data here\n\n', data_variable_name)
    })

    list(
      data = data_reactive,
      data_name = data_name_reactive,
      data_filename = data_filename_reactive,
      script = script_reactive,
      packages = c()
    )
  }
}

default_modules <- list(
  list(
    ui=interactive_import_mod_UI,
    server=interactive_import_mod,
    name='Import'
  ),
  list(
    ui=interactive_phase_mod_UI,
    server=interactive_phase_mod,
    name='Phasing'
  ),
  list(
    ui=interactive_baseline_mod_UI,
    server=interactive_baseline_mod,
    name='Baseline'
  ),
  list(
    ui=interactive_plotting_mod_UI,
    server=interactive_plotting_mod,
    name='Plot'
  ),
  list(
    ui=export_UI,
    server=export_server,
    name='Export'
  )
)

# Container can be shiny::navbarPage or shiny::tabsetPanel
# Use tabsetPanel to embed the wizard within an existing page (title=NULL);
# use navbarPage for a standalone interface
wizard_mod_UI <- function(id, zoomLevel=1.0, modules=default_modules, container=shiny::navbarPage, title='In Situ NMR Processing Wizard') {
  ns <- shiny::NS(id)

  # Note:
  # CSS zoom doesn't seem to be accounted for w.r.t mouse coordinates
  # May be browser specific
  # Possibly covered by the following issues
  # https://github.com/rstudio/shiny/issues/541
  # https://github.com/rstudio/shiny/issues/2205

  style=sprintf('-moz-transform: scale(%s, %1$s); /* Moz-browsers */
  zoom: %1$s; /* Other non-webkit browsers */
  zoom: %s%%; /* Webkit browsers */', zoomLevel, as.numeric(zoomLevel) * 100)

  pageArgs = list(title, id = ns('tabs'),
                  shiny::tabPanel('About', about_mod_UI(ns('about'))))

  selected <- NULL
  for(mod in modules) {
    if(is.null(mod$ui)) next
    jms.classes::log.debug('Loading UI for %s module', mod$name)
    slug <- make.names(tolower(mod$name))
    pageArgs[[length(pageArgs) + 1]] <- shiny::tabPanel(mod$name, mod$ui(ns(slug)))
    if(is.null(selected)) selected <- mod$name
  }

  pageArgs$selected <- selected

  shiny::fluidPage(
    style=style,
    shiny::tags$head(shiny::tags$style(disconnected_style)),
    do.call(container, pageArgs)
  )
}

wizard_mod <- function(input, output, session, modules=default_modules) {
  # We run through the modules as a chain -- each module takes the output data of the previous as its input
  # Here we store that data
  data_reactives <- list(function() {})
  data_name_reactives <- list(function() {'insitu_nmr_data'})
  data_filename_reactives <- list(function() {'insitu'})

  # This collects the list of script reactives from each module
  script_modules <- list()
  # This collects the list of packages required by each module's script
  package_reactives = c()

  # Here we compile the overall script
  script <- shiny::reactive({
    all_packages <- c()
    for(p in package_reactives) {
      all_packages = c(all_packages, p())
    }
    all_packages = unique(all_packages)
    if(length(all_packages) > 0) {
      packages_script = paste("library('", all_packages, "')", collapse='\n', sep='')
    } else {
      packages_script = ''
    }

    script <- sprintf("# This script was automatically generated using the insitu_gui command\n\n%s", packages_script)
    for(mod in script_modules) {
      mod_script <- mod$script()
      if(is.null(mod_script) || length(mod_script) == 0) mod_script <- ''
      script <- sprintf(
        '%s\n\n%s\n\n%s',
        script,
        hash_header(toupper(mod$name)),
        mod_script
      )
    }
    script
  })

  for(i in 1:length(modules)) {
    mod <- modules[[i]]
    if(is.null(mod$server)) next

    jms.classes::log.debug('Loading %s module', mod$name)
    result <- (function() { # Reactives need their own scope
      mod <- modules[[i]]
      data <- data_reactives[[i]]
      data_name <- data_name_reactives[[i]]
      data_filename <- data_filename_reactives[[i]]
      slug <- make.names(tolower(mod$name))

      isVisible <- shiny::reactive({
        input$tabs == mod$name
      })
      result <- shiny::callModule(mod$server, slug,
                                  data=data,
                                  data_name=data_name,
                                  data_filename=data_filename,
                                  script=script,
                                  visible=isVisible,
                                  embedded=TRUE)
      result2 <- result
      if(!is.null(result$data)) {
        if(!is.null(result$action)) {
          # If the module describes its action, we add a progress indicator
          result2$data <- shiny::reactive({
            shiny::withProgress(message = result$action, value = 1, {
              result$data()
            })
          })
        }
      }
      result2
    })()

    # If the module changes the data, we update the data sent to the next module
    data_reactives[[i + 1]] <- data_reactives[[i]]
    if(!is.null(result$data)) {
      data_reactives[[i + 1]] <- result$data
    }

    # If the module changes the variable name, we update this for the next module
    data_name_reactives[[i + 1]] <- data_name_reactives[[i]]
    if(!is.null(result$data_name)) {
      data_name_reactives[[i + 1]] <- result$data_name
    }

    # If the module changes the file name, we update this for the next module
    data_filename_reactives[[i + 1]] <- data_filename_reactives[[i]]
    if(!is.null(result$data_filename)) {
      data_filename_reactives[[i + 1]] <- result$data_filename
    }

    # If the module provides a script, we store it
    if(!is.null(result$script)) {
      script_modules[[length(script_modules) + 1]] <- list(
        name = mod$name,
        script = result$script
      )
    }

    # If the module provides a list of packages, we store it
    if(!is.null(result$packages)) {
      package_reactives <- c(package_reactives, result$packages)
    }
  }

  shiny::callModule(about_mod, 'about')

  shiny::setBookmarkExclude(c("tabs"))
}


.wizard_package_deps <- c('shiny', 'shinyFiles', 'shinyBS', 'DT', 'colourpicker', 'rstudioapi')

#' Wizard for processing insitu data
#'
#' Use \code{insitu_gui} for the main wizard, or access an individual tab with its respective command.
#' You do not need to provide the \code{nmr} argument to \code{insitu_gui}, doing so will disable the import tab.\cr\cr
#' By default logs will be output to the console during operation, you can adjust the level of detail using the command:\cr
#' \code{jms.classes::set_persistent_setting('NMR-GUI-LOGLEVEL', 'DEBUG')}\cr
#' Replacing \code{'DEBUG'} with \code{'INFO'}, \code{'WARN'}, or \code{'ERROR'}.
#' You can disable logging entirely using \code{'FALSE'}. R may need to be restarted before this command will take effect.
#'
#' @param nmr The NMR data
#' @export
#' @rdname insitu_gui
#' @examples
#' insitu_gui()
insitu_gui <- function(nmr) {
  deps <- as.list(.wizard_package_deps)
  deps$purpose <- 'Interactive processing'
  do.call(jms.classes::assert_packages, deps)

  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  shiny::addResourcePath('www', system.file('www', package='jms.classes'))

  modules <- default_modules
  if(!missing(nmr)) {
    if(!is.nmr2d.data.object(nmr)) {
      stop('NMR Data must be a 2D NMR data object. Use insitu_gui() without arguments to import this graphically.', call.=F)
    }

    data_name = deparse(substitute(nmr))

    # Replace the import tab with a module that just returns the existing data
    modules[[1]] <- list(
      server=existing_data_server(nmr, data_name),
      name='Import'
    )

    # Remove the phasing tab if we only have real data
    if(!any_complex(nmr)) {
      modules <- modules[-2]
    }
  }

  # Null if not defined
  enable_state_saving <- jms.classes::get_persistent_setting('NMR-GUI-ENABLE-STATE')
  if(is.null(enable_state_saving)) {
    enable_state_saving <- FALSE
  } else {
    enable_state_saving <- as.logical(enable_state_saving)
  }

  # Null if not defined
  zoomLevel = jms.classes::get_persistent_setting('NMR-GUI-ZOOM')

  # Null if not defined
  logLevel = jms.classes::get_persistent_setting('NMR-GUI-LOGLEVEL')
  if(is.null(logLevel)) logLevel <- 'INFO'
  if(!logLevel == 'FALSE') {
    jms.classes::jms.logging.threshold(logLevel)
    jms.classes::jms.enable.logging()
    shiny::onStop(jms.classes::jms.disable.logging)
  }

  server <- function(input, output, session) {
    if(enable_state_saving) {
      shiny::callModule(saveLoadServer, "wizard-tabs") # TODO this should be isolated from having to know the id for the navbarPage...
      shiny::shinyOptions(save.interface=saveInterface, load.interface=loadInterface)
    }

    onBookmarked(function(url) {
      jms.classes::log.debug("Successfully saved state")
    })

    # TODO: ideally we would set suspendWhenHidden = FALSE globally on restored to restore all imports, then reset it
    # but https://github.com/rstudio/shiny/issues/1716
    # https://github.com/rstudio/shiny/issues/1731

    shiny::callModule(wizard_mod, "wizard", modules)
  }

  if(enable_state_saving) {
    container <- navbarPageSaveLoad
  } else {
    container <- shiny::navbarPage
  }

  ui <- function(request) {
    wizard_mod_UI("wizard", zoomLevel, modules, container=container)
  }

  if(enable_state_saving) {
    jms.classes::log.debug("Enabling bookmarking (state saving)")
    shiny::shinyApp(ui, server, enableBookmarking = "server")
  } else {
    shiny::shinyApp(ui, server)
  }
}
