bookmark_version <- 1
bookmark_migrate <- function(states, version) {
  # If anything changes in the wizard, we may need to adjust the data in order to restore properly
  # Such migration could be added here.
  # Currently the only possible version is 1, which will not trigger migration.
  stop("Data were saved using an unsupported version of the wizard -- unable to restore.")
}


get_bookmarked_states <- function() {
  state_file <- file.path(jms.classes:::config_dir, "nmr_insitu_gui", "states")
  available_states <- list(version=bookmark_version)
  if (file.exists(state_file)) {
    available_states <- readRDS(state_file)
    version <- available_states$version
    if (!version == bookmark_version) {
      available_states <- bookmark_migrate(available_states, version)
    }
  }
  available_states
}

save_bookmarked_states <- function(states) {
  state_file <- file.path(jms.classes:::config_dir, "nmr_insitu_gui", "states")
  saveRDS(states, state_file)
}

navbarPageSaveLoad <- function(..., id=NULL) {
  ns <- shiny::NS(id)
  navbar <- shiny::navbarPage(..., id=id)
  inputs <- shiny::tagList(
    shiny::actionButton(ns("save"), label="Save", icon=shiny::icon("save"), class="btn-link"),
    shiny::actionButton(ns("load"), label="Load", icon=shiny::icon("folder-open"), class="btn-link")
  )
  form <- tags$form(class="navbar-form navbar-right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form
  )
  navbar
}

# Custom selectize input
bookmark_selector <- function(inputId, label, choices, selected=NULL) {
  selected <- shiny::restoreInput(id=inputId, default=selected)
  if (is.null(selected)) {
    selected <- shiny:::firstChoice(choices)
  }
  else {
    selected <- as.character(selected)
  }

  choices <- mapply(choices, names(choices), FUN=function(v, n) {
    sprintf(
      "<option data-data='%s' value=\"%s\"%s>%s</option>",
      jsonlite::toJSON(v),
      htmltools::htmlEscape(n),
      if (n %in% selected) " selected" else "",
      v$name
    )
  })
  choices <- HTML(paste(choices, collapse="\n"))
  selectTag <- tags$select(id=inputId, choices)
  res <- div(class="form-group shiny-input-container", shiny:::controlLabel(inputId, label), div(selectTag))
  shiny:::selectizeIt(inputId, res, options=list(render=I("
{
  option: function(item, escape) {
  console.log(item);
    return '<div style=\"display: flex; justify-content: space-between;\">' +
              '<span>' + escape(item.name) + '</span>' +
              '<span style=\"color: #f4a022;\">' + escape(item.date) + '</span>' +
            '</div>';
  }
}")))
}

saveLoadServer <- function(input, output, session, default_name=function() {""}) {
  shiny::setBookmarkExclude(c("save", "load", "modal_restore", "modal_restore", "modal_delete"))

  states_update_cookie <- shiny::reactiveVal(0)
  available_states <- shiny::reactive({
    states_update_cookie()
    get_bookmarked_states()
  })

  available_states_clean <- shiny::reactive({
    asts <- available_states()
    asts[["version"]] <- NULL
    asts
  })

  has_available_states <- shiny::reactiveVal(FALSE)

  # Make sure has_available_states only invalidates if its value changes
  shiny::observe({
    new <- length(available_states_clean()) > 0
    old <- shiny::isolate(has_available_states())
    if (new != old) {
      has_available_states(new)
    }
  })

  output$modal_restore_state_UI <- shiny::renderUI({
    bookmark_selector(session$ns("modal_restore_state"), "Choose a previously saved state to restore", choices=available_states_clean())
  })

  output$modal_restore_UI <- shiny::renderUI({
    if (has_available_states()) {
      shiny::tagList(
        shiny::uiOutput(session$ns("modal_restore_state_UI"), inline=T, style="display: inline-block;"),
        shiny::span(jms.classes::deleteConfirmButton(session$ns("modal_delete")),
          style="display: inline-block;vertical-align: top;margin-top:25px"
        ),

        shiny::actionButton(session$ns("modal_restore"), label="Restore", icon=shiny::icon("sync-alt"), style="display:block;")
      )
    } else {
      shiny::span(
        class="text-muted",
        'No saved states available. Use the "save" button to store the current application state, saved states will then appear here'
      )
    }
  })

  shiny::observeEvent(input$load, {
    states_update_cookie(states_update_cookie() + 1) # Force an update
    shiny::showModal(
      shiny::modalDialog(
        title="Restore application state",
        easyClose=TRUE,
        shiny::uiOutput(session$ns("modal_restore_UI"))
      )
    )
  })

  shiny::observeEvent(input$save, {
    shiny::showModal(
      shiny::modalDialog(
        title="Save application state",
        easyClose=TRUE,
        shiny::textInput("._modal_save_name_", "Enter a name to save the current state", value=default_name()),
        shiny::actionButton("._bookmark_", label="Save", icon=shiny::icon("save"))
      )
    )
  })

  shiny::observeEvent(input$modal_delete, {
    id <- input$modal_restore_state
    if (!is.null(id)) {
      jms.classes::log.debug("Deleting state %s (%s)", id, input$modal_restore_state)
      asts <- available_states()
      asts[[id]] <- NULL
      save_bookmarked_states(asts)
      states_update_cookie(states_update_cookie() + 1)

      stateDir <- file.path(jms.classes:::config_dir, "nmr_insitu_gui", "store", id)
      if (dir.exists(stateDir)) {
        jms.classes::log.debug("Removing %s", stateDir)
        unlink(stateDir, recursive=TRUE)
      }
    }
  })

  shiny::observeEvent(input$modal_restore, {
    id <- input$modal_restore_state
    session$sendCustomMessage(type="jsCode", list(code=sprintf("window.location.href = '/?_state_id_=%s';", id)))
    # Redirect
    session$onFlushed(function() {
      # Load new page
      session$onFlushed(function() {
        # Close previous session
        session$close()
      })
    })
  })

  # N.b. https://github.com/rstudio/shiny/issues/1716
  # https://github.com/rstudio/shiny/issues/1731
  shiny::onRestore(function(state) {
    # Make sure all server functions have run (but no render functions)
    session$onFlush(function() {
      elements <- session$userData$bookmarked_suspended_outputs
      restoreTo <- rep_len(TRUE, length(elements))
      names(restoreTo) <- elements
      for (el in elements) {
        current <- session$outputOptions(el)
        if (!is.null(current$suspendWhenHidden)) {
          restoreTo[[el]] <- current$suspendWhenHidden
        }

        # Already namespaced, so use session not shiny::outputOptions
        session$outputOptions(el, suspendWhenHidden=FALSE)
        session$userData$bookmarked_suspended <- restoreTo
      }
    })
  })

  shiny::onRestored(function(state) {
    elements <- session$userData$bookmarked_suspended_outputs
    restoreTo <- session$userData$bookmarked_suspended
    # TODO: surely there must be a neater way to do this ?!
    # One onFlushed per level of nested renderUI?
    session$onFlushed(function() {
      session$onFlushed(function() {
        session$onFlushed(function() {
          session$onFlushed(function() {
            session$onFlushed(function() {
              # Everything restored, reset things to how they were
              for (el in elements) {
                session$outputOptions(el, suspendWhenHidden=restoreTo[[el]])
              }
            })
          })
        })
      })
    })
  })
}

# To be called with any inputs created via renderUI
# So that we can ensure they are restored immediately
setBookmarkSuspendedOutput <- function(..., session = getDefaultReactiveDomain()) {
  session$userData$bookmarked_suspended_outputs <- c(session$userData$bookmarked_suspended_outputs, sapply(list(...), session$ns))
}

# TODO: saving again with the same name -- how best to handle?
# Currently it will just add another item with the same name (possibly a different date)
# We could:
# Ask the user whether to overwrite
# Enforce unique names (refuse to save)
# Versioning?
# ...
saveInterface <- function(id, callback) {
  stateDir <- file.path(jms.classes:::config_dir, "nmr_insitu_gui", "store", id)
  if (!dir.exists(stateDir)) {
    dir.create(stateDir, recursive=TRUE)
  }

  available_states <- get_bookmarked_states()

  session <- shiny::getDefaultReactiveDomain()
  input <- shiny::isolate(session$input)
  state_name <- shiny::isolate(input$`._modal_save_name_`)

  available_states[[id]] <- list(name=state_name, date=Sys.Date())
  save_bookmarked_states(available_states)

  jms.classes::log.info('Saving "%s" state to "%s"', state_name, stateDir)
  callback(stateDir)
  shiny::removeModal()
}

loadInterface <- function(id, callback) {
  stateDir <- file.path(jms.classes:::config_dir, "nmr_insitu_gui", "store", id)
  jms.classes::log.info('Loading state from "%s"', stateDir)
  callback(stateDir)
}
