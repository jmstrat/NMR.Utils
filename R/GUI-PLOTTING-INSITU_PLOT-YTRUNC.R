y_trunc_mod_UI <- function(id) {
  ns = shiny::NS(id)
  shiny::uiOutput(ns('ytruncUI'))
}

y_trunc_mod <- function(input, output, session, enabled, yrange) {
  ytrunc_regions <- shiny::reactiveValues(regions=list(), labels=c())

  output$ytruncUI <- shiny::renderUI({
    if(enabled()) {
      shiny::tagList(
        shiny::fluidRow(shiny::column(12,shiny::div("Enter the left and right limits of a region in which data should be truncated, then hit add.",
                                                    style="text-align: justify;"))),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::numericInput(session$ns('ytruncleft'), 'Left limit', NA, step=10, width='100%')
          ),
          shiny::column(
            4,
            shiny::numericInput(session$ns('ytruncright'), 'Right limit', NA, step=10, width='100%')
          ),
          shiny::column(
            4,
            shiny::actionButton(session$ns('addytruncregion'), 'Add region', style='width: 100%; margin-top: 25px; height:34px')
          )
        ),
        shiny::uiOutput(session$ns('ytruncregions')),
        shiny::numericInput(session$ns('truncLimit'), 'Y value to truncate above (% of upper y limit)', 90, width='100%'),
        shiny::fluidRow(
          shiny::column(
            6,
            restorableColourInput(session$ns('ytrunccolour'), 'Colour of truncation line', value='#bebebe')
          ),
          shiny::column(
            6,
            restorableColourInput(session$ns('ytrunctextcolour'), 'Colour of truncation labels', value='black')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(session$ns('ytruncamp'), 'Factor to divide amplitude of truncation line', 200, 0, step=10, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(session$ns('ytruncoff'), 'Factor to offset truncation line label by', 20, 0, step=5, width='100%')
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(session$ns('ytruncsin'), 'Period of sine wave for truncation line', 5, 0, step=1, width='100%')
          ),
          shiny::column(
            6,
            shiny::numericInput(session$ns('ytrunclwd'), 'Thickness of truncation line', 2, 0, step=0.5, width='100%')
          )
        ),
        shiny::numericInput(session$ns('ytrunccex'), 'Relative font size of truncation label', 1, 0, step=0.5, width='50%')
      )
    }
  })

  output$ytruncregions <- shiny::renderUI({
    regions = list()
    if(length(ytrunc_regions$regions) > 0) {
      delete_buttons = jms.classes:::make_inputs(
        'removeytruncregion',
        seq_along(ytrunc_regions$regions),
        shiny::actionButton, 'Remove region', width='100%',
        session=session)
    }

    for(i in seq_along(ytrunc_regions$regions)) {
      labelInput = extendedTextInput(session$ns('ytruncLabel'), i, ytrunc_regions$labels[[i]], placeholder='Label')
      regions[[i]] <- shiny::tagList(
        shiny::fluidRow(
          shiny::column(12,shiny::span(sprintf('Truncate between %.0f – %.0f ppm:', ytrunc_regions$regions[[i]][[1]], ytrunc_regions$regions[[i]][[2]])))),
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::HTML(labelInput)
          ),
          shiny::column(
            4,
            shiny::HTML(delete_buttons[[i]])
          )
        )
      )
    }
    regions
  })

  shiny::observeEvent(input$addytruncregion, {
    n = length(ytrunc_regions$regions)
    ytrunc_regions$regions[[n+1]] = c(input$ytruncright, input$ytruncleft)
    ytrunc_regions$labels[[n+1]] = NA
  })

  shiny::observeEvent(input$removeytruncregion, {
    components=strsplit(input$removeytruncregion,'_')[[1]]
    n = as.numeric(components[[length(components)-1]])
    ytrunc_regions$regions[[n]] <- NULL
  })

  extendedTextInput <- function(inputId, id, value = "", placeholder = NULL) {
    # oninput will update after each character, but, as plotting is slow, onchange is better in this case
    if(is.na(value)) value=""
    sprintf('<div class="form-group shiny-input-container" style="width: 100%%;">
      <input id="%1$s_%2$s" type="text" class="form-control" value="%3$s" placeholder="%4$s" onchange="Shiny.onInputChange(\'%1$s\',  \'%2$s_\'+this.value)"/>
    </div>', inputId, id, value, placeholder)
  }

  shiny::observeEvent(input$ytruncLabel, {
    n = as.numeric(gsub('^([[:digit:]]*)_.*$', '\\1',input$ytruncLabel))
    lab = gsub('^[[:digit:]]*_(.*)$', '\\1',input$ytruncLabel)
    ytrunc_regions$labels[[n]] <- lab
  })

  ytruncArgs <- shiny::reactive({
    if(enabled()){
      ytrunc = input$truncLimit
      if(is.null(ytrunc)) {
        ytrunc = NA
      } else {
        ytrunc = ytrunc / 100 * yrange()[[2]]
      }
    } else {
      ytrunc = NA
    }
    list(
      y_trunc=ytrunc,
      y_trunc_x_points=ytrunc_regions$regions,
      y_trunc_labels=ytrunc_regions$labels,
      y_trunc_amp_div=input$ytruncamp,
      y_trunc_label_offset_factor=input$ytruncoff,
      y_trunc_sin_period=input$ytruncsin,
      y_trunc_text_col=input$ytrunctextcolour,
      y_trunc_line_col=input$ytrunccolour,
      y_trunc_lwd=input$ytrunclwd,
      y_trunc_cex=input$ytrunccex
      )
  })

  ytruncScript <- shiny::reactive({
    if(enabled()){
      ytrunc = input$truncLimit
      if(is.null(ytrunc)) {
        ytrunc = NA
      } else {
        ytrunc = ytrunc / 100 * yrange()[[2]]
      }
    } else {
      ytrunc = NA
    }

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

    sprintf('# Truncation of strong peaks
  y_trunc=%s,
  y_trunc_x_points=%s,
  y_trunc_labels=%s,
  y_trunc_amp_div=%s,
  y_trunc_label_offset_factor=%s,
  y_trunc_sin_period=%s,
  y_trunc_text_col=%s,
  y_trunc_line_col=%s,
  y_trunc_lwd=%s,
  y_trunc_cex=%s',
  val(ytrunc),
  paste(deparse(ytrunc_regions$regions), collapse=' '),
  paste(deparse(ytrunc_regions$labels), collapse=' '),
  val(input$ytruncamp),
  val(input$ytruncoff),
  val(input$ytruncsin),
  col(input$ytrunctextcolour),
  col(input$ytrunccolour),
  val(input$ytrunclwd),
  val(input$ytrunccex)
  )
  })



  #### State saving ####

  # Save extra values in state$values when we bookmark
  shiny::onBookmark(function(state) {
    state$values$regions <- ytrunc_regions$regions
    state$values$labels <- ytrunc_regions$labels
  })

  # Read values from state$values when we restore
  shiny::onRestored(function(state) {
    jms.classes::log.debug("Restoring plot ytrunc settings")
    ytrunc_regions$regions <- state$values$regions
    ytrunc_regions$labels <- state$values$labels
  })

  shiny::setBookmarkExclude(c("addytruncregion", "removeytruncregion"))

  #### Return ####
  return(list(
    plotArgs=ytruncArgs,
    scriptArgs=ytruncScript))
}
