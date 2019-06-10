# TODO investigate getHook("before.plot.new") and getHook("plot.new") -- can we use this to remove the need to call annotateFunc??
#      May also then be able to make a stand alone annotation gui (along with recordPlot & replayPlot)
# TODO: add clipping (xpd) option: Plot (FALSE) Figure (TRUE) None (NA) -- only display if not provided to annotateFunc
# USAGE: currently the module takes the following arguments
# click, doubleClick, hover --> shiny reactives that provide normalised x and y cordinates on a mouse click
# layoutMatrix --> reactive giving the layout matrix
# plotNames --> reactive giving the names for the plots in the layout
# nPlots --> number of plots in the layout (can be fewer than the max value in matrix)
#
# And returns a list of:
# annotateFunc --> to be called immediately after each plot
# scriptFunc --> script to be inserted immediately after each plotting script
# coordinates --> currently selected plot #, x and y coordinates
# ndccoordinates --> currently selected x and y normalised coordinates
# updatendc --> function to refresh ndc coordinates if the plot has changed

annotator_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div('Click on the plot to add an annotation, or enter the x, y and plot number manually and click add. Double click to select an existing annotation.'),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns('n_select'))),
      shiny::column(2, shiny::numericInput(ns('x'), 'x', NA)),
      shiny::column(2, shiny::numericInput(ns('y'), 'y', NA)),
      shiny::column(4, shiny::uiOutput(ns('dataNote')))
    ),
    shiny::uiOutput(ns('type_select_UI')),
    shiny::uiOutput(ns('type_options_UI')),
    shiny::uiOutput(ns('clipping_options_UI')),
    shiny::fluidRow(
      shiny::column(3, jms.classes::add_update_delete_buttonUI(ns('addupdatedelete'))),
      shiny::column(3,  shiny::tags$button(id=ns('clear'), type = "button", class = "btn action-button btn-danger", style="margin-top: 25px; width: 100%;", 'Clear Plot'))
      )
  )
}

# TODO: Backspace == delete
# tags$script('
#             pressedKeyCount = 0;
#             $(document).on("keydown", function (e) {
#             if( key == 8 || key == 46 ) {
#             return false;
#             Shiny.onInputChange("pressedKey", pressedKeyCount++);
#             Shiny.onInputChange("pressedKeyId", e.which);
#             }
#             });'
#   )


registeredPlotAnnotationTypes = list()

#' Register an interface for drawing an annotation within \code{\link{interactivePlotting}}
#'
#' Note that this function is not persistent. You must re-register in every R session you wish to use this annotation type.
#'
#' @param name Name of the annotation type
#' @param uiFunction Shiny module UI function that takes an id parameter and returns a \code{\link[shiny]{tagList}}.
#' @param serverFunction Shiny module server function that returns a \code{\link[base]{list}} with items: \cr
#' \describe{
#'  \item{arguments}{Function that returns a list of arguments to be passted to \code{drawFunc}}
#'  \item{drawFunc}{Function that draws the annotation. Takes a list of arguments as returned above, plus x and y coordinates as the first two arguments, and (possibly) a named xpd argument. Should NOT be reactive nor depend on the UI.}
#'  \item{script}{Function that returns a string containing the code to draw the annotation. Takes the same arguments as \code{drawFunc}. Should NOT be reactive nor depend on the UI.}
#'  \item{getBoundingRect}{Function that returns vector containing the bounding rect of the annotation for hit testing. Takes the same arguments as \code{drawFunc}, returns c(left, bottom, right, top).}
#'  \item{focus}{Function that highlights an annotation (typically by drawing a red border). Takes the same arguments as \code{drawFunc}.}
#'  \item{updateInputs}{Function that updates the UI to reflect the argument list. Takes the same arguments returned by \code{arguments}.}
#' }
#' @export
#' @seealso  \link{http://shiny.rstudio.com/articles/modules.htm}
register_plot_annotation_module <- function(name, uiFunction, serverFunction) {
  registeredPlotAnnotationTypes[[name]] <<- list(
    name = name,
    ui = uiFunction,
    server = serverFunction
  )
}

annotator_mod <- function(input, output, session,
                          click=function() {}, doubleClick=function() {}, hover=function() {},
                          layoutMatrix=function(){matrix(1)}, plotNames=function(){1:max(layoutMatrix())},
                          nPlots=function(){max(layoutMatrix())}) {

  annotation_types = list()
  if(length(registeredPlotAnnotationTypes)) {
    for (i in 1:length(registeredPlotAnnotationTypes)) {
      anoType = registeredPlotAnnotationTypes[[i]]
      annotation_types[[anoType$name]] <- list(
        ui = anoType$ui,
        result = shiny::callModule(anoType$server, paste0('anoType', i))
      )
    }
  }

  output$type_select_UI <- shiny::renderUI({
    if(length(annotation_types) == 0) {
      return(
        shiny::tagList(
          shiny::h3('No plot annotation modules have been added.'),
          shiny::span('See ?register_plot_annotation_module')
        )
      )
    }
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(session$ns('type_select'), 'Annotation Type', names(annotation_types))),
      shiny::column(5, shiny::div(style='margin-top: 20px;','Add additional custom annotations to this list with the command\n',shiny::code('register_plot_annotation_module')))
    )
  })

  output$type_options_UI <- shiny::renderUI({
    if(is.null(input$type_select)) return()
    w <- which(input$type_select == names(annotation_types))
    (annotation_types[[w]])$ui(session$ns(paste0('anoType', w)))
  })

  output$clipping_options_UI <- shiny::renderUI({
    # TODO this should be conditional depending on whether the plot supports clipping
    # (currently all do, but we support the case that they don't with options to annotateFunc and scriptFunc -- this will need to change)
    shiny::radioButtons(session$ns('clipping'), 'Clipping', inline=T,
                        choiceNames=c('Clip to plot', 'Clip to figure', 'No clipping'),
                        choiceValues=c("FALSE", "TRUE", "NA"),
                        selected = "TRUE")
  })

  output$n_select <- shiny::renderUI({
    shiny::selectInput(session$ns('n'), 'Plot', plotNames()[1:nPlots()])
  })

  ncol <- shiny::reactive({
    base::ncol(layoutMatrix())
  })

  nrow <- shiny::reactive({
    base::nrow(layoutMatrix())
  })

  whichPlot <- function(x, y) {
    col <- findInterval(x, 0:ncol()/ncol())
    row <- findInterval(1-y, 0:nrow()/nrow())
    layoutMatrix()[[row,col]]
  }

  storedCoordinates <- list()

  storeCoordinatesFromPar <- function(n) {
    storedCoordinates[[n]] <<- list(
      usr=par('usr'), # c(x1, x2, y1, y2) in user
      plt=par('plt'), # c(x1, x2, y1, y2) in NFC
      fig=par('fig') # c(x1, x2, y1, y2) in NDC
    )
  }

  storeCoordinatesFromData <- function(n, data) {
    storedCoordinates[[n]] <<- data
  }

  rescaleFuncs <- list()

  storeRescale <- function(n, funcs) {
    rescaleFuncs[[n]] <<- funcs
  }

  ndcToUser <- function(x, y) {
    n <- whichPlot(x, y)
    if(n > nPlots() || n > length(storedCoordinates)) return()
    stored <- storedCoordinates[[n]]
    if(is.null(stored)) return()

    xfig <- (x - stored$fig[[1]]) / (stored$fig[[2]] - stored$fig[[1]])
    yfig <- (y - stored$fig[[3]]) / (stored$fig[[4]] - stored$fig[[3]])

    xplt <- (xfig - stored$plt[[1]]) / (stored$plt[[2]] - stored$plt[[1]])
    yplt <- (yfig - stored$plt[[3]]) / (stored$plt[[4]] - stored$plt[[3]])

    xusr <- xplt * (stored$usr[[2]] - stored$usr[[1]]) + stored$usr[[1]]
    yusr <- yplt * (stored$usr[[4]] - stored$usr[[3]]) + stored$usr[[3]]

    if(length(rescaleFuncs) >= n && !is.null(rescaleFuncs[[n]])) {
      rescale = rescaleFuncs[[n]]$userToVisible
      if(!is.null(rescale)) {
        xy = rescale(xusr, yusr)
        xusr = xy[[1]]
        yusr = xy[[2]]
      }
    }
    c(x=signif(xusr, 3), y=signif(yusr, 3), n=n)
  }

  userTondc <- function(n, x, y) {
    if(n > nPlots() || n > length(storedCoordinates)) return()
    stored <- storedCoordinates[[n]]
    if(length(rescaleFuncs) >= n && !is.null(rescaleFuncs[[n]])) {
      rescale = rescaleFuncs[[n]]$visibleToUser
      inverse_rescale = rescaleFuncs[[n]]$userToVisible
      if(!is.null(rescale) && !is.null(inverse_rescale)) {
        xy = rescale(x, y)
        x = xy[[1]]
        y = xy[[2]]
      }
    }

    xplt = (x - stored$usr[[1]]) / (stored$usr[[2]] - stored$usr[[1]])
    yplt = (y - stored$usr[[3]]) / (stored$usr[[4]] - stored$usr[[3]])

    xfig = xplt * (stored$plt[[2]] - stored$plt[[1]]) + stored$plt[[1]]
    yfig = yplt * (stored$plt[[4]] - stored$plt[[3]]) + stored$plt[[3]]

    xndc = xfig * (stored$fig[[2]] - stored$fig[[1]]) + stored$fig[[1]]
    yndc = yfig * (stored$fig[[4]] - stored$fig[[3]]) + stored$fig[[3]]

    c(x=signif(xndc, 3), y=signif(yndc, 3))
  }

  focusedAnnotation <- shiny::reactiveValues(n=NULL, annotation=NULL)

  shiny::observeEvent(hover(), ignoreNULL=FALSE, {
    cl <- hover()
    # Note hover() is often null even when the mouse is still over the plot -- so don't set x & y to NA
    if(is.null(cl) || !is.null(focusedAnnotation$n)) return()
    c <- ndcToUser(cl$x, cl$y)
    if(is.null(c) || c[['n']] > nPlots()) return()
    shiny::updateNumericInput(session, 'x', value=c[['x']])
    shiny::updateNumericInput(session, 'y', value=c[['y']])
    shiny::updateSelectInput(session, 'n', selected = plotNames()[[c[['n']]]])
  })

  throttledClick <- shiny::throttle(click, 1000)
  shiny::observeEvent(throttledClick(), {
    if(!is.null(focusedAnnotation$n)) {
      focusedAnnotation$n <- NULL
      focusedAnnotation$i <- NULL
      focusedAnnotation$annotation <- NULL
      return()
    }

    cl <- click()
    if(is.null(cl)) return()

    c <- ndcToUser(cl$x, cl$y)
    if(is.null(c) || c[['n']] > nPlots()) return()
    shiny::updateNumericInput(session, 'x', value=c[['x']])
    shiny::updateNumericInput(session, 'y', value=c[['y']])
    shiny::updateSelectInput(session, 'n', selected = plotNames()[[c[['n']]]])
    addAnnotation()
  })

  onNextTick <- function(expr, env=parent.frame()) {
    hasRun <- 0
    o <- observe({
      invalidateLater(100)
      if (hasRun==1) {
        eval(expr, envir=env)
      }
      if (hasRun >= 1) {
        o$destroy()
      }
      hasRun <<- hasRun+1
    });
  }

  shiny::observeEvent(doubleClick(), {
    cl <- doubleClick()
    if(is.null(cl)) return()
    c <- ndcToUser(cl$x, cl$y)
    if(is.null(c) || c[['n']] > nPlots()) return()

    x <- cl$x
    y <- cl$y
    n <- c[['n']]
    id = paste0('ano_', n)
    if(! id %in% names(plotAnnotations)) return()

    # Hit test
    anos <- plotAnnotations[[id]]

    for(i in seq_along(anos)) {
      a = anos[[i]]
      bounds <- a$ndcbounds
      if(x >= bounds[[1]] && y >= bounds[[2]] && x <= bounds[[3]] && y <= bounds[[4]]) {
        shiny::updateSelectInput(session, 'type_select', selected = a$type)
        focusedAnnotation$n <- n
        focusedAnnotation$i <- i
        focusedAnnotation$annotation <- a
        shiny::updateNumericInput(session, 'x', value=a$x)
        shiny::updateNumericInput(session, 'y', value=a$y)
        shiny::updateSelectInput(session, 'n', selected = plotNames()[[n]])
        shiny::updateRadioButtons(session, 'clipping', selected=toString(a$clipping))
        onNextTick({do.call(annotation_types[[a$type]]$result$updateInputs, a$args)})
        return()
      }
    }
    # Missed
    focusedAnnotation$n <- NULL
    focusedAnnotation$i <- NULL
    focusedAnnotation$annotation <- NULL
  })


  dataReactives <- list()

  storeData <- function(n, dataReactive) {
    dataReactives[[n]] <<- dataReactive
  }

  dataAvailableForPlot <- shiny::reactive({
    if(is.null(input$n)) return(FALSE)
    n = which(plotNames() == input$n)
    if(length(dataReactives) >= n && !is.null(dataReactives[[n]])) {
      data <- dataReactives[[n]]()
      return(!is.null(data) && jms.classes::is.jms.data.object(data))
    }
    return(FALSE)
  })

  output$dataNote <- shiny::renderUI({
    if(dataAvailableForPlot()) {
      return(shiny::div(style='margin-top: 20px; text-align: justify', "This plot suppors pinning annotations to the plotted data by leaving either the x or y field blank."))
      # possibly want an offset field of some description as well
    }
    return()
  })

  replaceMissingCoordinateWithData <- function(n, x, y) {
    dataReactive <- NULL
    if(length(dataReactives) >= n && !is.null(dataReactives[[n]])) {
      dataReactive <- dataReactives[[n]]
    }
    # TODO In case of multiple y cols: take largest possible value?
    if(!is.null(dataReactive) && is.na(x) && !is.na(y)) {
      data <- dataReactives[[n]]()
      xscale = jms.classes::xscale(data)
      yscale = jms.classes::yscale(data)
      args = list(data)
      args[[colnames(data)[[jms.classes::ycol(data)]]]] = y / yscale
      x <- as.numeric(do.call(Plotting.Utils::nearest, args)[,jms.classes::xcol(data)]) * xscale
    } else if(!is.null(dataReactive) && !is.na(x) && is.na(y)) {
      data <- dataReactives[[n]]()
      xscale = jms.classes::xscale(data)
      yscale = jms.classes::yscale(data)
      args = list(data)
      args[[colnames(data)[[jms.classes::xcol(data)]]]] = x / xscale
      y <- as.numeric(do.call(Plotting.Utils::nearest, args)[,jms.classes::ycol(data)]) * yscale
    } else if(is.na(x) || is.na(y)) {
      return()
    }
    return(c(x,y))
  }

  plotAnnotations = shiny::reactiveValues()

  addAnnotation <- function() {
    shiny::isolate({
      if(is.null(input$n) || is.null(input$x) || is.null(input$y) || is.null(input$type_select)) return()

      x = input$x
      y = input$y
      n = which(plotNames() == input$n)

      xy <- replaceMissingCoordinateWithData(n, x, y)
      if(is.null(xy)) return()
      x <- xy[[1]]
      y <- xy[[2]]

      id = paste0('ano_', n)
      ans <- plotAnnotations[[id]]

      plotAnnotations[[id]][[length(ans) + 1]] <- list(
        x=x,
        y=y,
        type=input$type_select,
        clipping=as.logical(input$clipping),
        args=annotation_types[[input$type_select]]$result$arguments()
      )
    })
  }

  updateAnnotation <- function() {
    shiny::isolate({
      if(is.null(input$n) || is.null(input$x) || is.null(input$y) || is.null(input$type_select)) return()

      n <- focusedAnnotation$n
      i <- focusedAnnotation$i

      x = input$x
      y = input$y

      xy <- replaceMissingCoordinateWithData(n, x, y)
      if(is.null(xy)) return()
      x <- xy[[1]]
      y <- xy[[2]]

      id = paste0('ano_', n)
      if(! id %in% names(plotAnnotations)) return()

      plotAnnotations[[id]][[i]] <- list(
        x=x,
        y=y,
        type=input$type_select,
        clipping=as.logical(input$clipping),
        args=annotation_types[[input$type_select]]$result$arguments()
      )
      focusedAnnotation$annotation <- plotAnnotations[[id]][[i]]
    })
  }

  deleteAnnotation <- function() {
    n <- focusedAnnotation$n
    i <- focusedAnnotation$i

    id = paste0('ano_', n)
    if(! id %in% names(plotAnnotations)) return()

    # Delete annotation
    plotAnnotations[[id]][[i]] <- NULL

    focusedAnnotation$n <- NULL
    focusedAnnotation$i <- NULL
    focusedAnnotation$annotation <- NULL

    shiny::updateNumericInput(session, 'x', value=NA)
    shiny::updateNumericInput(session, 'y', value=NA)
  }

  callbacks <- function(button) {
    if(button == 'add') {
      addAnnotation()
    } else if(button == 'delete') {
      deleteAnnotation()
    } else {
      updateAnnotation()
    }
  }

  isSelected <- shiny::reactive({
    !is.null(focusedAnnotation$n)
  })

  shiny::callModule(jms.classes::add_update_delete_button, 'addupdatedelete', isSelected, callbacks)

  shiny::observeEvent(input$clear, {
    if(is.null(input$n) || is.null(input$x) || is.null(input$y) || is.null(input$type_select)) return()
    n = which(plotNames() == input$n)

    id = paste0('ano_', n)
    if(! id %in% names(plotAnnotations)) return()
    plotAnnotations[[id]] <- list()

    focusedAnnotation$n <- NULL
    focusedAnnotation$i <- NULL
    focusedAnnotation$annotation <- NULL
  })

  # To be called after plot #n is drawn
  annotateFunc <- function(n, coordinates=NULL, rescaleParams=NULL, xpd=NULL, data=NULL) {
    jms.classes::log.debug('Adding annotations to plot %s', n)
    shiny::isolate({
      if(is.null(coordinates)) {
        storeCoordinatesFromPar(n)
      } else {
        storeCoordinatesFromData(n, coordinates)
      }
      if(!is.null(rescaleParams)) {
        storeRescale(n, rescaleParams)
      }
      if(!is.null(data)) {
        storeData(n, data)
      }
    })

    id = paste0('ano_', n)
    if(! id %in% names(plotAnnotations)) return()
    # Draw the annotations
    anos <- plotAnnotations[[id]]
    for(i in seq_along(anos)) {
      a = anos[[i]]
      x = a$x
      y = a$y
      shiny::isolate({
        if(length(rescaleFuncs) >= n && !is.null(rescaleFuncs[[n]])) {
          rescale = rescaleFuncs[[n]]$visibleToUser
          if(!is.null(rescale)) {
            r <- rescaleFuncs[[n]]$visibleToUser(x, y)
            x <- r[[1]]
            y <- r[[2]]
          }
        }

        args <- a$args

        if(!is.null(xpd)) {
          args$xpd=xpd
        } else {
          args$xpd=a$clipping
        }

        args <- c(list(x, y), args)

        bounds <- do.call(annotation_types[[a$type]]$result$getBoundingRect, args)
        # Enlarge region
        bounds <- c(
          grconvertX(bounds[[1]] - xinch() * 0.1, to='ndc'),
          grconvertY(bounds[[2]] - yinch() * 0.1, to='ndc'),
          grconvertX(bounds[[3]] + xinch() * 0.1, to='ndc'),
          grconvertY(bounds[[4]] + yinch() * 0.1, to='ndc')
        )
        plotAnnotations[[id]][[i]]$ndcbounds <- bounds

        do.call(annotation_types[[a$type]]$result$drawFunc, args)
      })
    }
  }

  # Returns the annotation script for plot #n
  scriptFunc <- function(n, xpd=NULL) {
    jms.classes::log.debug('Compiling annotation script for plot %s', n)
    id = paste0('ano_', n)
    if(! id %in% names(plotAnnotations)) return('# Add any plot annotations here')
    scripts=c()
    anos <- plotAnnotations[[id]]
    for(i in seq_along(anos)) {
      script = ''
      a = anos[[i]]
      x = a$x
      y = a$y

      if(length(rescaleFuncs) >= n && !is.null(rescaleFuncs[[n]])) {
        script = sprintf('xy <- %s(%s, %s)\n', rescaleFuncs[[n]]$visibleToUser_name, x, y)
        x = 'xy[[1]]'
        y = 'xy[[2]]'
      }

      args <- a$args
      if(!is.null(xpd)) {
        args$xpd=xpd
      } else {
        args$xpd = a$clipping
      }
      scripts[[i]] = paste0(script, do.call(annotation_types[[a$type]]$result$script, c(list(x, y), args)))
    }
    paste0(scripts, collapse='\n')
  }

  coordinates <- shiny::reactive({
    c(x=input$x, y=input$y, n=input$n)
  })
  ndcCounter <- shiny::reactiveValues(counter=0)
  ndccoordinates <- shiny::reactive({
    force(ndcCounter$counter)
    x = input$x
    y = input$y
    n = which(plotNames() == input$n)
    xy <- replaceMissingCoordinateWithData(n, x, y)
    if(is.null(xy)) return()
    x <- xy[[1]]
    y <- xy[[2]]
    userTondc(n, x, y)
  })

  updatendc <- function(){ndcCounter$counter = shiny::isolate(ndcCounter$counter) + 1}
  preview <- function() {
    coords = ndccoordinates()
    if(is.null(coords)) return()

    x = coords[[1]]
    y = coords[[2]]
    x = grconvertX(x, from='ndc')
    y = grconvertY(y, from='ndc')

    type=input$type_select
    args=annotation_types[[type]]$result$arguments()
    do.call(annotation_types[[type]]$result$drawFunc, c(list(x, y), args))
  }

  focus <- function() {
    a <- focusedAnnotation$annotation
    if(is.null(a)) return()
    x = a$x
    y = a$y
    xy = userTondc(focusedAnnotation$n, x, y)
    x = grconvertX(xy[[1]], from='ndc')
    y = grconvertY(xy[[2]], from='ndc')

    args <- a$args

    do.call(annotation_types[[a$type]]$result$focus, c(list(x, y), args))
  }
  #### State saving ####

  # Save extra values in state$values when we bookmark
  shiny::onBookmark(function(state) {
    annotationList <- shiny::reactiveValuesToList(plotAnnotations)
    state$values$annotationList <- annotationList
  })

  # Read values from state$values when we restore
  shiny::onRestored(function(state) {
    jms.classes::log.debug("Restoring plot annotations")
    annotationList <- state$values$annotationList
    for (id in names(annotationList)) {
      plotAnnotations[[id]] <- annotationList[[id]]
    }
  })

  shiny::setBookmarkExclude(c("n_select", "x", "y", "addupdatedelete", "clear"))

  #### Return ####
  return(list(
    annotateFunc=annotateFunc,
    scriptFunc=scriptFunc,
    coordinates=coordinates,
    ndccoordinates=ndccoordinates,
    updatendc=updatendc,
    preview=preview,
    focus=focus
  ))
}
