point_annotation_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("pch"), "Type", 0:25, 16)),
      shiny::column(2, shiny::numericInput(ns("cex"), "Relative Size", 2, min=0, step=0.5)),
      shiny::column(2, shiny::numericInput(ns("lwd"), "Line width", 1, min=0, step=0.5)),
      shiny::column(2, restorableColourInput(ns("col"), "Colour", value="black")),
      shiny::column(2, restorableColourInput(ns("bg"), "Background Colour", value="black"))
    )
  )
}

point_annotation_mod <- function(input, output, session) {
  arguments <- function() {
    return(list(
      pch=as.numeric(input$pch),
      cex=input$cex,
      lwd=input$lwd,
      col=input$col,
      bg=input$bg
    ))
  }

  updateInputs <- function(pch, cex, lwd, col, bg, xpd=NULL) {
    shiny::updateSelectInput(session, "pch", selected=pch)
    shiny::updateNumericInput(session, "cex", value=cex)
    shiny::updateNumericInput(session, "lwd", value=lwd)
    colourpicker::updateColourInput(session, "col", value=col)
    colourpicker::updateColourInput(session, "bg", value=bg)
    return()
  }

  getBoundingRect <- function(x, y, pch, cex, lwd, col, bg, xpd=NULL) {
    r <- (cex * par("cxy")) / 2
    return(c(x - r[[1]], y - r[[2]], x + r[[1]], y + r[[2]]))
  }

  drawFunc <- function(x, y, pch, cex, lwd, col, bg, xpd=NULL) {
    points(x, y, pch=pch, cex=cex, lwd=lwd, col=col, bg=bg, xpd=xpd)
  }

  focus <- function(...) {
    bounds <- getBoundingRect(...)
    rect(
      bounds[[1]] - xinch() * 0.1,
      bounds[[2]] - yinch() * 0.1,
      bounds[[3]] + xinch() * 0.1,
      bounds[[4]] + yinch() * 0.1,
      border="red",
      lwd=2
    )
  }

  script <- function(x, y, pch, cex, lwd, col, bg, xpd=NULL) {
    xpds <- ""
    if (!is.null(xpd)) {
      xpds <- sprintf(", xpd=%s", xpd)
    }

    sprintf(
      'points(%s, %s, pch=%s, cex=%s, lwd=%s, col="%s", bg="%s"%s)',
      x, y, pch, cex, lwd, col, bg, xpds
    )
  }

  return(list(
    arguments=arguments,
    drawFunc=drawFunc,
    script=script,
    getBoundingRect=getBoundingRect,
    focus=focus,
    updateInputs=updateInputs
  ))
}

register_plot_annotation_module("Point", point_annotation_mod_UI, point_annotation_mod)
