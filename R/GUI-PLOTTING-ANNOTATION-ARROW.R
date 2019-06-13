arrow_annotation_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(
        ns("dir"), "Direction",
        c("Up", "Down", "Left", "Right"), "Down"
      )),
      shiny::column(2, shiny::numericInput(ns("len"), "Length (cm)", 1, min=0, step=0.5)),
      shiny::column(2, shiny::numericInput(ns("headlen"), "Head Length (cm)", 0.25, min=0, step=0.05)),
      shiny::column(2, shiny::numericInput(ns("lwd"), "Line width", 1, min=0, step=0.5)),
      shiny::column(2, restorableColourInput(ns("col"), "Colour", value="black"))
    )
  )
}

arrow_annotation_mod <- function(input, output, session) {
  arguments <- function() {
    return(list(
      dir=input$dir,
      len=input$len,
      headlen=input$headlen,
      lwd=input$lwd,
      col=input$col
    ))
  }

  updateInputs <- function(dir, len, headlen, lwd, col, xpd=NULL) {
    shiny::updateSelectInput(session, "dir", selected=dir)
    shiny::updateNumericInput(session, "len", value=len)
    shiny::updateNumericInput(session, "headlen", value=headlen)
    shiny::updateNumericInput(session, "lwd", value=lwd)
    colourpicker::updateColourInput(session, "col", value=col)
    return()
  }

  getCoords <- function(x, y, dir, len) {
    if (dir == "Up") {
      return(c(x, y - yinch() * len / 2.54, x, y))
    } else if (dir == "Down") {
      return(c(x, y + yinch() * len / 2.54, x, y))
    } else if (dir == "Left") {
      return(c(x + xinch() * len / 2.54, y, x, y))
    } else if (dir == "Right") {
      return(c(x - xinch() * len / 2.54, y, x, y))
    }
  }

  getBoundingRect <- function(x, y, dir, len, headlen, lwd, col, xpd=NULL) {
    # Note slightly different from coords
    if (dir == "Up") {
      return(c(x, y - yinch() * len / 2.54, x, y))
    } else if (dir == "Down") {
      return(c(x, y, x, y + yinch() * len / 2.54))
    } else if (dir == "Left") {
      return(c(x, y, x + xinch() * len / 2.54, y))
    } else if (dir == "Right") {
      return(c(x - xinch() * len / 2.54, y, x, y))
    }
  }

  drawFunc <- function(x, y, dir, len, headlen, lwd, col, xpd=NULL) {
    r <- getCoords(x, y, dir, len)
    arrows(r[[1]], r[[2]], r[[3]], r[[4]], length=headlen / 2.54, lwd=lwd, col=col, xpd=xpd)
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

  script <- function(x, y, dir, len, headlen, lwd, col, xpd=NULL) {
    xpds <- ""
    if (!is.null(xpd)) {
      xpds <- sprintf(", xpd=%s", xpd)
    }

    headlen <- round(headlen / 2.54, digits=2)
    len <- round(len / 2.54, digits=2)

    # x and y aren't necessarily numeric!
    if (dir == "Up") {
      r <- c(x, y, x, sprintf("%s + yinch() * %s", y, len))
    } else if (dir == "Down") {
      r <- c(x, y, x, sprintf("%s - yinch() * %s", y, len))
    } else if (dir == "Left") {
      r <- c(x, y, sprintf("%s - xinch() * %s", x, len), y)
    } else if (dir == "Right") {
      r <- c(x, y, sprintf("%s + xinch() * %s", x, len), y)
    }

    sprintf(
      'arrows(%s, %s, %s, %s, length=%s, lwd=%s, col="%s"%s)',
      r[[1]], r[[2]], r[[3]], r[[4]], headlen, lwd, col, xpds
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

register_plot_annotation_module("Arrow", arrow_annotation_mod_UI, arrow_annotation_mod)
