label_posns <- list(
  Centred=NULL,
  Left=2,
  Right=4,
  Above=3,
  Below=1
)

label_annotation_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3, shiny::textInput(ns("label"), "Label", "Label")),
      shiny::column(3, shiny::selectInput(ns("pos"), "Position", names(label_posns))),
      shiny::column(2, shiny::numericInput(ns("cex"), "Relative Size", 1, min=0, step=0.5)),
      shiny::column(2, restorableColourInput(ns("col"), "Colour", value="black"))
    )
  )
}

label_annotation_mod <- function(input, output, session) {
  arguments <- function() {
    pos <- input$pos
    if (is.null(pos)) pos <- "Centred"
    return(list(
      label=input$label,
      pos=label_posns[[pos]],
      cex=input$cex,
      col=input$col
    ))
  }

  updateInputs <- function(label, pos, cex, col, xpd=NULL) {
    shiny::updateTextInput(session, "label", value=label)

    if (is.null(pos)) {
      pos <- "Centred"
    } else {
      options <- unlist(label_posns)
      pos <- names(options)[options == pos]
    }

    shiny::updateSelectInput(session, "pos", selected=pos)
    shiny::updateNumericInput(session, "cex", value=cex)
    colourpicker::updateColourInput(session, "col", value=col)
    return()
  }

  getBoundingRect <- function(x, y, label, pos, cex, col, xpd=NULL) {
    offset <- 0.5 * par("cxy")

    if (is.null(pos)) {
      adj <- c(0.5, 0.5)
    } else if (pos == 1) {
      adj <- c(0.5, 1)
      y <- y - offset[[2]]
    } else if (pos == 2) {
      adj <- c(1, 0.5)
      x <- x - offset[[1]]
    } else if (pos == 3) {
      adj <- c(0.5, 0)
      y <- y + offset[[2]]
    } else if (pos == 4) {
      adj <- c(0, 0.5)
      x <- x + offset[[1]]
    }

    lwidths <- strwidth(label, cex=cex)
    rwidths <- lwidths * (1 - adj[[1]])
    lwidths <- lwidths * adj[[1]]
    bheights <- strheight(label, cex=cex)
    theights <- bheights * (1 - adj[[2]])
    bheights <- bheights * adj[[2]]

    xr <- x + rwidths
    xl <- x - lwidths
    yb <- y - bheights
    yt <- y + theights
    return(c(xl, yb, xr, yt))
  }

  drawFunc <- function(x, y, label, pos, cex, col, xpd=NULL) {
    text(x, y, label, pos=pos, cex=cex, col=col, xpd=xpd)
  }

  focus <- function(...) {
    # drawFunc(...)

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

  script <- function(x, y, label, pos, cex, col, xpd=NULL) {
    xpds <- ""
    if (!is.null(xpd)) {
      xpds <- sprintf(", xpd=%s", xpd)
    }
    poss <- ""
    if (!is.null(pos)) {
      poss <- sprintf(", pos=%s", pos)
    }
    sprintf(
      'text(%s, %s, "%s"%s, cex=%s, col="%s"%s)',
      x, y, label, poss, cex, col, xpds
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

register_plot_annotation_module("Label", label_annotation_mod_UI, label_annotation_mod)
