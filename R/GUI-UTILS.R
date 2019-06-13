NotificationStyle <- function() {
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(
        ".shiny-notification {height: 50px; width: 400px;}
         #shiny-notification-panel {top: 0; width: 400px; right: 5px;}"
      )
    ),
    shiny::tags$head(
      shiny::tags$style(
        ".shiny-notification-error {position: fixed; top: 5%; left: 0; right: 0;
                                    margin: auto; height: 200px; opacity: 1;
                                    text-align: center;}"
      )
    ),
    shiny::tags$head(
      shiny::tags$style(
        ".shiny-notification-warning {position: fixed; top: 5%; left: 0; right: 0;
                                      margin: auto; height: 200px; opacity: 1;
                                      text-align: center;}"
      )
    )
  )
}

gui_try_show_error <- function(expr, message_verb) {
  jms.classes::tryCatchST(expr, error=function(e, st) {
    # N.b. might not work with recent shiny
    # https://github.com/rstudio/shiny/issues/2096
    st <- paste0(shiny::formatStackTrace(st), collapse="\n")
    jms.classes::log.error("Error whilst %s: %s\n%s", message_verb, e, st)
    shiny::showNotification(
      shiny::div(
        htmltools::HTML(
          "<b>Error: Something went wrong whilst ",
          message_verb, ":</b><br/>", e$message
        )
      ),
      duration=10,
      type="error", id="error"
    )
    # stop(e)
    return()
  })
}

hash_line_length <- 50
hash_line <- function(n) paste0(rep_len("#", n), collapse="")
hash_header <- function(x) {
  l <- nchar(x)
  lpad <- ceiling((hash_line_length - l - 2) / 2)
  rpad <- hash_line_length - lpad - l - 2
  paste0(hash_line(lpad), " ", x, " ", hash_line(rpad))
}


restorableColourInput <- function(inputId, ..., value="white") {
  value <- shiny::restoreInput(id=inputId, default=value)
  colourpicker::colourInput(inputId, ..., value=value)
}


disconnected_style <- '
div#shiny-disconnected-overlay {
  opacity: 0.6;
}
div#shiny-disconnected-overlay:before {
	content: "Disconnected from server";
	text-align: center;
	font-size: 50pt;
	background-color: #ffd600;
	color: #0000ff;
	padding: 5%;
	width: 100%;
	position: absolute;
	top: 50%;
	transform: translateY(-50%);
}'
