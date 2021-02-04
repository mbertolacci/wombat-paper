colourInput <- function(...) {
  if (!requireNamespace('colourpicker', quietly = TRUE)) {
    shiny::textInput(...)
  } else {
    colourpicker::colourInput(...)
  }
}
