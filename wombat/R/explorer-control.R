#' @export
control_explorer_ui <- function(id, process_model) {
  ns <- shiny::NS(id)

  control <- process_model$control_mole_fraction

  co2_range <- range(control$co2)
  co2_range[1] <- floor(co2_range[1])
  co2_range[2] <- ceiling(co2_range[2])

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput(
        ns('start_date'),
        'Start date:',
        value = date(min(control$time)),
        min = date(min(control$time)),
        max = date(max(control$time)),
        step = 1
      ),
      shiny::sliderInput(
        ns('days_to_include'),
        'Days to include:',
        value = 30,
        min = 1,
        max = 60,
        step = 1
      ),
      shiny::sliderInput(
        ns('co2_range'),
        'CO2 range [ppm]:',
        min = co2_range[1],
        max = co2_range[2],
        value = co2_range,
        step = 0.1
      ),
      width = 3
    ),
    shiny::mainPanel(
      shiny::plotOutput(ns('controlMap')),
      width = 9
    )
  )
}

#' @export
control_explorer <- function(
  input,
  output,
  session,
  process_model
) {
  reactive <- shiny::reactive

  control <- process_model$control_mole_fraction

  end_date <- reactive(input$start_date + days(input$days_to_include))
  control_window <- reactive({
    control %>%
      filter(
        time >= input$start_date,
        time <= end_date()
      )
  })

  output$controlMap <- shiny::renderPlot({
    ggplot(
      control_window(),
      aes(longitude, latitude, colour = co2)
    ) +
      geom_world() +
      geom_point() +
      coord_quickmap() +
      scale_colour_wes_palette_c(limits = input$co2_range) +
      labs(x = 'Longitude', y = 'Latitude', colour = 'CO2 [ppm]') +
      ggtitle(sprintf(
        'Control CO2 between %s and %s',
        input$start_date,
        end_date()
      ))
  })
}
