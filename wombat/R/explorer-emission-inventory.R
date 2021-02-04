#' @export
emission_inventory_explorer_ui <- function(id, process_model) {
  ns <- shiny::NS(id)

  emissions <- process_model$control_emissions

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shinyWidgets::sliderTextInput(
        ns('month'),
        'Month:',
        choices = format(sort(unique(emissions$month_start)), '%Y-%m'),
        animate = TRUE
      ),
      shiny::sliderInput(
        ns('flux_max_abs'),
        'Max absolute flux [kg/m²/year]:',
        value = 2,
        min = 0.01,
        max = ceiling(max(abs(emissions$flux_density)))
      ),
      colourInput(ns('low_colour'), 'Low colour:', value = '#35978f'),
      colourInput(ns('high_colour'), 'High colour:', value = '#bf812d'),
      width = 3
    ),
    shiny::mainPanel(
      shiny::plotOutput(ns('emissionsMap')),
      width = 9
    )
  )
}

#' @export
emission_inventory_explorer <- function(
  input,
  output,
  session,
  process_model
) {
  emissions <- process_model$control_emissions
  output$emissionsMap <- shiny::renderPlot({
    ggplot() +
      geom_tile(
        data = emissions %>%
          filter(format(month_start, '%Y-%m') == input$month),
        mapping = aes(
          longitude,
          latitude,
          width = cell_width,
          height = cell_height,
          fill = flux_density
        )
      ) +
      geom_world() +
      scale_fill_gradient2_oob(
        low = input$low_colour,
        high = input$high_colour,
        limits = c(-input$flux_max_abs, input$flux_max_abs)
      ) +
      coord_quickmap() +
      labs(
        x = 'Longitude',
        y = 'Latitude',
        fill = expression('Flux [kg/' * m ^ 2 * '/year]')
      ) +
      xlim(-180, 180) +
      ylim(-90, 90)
  })
}
