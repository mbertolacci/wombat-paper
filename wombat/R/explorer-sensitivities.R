.binary_search_bounds <- function(A, T, start_L = 1, start_R = length(A)) {
  L <- start_L - 1
  R <- start_R
  while (L < R) {
    m <- floor((L + R) / 2)
    if (A[m + 1] < T) {
      L <- m + 1
    } else {
      R <- m
    }
  }
  lower <- L

  L <- start_L - 1
  R <- start_R
  while (L < R) {
    m <- floor((L + R) / 2)
    if (A[m + 1] > T) {
      R <- m
    } else {
      L <- m + 1
    }
  }
  upper <- L - 1

  c(lower, upper) + 1
}

#' @export
sensitivity_explorer_ui <- function(id, process_model) {
  sliderInput <- shiny::sliderInput
  plotOutput <- shiny::plotOutput

  ns <- shiny::NS(id)

  regions <- process_model$regions
  names(regions) <- sprintf('Region %02d', regions)

  control_emissions <- process_model$control_emissions
  control_mole_fraction <- process_model$control_mole_fraction
  perturbations <- process_model$perturbations
  sensitivities <- process_model$sensitivities

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shinyWidgets::sliderTextInput(
        ns('month'),
        'Month:',
        choices = format(sort(unique(control_emissions$month_start)), '%Y-%m'),
        animate = TRUE
      ),
      sliderInput(
        ns('start_date'),
        'Start date:',
        value = date(min(control_mole_fraction$time)),
        min = date(min(control_mole_fraction$time)),
        max = date(max(control_mole_fraction$time)),
        step = 1
      ),
      sliderInput(
        ns('days_to_include'),
        'Days to include:',
        value = 14,
        min = 1,
        max = 60,
        step = 1
      ),
      shiny::checkboxGroupInput(
        ns('observation_type'),
        'Observation types:',
        sort(unique(control_mole_fraction$observation_type)),
        selected = sort(unique(control_mole_fraction$observation_type))
      ),
      shiny::selectInput(
        ns('region_str'),
        'Region:',
        regions
      ),
      sliderInput(
        ns('sensitivity_max_abs'),
        'Max absolute sensitivity [ppm]:',
        value = 1,
        step = 0.1,
        min = 0.01,
        max = ceiling(max(abs(sensitivities$co2_sensitivity)))
      ),
      sliderInput(
        ns('flux_max_abs'),
        'Max absolute flux [kg/m²/year]:',
        value = 2,
        min = 0.01,
        max = ceiling(max(abs(control_emissions$flux_density)))
      ),
      colourInput(ns('low_colour'), 'Low colour:', value = '#35978f'),
      colourInput(
        ns('mid_colour'),
        'Mid colour (CO2 only):',
        value = '#eeeeee'
      ),
      colourInput(ns('high_colour'), 'High colour:', value = '#bf812d'),
      width = 3
    ),
    shiny::mainPanel(
      plotOutput(ns('emissionsMap'), height = '500px'),
      plotOutput(ns('sensitivityMap'), height = '500px'),
      width = 9
    )
  )
}

#' @export
sensitivity_explorer <- function(
  input, output, session,
  process_model
) {
  reactive <- shiny::reactive
  renderPlot <- shiny::renderPlot

  control_emissions <- process_model$control_emissions
  control_mole_fraction <- process_model$control_mole_fraction
  perturbations <- process_model$perturbations %>%
    left_join(
      control_emissions %>%
        select(
          model_id,
          month_start,
          longitude,
          cell_width,
          latitude,
          cell_height
        ),
      by = 'model_id'
    )
  min_from_month_start <- process_model$sensitivities$from_month_start[1]
  sensitivities <- process_model$sensitivities %>%
    mutate(
      from_month_start_dt = as.double(
        from_month_start - min_from_month_start,
        units = 'days'
      )
    )

  end_date <- reactive(input$start_date + days(input$days_to_include))
  region <- reactive(as.integer(input$region_str))

  shiny::observe({
    shiny::updateSliderInput(
      session,
      'start_date',
      min = date(sprintf('%s-01', input$month))
    )
  })

  control_mole_fraction_window <- reactive({
    control_mole_fraction %>%
      select(
        model_id,
        observation_type,
        time,
        longitude,
        latitude
      ) %>%
      filter(
        time >= input$start_date,
        time <= end_date(),
        observation_type %in% input$observation_type
      )
  })

  sensitivities_window <- reactive({
    month_date_dt <- as.double(
      date(sprintf('%s-01', input$month)) - min_from_month_start,
      units = 'days'
    )

    region_range <- .binary_search_bounds(
      sensitivities$region,
      region()
    )
    from_month_start_range <- .binary_search_bounds(
      sensitivities$from_month_start_dt,
      month_date_dt,
      region_range[1],
      region_range[2]
    )
    sensitivities_small <- sensitivities %>% select(model_id, co2_sensitivity)
    control_mole_fraction_window() %>%
      inner_join(
        sensitivities_small[
          from_month_start_range[1] : from_month_start_range[2],
        ],
        by = 'model_id'
      )
  })

  output$emissionsMap <- renderPlot({
    ggplot() +
      geom_tile(
        data = perturbations %>%
          filter(
            region == region(),
            strftime(month_start, '%Y-%m') == input$month,
            abs(flux_density) > 0
          ),
        mapping = aes(
          longitude,
          latitude,
          width = cell_width,
          height = cell_height,
          fill = flux_density
        )
      ) +
      geom_world() +
      coord_quickmap() +
      scale_fill_gradient2_oob(
        low = input$low_colour,
        high = input$high_colour,
        limits = c(-1, 1) * input$flux_max_abs
      ) +
      labs(
        x = 'Longitude',
        y = 'Latitude',
        fill = expression('Flux [kg/' * m ^ 2 * '/year]')
      ) +
      xlim(-180, 180) +
      ylim(-90, 90) +
      theme(legend.position = 'bottom')
  })

  output$sensitivityMap <- renderPlot({
    ggplot(
      sensitivities_window(),
      aes(longitude, latitude, colour = co2_sensitivity)
    ) +
      geom_point(
        position = position_jitter(width = 0.5, height = 0.5),
        size = 1
      ) +
      geom_world() +
      coord_quickmap() +
      scale_colour_gradient2_oob(
        low = input$low_colour,
        mid = input$mid_colour,
        high = input$high_colour,
        limits = c(-1, 1) * input$sensitivity_max_abs
      ) +
      labs(x = 'Longitude', y = 'Latitude', colour = 'CO2 [ppm]') +
      theme(legend.position = 'bottom')
  })
}
