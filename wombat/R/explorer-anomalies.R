#' @export
anomaly_explorer_ui <- function(id, measurement_model, process_model) {
  sliderInput <- shiny::sliderInput

  ns <- shiny::NS(id)

  soundings <- measurement_model$soundings
  anomalies <- (
    soundings$xco2
    - as.vector(measurement_model$C %*% process_model$control$xco2)
  )

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      sliderInput(
        ns('start_date'),
        'Start date:',
        value = date(min(soundings$observation_time)),
        min = date(min(soundings$observation_time)),
        max = date(max(soundings$observation_time)),
        step = 1
      ),
      sliderInput(
        ns('days_to_include'),
        'Days to include:',
        value = 30,
        min = 1,
        max = 60,
        step = 1
      ),
      shiny::checkboxGroupInput(
        ns('operation_mode'),
        'Operation mode:',
        sort(unique(soundings$operation_mode)),
        selected = sort(unique(soundings$operation_mode))
      ),
      sliderInput(
        ns('anomaly_max_abs'),
        'Max absolute anomaly:',
        value = 4,
        min = 0.01,
        max = ceiling(max(abs(anomalies)))
      ),
      colourInput(ns('low_colour'), 'Low colour:', value = '#35978f'),
      colourInput(ns('high_colour'), 'High colour:', value = '#bf812d'),
      width = 3
    ),
    shiny::mainPanel(
      shiny::plotOutput(ns('anomalyMap1')),
      shiny::plotOutput(ns('anomalyMap2')),
      width = 9
    )
  )
}

#' @export
anomaly_explorer <- function(
  input,
  output,
  session,
  measurement_model,
  process_model
) {
  reactive <- shiny::reactive
  renderPlot <- shiny::renderPlot

  soundings <- measurement_model$soundings %>%
    mutate(
      xco2_anomaly = (
        xco2
        - as.vector(measurement_model$C %*% process_model$control$xco2)
      )
    )

  end_date <- reactive(input$start_date + days(input$days_to_include))
  anomalies_window <- reactive({
    soundings %>%
      filter(
        operation_mode %in% input$operation_mode,
        observation_time >= input$start_date,
        observation_time <= end_date()
      )
  })

  output$anomalyMap1 <- renderPlot({
    ggplot(
      anomalies_window(),
      aes(observation_longitude, observation_latitude, colour = xco2_anomaly)
    ) +
      geom_world() +
      geom_point(
        position = position_jitter(width = 0.5, height = 0.5),
        size = 0.75
      ) +
      coord_quickmap() +
      annotate(
        'text',
        x = 170,
        y = 80,
        label = sprintf(
          'min = %.1f\nmax = %.1f',
          min(anomalies_window()$xco2_anomaly),
          max(anomalies_window()$xco2_anomaly)
        )
      ) +
      scale_colour_gradient2(
        low = input$low_colour,
        high = input$high_colour,
        limits = c(-1, 1) * input$anomaly_max_abs,
        oob = scales::squish
      ) +
      labs(x = 'Longitude', y = 'Latitude', colour = 'XCO2 anomaly') +
      ggtitle(sprintf(
        'XCO2 anomalies between %s and %s',
        input$start_date,
        end_date()
      ))
  })

  output$anomalyMap2 <- renderPlot({
    df_average <- anomalies_window() %>%
      group_by(model_longitude, model_latitude) %>%
      summarise(xco2_anomaly_mean = mean(xco2_anomaly))

    ggplot(
      df_average,
      aes(model_longitude, model_latitude, fill = xco2_anomaly_mean)
    ) +
      geom_world() +
      geom_tile() +
      coord_quickmap() +
      annotate(
        'text',
        x = 170,
        y = 80,
        label = sprintf(
          'min = %.1f\nmax = %.1f',
          min(df_average$xco2_anomaly_mean),
          max(df_average$xco2_anomaly_mean)
        )
      ) +
      scale_fill_gradient2(
        low = input$low_colour,
        high = input$high_colour,
        limits = c(-1, 1) * input$anomaly_max_abs,
        oob = scales::squish
      ) +
      labs(x = 'Longitude', y = 'Latitude', fill = 'XCO2 anomaly') +
      ggtitle(sprintf(
        'Grid cell averaged XCO2 anomalies between %s and %s',
        input$start_date,
        end_date()
      ))
  })
}
