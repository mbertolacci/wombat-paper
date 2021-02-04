#' @export
osse_explorer_ui <- function(id, base_measurement_model, base_process_model) {
  actionButton <- shiny::actionButton
  tabPanel <- shiny::tabPanel
  sliderInput <- shiny::sliderInput
  splitLayout <- shiny::splitLayout
  numericInput <- shiny::numericInput
  plotOutput <- shiny::plotOutput

  ns <- shiny::NS(id)

  soundings <- base_measurement_model$soundings
  times <- soundings$model_time
  month_starts <- sort(unique(to_month_start(soundings$model_time)))

  emissions <- base_process_model$emissions

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::div(
        actionButton(
          ns('regenerate_process'),
          'Regenerate process'
        ),
        actionButton(
          ns('regenerate_measurements'),
          'Regenerate measurements'
        ),
        style = 'margin-bottom: 1em'
      ),
      shiny::tabsetPanel(
        tabPanel(
          'Display',
          shinyWidgets::sliderTextInput(
            ns('emissions_month'),
            'Emissions month:',
            choices = format(month_starts, '%Y-%m')
          ),
          sliderInput(
            ns('months_to_include'),
            'Months of emissions to include:',
            value = 2,
            min = 1,
            max = 5,
            step = 1
          ),
          sliderInput(
            ns('start_date'),
            'Process/measurement start date:',
            value = date(min(times)),
            min = date(min(times)),
            max = date(max(times)),
            step = 1
          ),
          sliderInput(
            ns('days_to_include'),
            'Process/measurement days to include:',
            value = 15,
            min = 1,
            max = 60,
            step = 1
          ),
          shiny::checkboxGroupInput(
            ns('operation_mode'),
            'Operation mode:',
            c('LG', 'LN', 'OG'),
            selected = c('LG', 'LN', 'OG')
          ),
          sliderInput(
            ns('flux_max_abs'),
            'Max absolute flux anomaly:',
            value = 1,
            min = 0.01,
            max = ceiling(max(abs(emissions$flux)))
          ),
          sliderInput(
            ns('anomaly_max_abs'),
            'Max absolute anomaly:',
            value = 4,
            min = 0.01,
            max = 20
          ),
          colourInput(ns('low_colour'), 'Low colour:', value = '#35978f'),
          colourInput(ns('high_colour'), 'High colour:', value = '#bf812d')
        ),
        tabPanel(
          'OSSE parameters',
          sliderInput(
            ns('a'),
            'a:',
            value = 0.9,
            min = 0,
            max = 1
          ),
          sliderInput(
            ns('alpha_sd'),
            'alpha stdev:',
            value = 0.2,
            min = 0.01,
            max = 10
          ),
          sliderInput(
            ns('eta_sd'),
            'eta stdev:',
            value = 1,
            min = 0.01,
            max = 10
          ),
          splitLayout(
            'Attenuations:',
            numericInput(
              ns('gamma_LG'),
              'LG:',
              value = 0.8,
              min = 0,
              max = 2
            ),
            numericInput(
              ns('gamma_LN'),
              'LN:',
              value = 1,
              min = 0,
              max = 2
            ),
            numericInput(
              ns('gamma_OG'),
              'OG:',
              value = 1.2,
              min = 0,
              max = 2
            )
          ),
          splitLayout(
            'Biases:',
            numericInput(
              ns('beta_LG'),
              'LG:',
              value = 0,
              min = -2,
              max = 2
            ),
            numericInput(
              ns('beta_LN'),
              'LN:',
              value = 0,
              min = -2,
              max = 2
            ),
            numericInput(
              ns('beta_OG'),
              'OG:',
              value = 0,
              min = -2,
              max = 2
            )
          ),
        )
      ),
      width = 3
    ),
    shiny::mainPanel(
      splitLayout(
        plotOutput(ns('processSampleMap')),
        plotOutput(ns('measurementSampleMap'))
      ),
      plotOutput(ns('emissionsSampleMap')),
      width = 9
    )
  )
}

#' @export
osse_explorer <- function(
  input, output, session,
  base_measurement_model, base_process_model, transcom_boundary
) {
  reactive <- shiny::reactive
  renderPlot <- shiny::renderPlot

  process_model <- reactive({
    out <- base_process_model
    out$a <- input$a
    out$w <- 1 / input$alpha_sd ^ 2
    out$eta_prior_precision <- Diagonal(
      ncol(base_process_model$Psi),
      1 / input$eta_sd ^ 2
    )
    out
  })

  measurement_model <- reactive({
    out <- base_measurement_model
    out$beta <- c(input$beta_LG, input$beta_LN, input$beta_OG)
    out$gamma <- c(input$gamma_LG, input$gamma_LN, input$gamma_OG)
    out
  })

  process_sample <- reactive({
    input$regenerate_process
    generate(process_model())
  })

  measurement_sample <- reactive({
    input$regenerate_measurements
    generate(measurement_model(), process_sample())
  })

  emissions_sample <- reactive({
    process_model()$emissions %>%
      mutate(flux_anomaly_sample = process_sample()$Y1_tilde)
  })

  end_date <- reactive(input$start_date + days(input$days_to_include))
  process_sample_window <- reactive({
    process_model()$control %>%
      mutate(
        xco2_anomaly_sample = process_sample()$Y2_tilde
      ) %>%
      filter(
        time >= input$start_date,
        time <= end_date()
      )
  })
  measurement_sample_window <- reactive({
    measurement_model()$soundings %>%
      mutate(
        xco2_anomaly_sample = measurement_sample()$Z2_tilde
      ) %>%
      filter(
        observation_time >= input$start_date,
        observation_time <= end_date(),
        operation_mode %in% input$operation_mode
      )
  })

  output$emissionsSampleMap <- renderPlot({
    ggplot() +
      geom_tile(
        data = emissions_sample() %>%
          filter(
            format(month_start, '%Y-%m') >= input$emissions_month,
            format(
              month_start - months(input$months_to_include)
            ) <= input$emissions_month
          ),
        mapping = aes(longitude, latitude, fill = flux_anomaly_sample)
      ) +
      geom_sf(
        data = transcom_boundary,
        fill = NA,
        colour = '#555555',
        size = 0.1
      ) +
      scale_fill_gradient2(
        low = input$low_colour,
        high = input$high_colour,
        limits = input$flux_max_abs * c(-1, 1),
        oob = scales::squish
      ) +
      labs(
        x = 'Longitude',
        y = 'Latitude',
        fill = expression('Flux [kg/' * m ^ 2 * '/year]')
      ) +
      xlim(-180, 180) +
      ylim(-90, 90) +
      facet_wrap(~ month_start, nrow = 1) +
      ggtitle('Flux anomaly')
  })

  output$processSampleMap <- renderPlot({
    df_average <- process_sample_window() %>%
      group_by(longitude, latitude) %>%
      summarise(xco2_anomaly_sample_mean = mean(xco2_anomaly_sample))

    ggplot(
      df_average,
      aes(longitude, latitude, fill = xco2_anomaly_sample_mean)
    ) +
      geom_world() +
      geom_tile() +
      coord_quickmap() +
      scale_fill_gradient2(
        low = input$low_colour,
        high = input$high_colour,
        limits = input$anomaly_max_abs * c(-1, 1),
        oob = scales::squish
      ) +
      labs(x = 'Longitude', y = 'Latitude', fill = 'XCO2') +
      ggtitle(sprintf(
        'Process anomaly averaged between %s and %s',
        input$start_date,
        end_date()
      ))
  })

  output$measurementSampleMap <- renderPlot({
    ggplot(
      measurement_sample_window(),
      aes(
        observation_longitude,
        observation_latitude,
        colour = xco2_anomaly_sample
      )
    ) +
      geom_world() +
      geom_point(
        position = position_jitter(width = 0.5, height = 0.5),
        size = 0.75
      ) +
      coord_quickmap() +
      scale_colour_gradient2(
        low = input$low_colour,
        high = input$high_colour,
        limits = input$anomaly_max_abs * c(-1, 1),
        oob = scales::squish
      ) +
      labs(x = 'Longitude', y = 'Latitude', colour = 'XCO2') +
      ggtitle(sprintf(
        'Measured anomaly between %s and %s',
        input$start_date,
        end_date()
      ))
  })
}
