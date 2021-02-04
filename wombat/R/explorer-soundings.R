#' @export
sounding_explorer_ui <- function(id, measurement_model) {
  sliderInput <- shiny::sliderInput

  ns <- shiny::NS(id)

  soundings <- measurement_model$soundings
  xco2_range <- range(soundings$xco2)
  xco2_range[1] <- floor(xco2_range[1])
  xco2_range[2] <- ceiling(xco2_range[2])

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
        ns('xco2_range'),
        'XCO2 range:',
        min = xco2_range[1],
        max = xco2_range[2],
        value = xco2_range,
        step = 0.1
      ),
      width = 3
    ),
    shiny::mainPanel(
      shiny::plotOutput(ns('soundingMap1')),
      shiny::plotOutput(ns('soundingMap2')),
      width = 9
    )
  )
}

#' @export
sounding_explorer <- function(input, output, session, measurement_model) {
  reactive <- shiny::reactive
  renderPlot <- shiny::renderPlot

  soundings <- measurement_model$soundings

  end_date <- reactive(input$start_date + days(input$days_to_include))
  sounding_window <- reactive({
    soundings %>%
      filter(
        operation_mode %in% input$operation_mode,
        observation_time >= input$start_date,
        observation_time <= end_date()
      )
  })

  output$soundingMap1 <- renderPlot({
    ggplot(
      sounding_window(),
      aes(observation_longitude, observation_latitude, colour = xco2)
    ) +
      geom_world() +
      geom_point(
        position = position_jitter(width = 0.5, height = 0.5),
        size = 0.75
      ) +
      coord_quickmap() +
      scale_colour_wes_palette_c(limits = input$xco2_range) +
      labs(x = 'Longitude', y = 'Latitude', colour = 'XCO2') +
      ggtitle(sprintf(
        'XCO2 soundings between %s and %s',
        input$start_date,
        end_date()
      ))
  })

  output$soundingMap2 <- renderPlot({
    df_average <- sounding_window() %>%
      group_by(model_longitude, model_latitude) %>%
      summarise(xco2_mean = mean(xco2))

    ggplot(df_average, aes(model_longitude, model_latitude, fill = xco2_mean)) +
      geom_world() +
      geom_tile() +
      coord_quickmap() +
      scale_fill_wes_palette_c(limits = input$xco2_range) +
      labs(x = 'Longitude', y = 'Latitude', fill = 'XCO2') +
      ggtitle(sprintf(
        'Grid cell averaged XCO2 between %s and %s',
        input$start_date,
        end_date()
      ))
  })
}
