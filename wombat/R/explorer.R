#' @export
flux_explorer <- function(measurement_model, process_model) {
  if (!requireNamespace('shiny', quietly = TRUE)) {
    stop('Please install the package `shiny\' to use this function')
  }

  tabPanel <- shiny::tabPanel
  callModule <- shiny::callModule

  ui <- shiny::navbarPage(
    'Flux Explorer',
    # tabPanel('Soundings', sounding_explorer_ui(
    #   'sounding_explorer', measurement_model
    # )),
    tabPanel('Control', control_explorer_ui(
      'control_explorer', process_model
    )),
    tabPanel('Emission inventory', emission_inventory_explorer_ui(
      'emission_inventory_explorer', process_model
    )),
    tabPanel('Sensitivities', sensitivity_explorer_ui(
      'sensitivity_explorer', process_model
    ))
    # tabPanel('Anomalies', anomaly_explorer_ui(
    #   'anomaly_explorer', measurement_model, process_model
    # )),
    # tabPanel('OSSE', osse_explorer_ui(
    #   'osse_explorer', measurement_model, process_model
    # ))
  )

  server <- function(input, output, session) {
    # callModule(
    #   sounding_explorer, 'sounding_explorer',
    #   measurement_model
    # )
    callModule(
      control_explorer, 'control_explorer',
      process_model
    )
    callModule(
      emission_inventory_explorer, 'emission_inventory_explorer',
      process_model
    )
    callModule(
      sensitivity_explorer, 'sensitivity_explorer',
      process_model
    )
    # callModule(
    #   anomaly_explorer, 'anomaly_explorer',
    #   measurement_model, process_model
    # )
    # callModule(
    #   osse_explorer, 'osse_explorer',
    #   measurement_model, process_model, transcom_boundary
    # )
  }

  shiny::shinyApp(ui = ui, server = server)
}
