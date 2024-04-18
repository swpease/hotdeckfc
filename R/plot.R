#' Plot forecast over seasonal history
#'
#' Plot forecast over the historical seasonal data.
#'
#' @param forecast tibble. Output of [hot_deck_forecast()].
#' @param historical_data tsibble. The data.
#' @param observation_col_name string. The observation column name.
#' @param filter_past_forecasts bool. Filter forecasts before `lubdridate::today()`?
#' @param forecasts_geom string. The geom to use for the forecasts on the ggplot
#' @param title string. The title.
#' @param x_lab string. The x-axis label.
#' @param y_lab string. The y-axis label.
#' @param caption string. The figure caption.
#'
#' @examples
#' data = append_lead(hotdeckfc::SUGG_temp, observation)
#' fc = hot_deck_forecast(data,
#'                        date,
#'                        observation,
#'                        times = 5,
#'                        h = 20,
#'                        window_back = 20,
#'                        window_fwd = 20,
#'                        n_closest = 5)
#' plot_forecast(fc, data)
#'
#' @export
plot_forecast = function(forecast,
                         historical_data,
                         observation_col_name = "observation",
                         filter_past_forecasts = FALSE,
                         forecasts_geom = c("line", "point"),
                         title = "Forecast",
                         x_lab = "Day of Year",
                         y_lab = "Observation",
                         caption = ggplot2::waiver()) {
  # data setup
  historical_data_index = tsibble::index(historical_data)
  historical_data = historical_data %>%
    dplyr::mutate(
      doy = lubridate::yday({{ historical_data_index }}),
      yr = lubridate::year({{ historical_data_index }})
    )
  forecast = forecast %>%
    dplyr::mutate(
      doy = lubridate::yday(datetime),
      yr = lubridate::year(datetime)
    )
  if (filter_past_forecasts) {
    forecast = forecast %>%
      dplyr::filter(datetime >= lubridate::today())
  }
  mean_fc = forecast %>%
    dplyr::group_by(doy) %>%
    dplyr::summarise(avg_pred = mean(forecast, na.rm = TRUE))
  latest_obs = historical_data %>%
    dplyr::slice_tail()

  # forecasts geom types
  forecasts_geom = match.arg(forecasts_geom)
  fcs_as_lines = ggplot2::geom_line(
    data = forecast,
    mapping = ggplot2::aes(
      x = doy,
      y = forecast,
      color = as.factor(simulation_num)
    ),
    show.legend = FALSE,
    linewidth = 0.25
  )
  fcs_as_points = ggplot2::geom_point(
    data = forecast,
    mapping = ggplot2::aes(
      x = doy,
      y = forecast,
      color = as.factor(simulation_num)
    ),
    show.legend = FALSE,
    alpha = 0.3,
    size = 0.25
  )
  fc_geom = if (forecasts_geom == "line") fcs_as_lines else fcs_as_points

  # plotting
  ggplot2::ggplot() +
    fc_geom +
    ggplot2::geom_line(
      data = historical_data,
      mapping = ggplot2::aes(
        x = doy,
        y = .data[[observation_col_name]],
        group = as.factor(yr)
      ),
      show.legend = FALSE,
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      data = latest_obs,
      mapping = ggplot2::aes(
        x = doy,
        y = .data[[observation_col_name]]
      ),
      color = "red"
    ) +
    ggplot2::geom_line(
      data = mean_fc,
      mapping = ggplot2::aes(
        x = doy,
        y = avg_pred
      ),
      color = "red"
    ) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::labs(caption = caption) +
    ggplot2::ggtitle(title)
}


#' Shiny forecast plotter
#'
#' Widget to play around with forecasting parameters.
#'
#' Currently, this widget doesn't allow for vectors to be passed to those
#' forecasting parameters that can take vectors (`window_*`, `n_closest`).
#'
#' @param .data tsibble. The data.
#' @param .datetime symbol. The datetime column of `.data`.
#' @param .observation symbol. The observation column of `.data`.
#' @param samplers list. Any samplers you want to have to option to use.
#' @param covariate_forecasts optional tsibble.
#'   Simulated sample paths of covariates.
#' @param title optional string. Plot title.
#'
#' @examples
#' data = append_lead(hotdeckfc::SUGG_temp, observation)
#' #shiny_visualize_forecast(data, date, observation)
#'
#'
#' @export
shiny_visualize_forecast <- function(.data,
                                     .datetime,
                                     .observation,
                                     samplers = list(lead = sample_lead(),
                                                     diff = sample_diff()),
                                     covariate_forecasts = NULL,
                                     title = ggplot2::waiver()) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Widget to play around with forecasting parameters."),
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        shiny::selectInput(inputId = "sampler",
                    label = "sampler",
                    choices = names(samplers),
                    selected = names(samplers)[[1]]),
        shiny::numericInput(inputId = "times",
                     label = "times",
                     min = 1,
                     max = 1000,
                     value = 5,
                     step = 1),
        shiny::numericInput(inputId = "h",
                     label = "h",
                     min = 1,
                     max = 1000,
                     value = 30,
                     step = 1),
        shiny::numericInput(inputId = "window_back",
                     label = "window_back",
                     min = 1,
                     max = 1000,
                     value = 20,
                     step = 1),
        shiny::numericInput(inputId = "window_fwd",
                     label = "window_fwd",
                     min = 1,
                     max = 1000,
                     value = 20,
                     step = 1),
        shiny::numericInput(inputId = "n_closest",
                     label = "n_closest",
                     min = 1,
                     max = 1000,
                     value = 5,
                     step = 1),
        shiny::selectInput(inputId = "fc_geom_type",
                    label = "Forecast paths display type",
                    choices = c("line", "point"),
                    selected = "line",),
        shiny::actionButton(inputId = "recalculate",
                     label = "Recalculate",)
      ),
      mainPanel = shiny::mainPanel(
        shiny::plotOutput(
          outputId = "forecastPlot",
          height = "450px",
        )
      )
    )
  )

  # Define server logic ----
  server <- function(input, output, session) {
    counter = shiny::reactiveVal(value = 0, label = "counter")  # for caching

    # Reset counter on fc param change
    shiny::observe({ counter(0) }) %>%
      shiny::bindEvent(input$sampler,
                input$times,
                input$h,
                input$window_back,
                input$window_fwd,
                input$n_closest)

    shiny::observe({
      new_val = counter() + 1
      counter(new_val)  # triggers this reactive, `counter`
    }) %>%
      shiny::bindEvent(input$recalculate)

    forecast = shiny::reactive({
      hot_deck_forecast(.data,
                        .datetime = {{ .datetime }},
                        .observation = {{ .observation }},
                        times = input$times,
                        h = input$h,
                        window_back = input$window_back,
                        window_fwd = input$window_fwd,
                        n_closest = input$n_closest,
                        sampler = samplers[[input$sampler]],
                        covariate_forecasts = covariate_forecasts)
    }) %>%
      shiny::bindCache(names(.data),
                       dim(.data),
                       input$times,
                       input$h,
                       input$window_back,
                       input$window_fwd,
                       input$n_closest,
                       input$sampler,
                       covariate_forecasts,
                       counter())

    output$forecastPlot <- shiny::renderPlot({
      fc = forecast()
      plot_forecast(forecast = fc,
                    historical_data = .data,
                    observation_col_name = rlang::as_name(rlang::ensym(.observation)),
                    forecasts_geom = input$fc_geom_type, title = title,
                    filter_past_forecasts = FALSE)
    }) %>%
      shiny::bindCache(names(.data),
                dim(.data),
                input$times,
                input$h,
                input$window_back,
                input$window_fwd,
                input$n_closest,
                input$sampler,
                covariate_forecasts,
                input$fc_geom_type,
                counter())
  }

  shiny::shinyApp(ui = ui, server = server)
}


#' Plot CRPS of CV
#'
#' Plot the CRPS'es of cross validation output.
#'
#' @param arg_list list. The `$arg_list` element in grid search output.
#' @param cv_crps_out The output of [calc_cv_crps()] applied to the relevant
#'   grid search output's element.
#' @param ymax optional numeric. The plots' y max.
plot_cv_crps <- function(arg_list, cv_crps_out, ymax = NULL) {
  plt = cv_crps_out %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = h,
                    y = score,
                    color = as.factor(k))
    ) +
    ggplot2::ggtitle(paste("wf", sum(arg_list$window_fwd),
                  "wb", sum(arg_list$window_back),
                  "nc", arg_list$n_closest,
                  "sa_name", arg_list$sa_name)) +
    ggplot2::guides(color=ggplot2::guide_legend(title="k")) +
    ggplot2::ylab("CRPS")
  plt = if (is.null(ymax)) plt else {
    plt + ggplot2::ylim(0, ymax)
  }
  show(plt)
}


#' Plot CRPS of grid search
#'
#' Plot the CRPS'es of grid search output along h.
#'
#' The title of the plot is the various parameter values, where
#'   - "wf" = window_forward
#'   - "wb" = window_back
#'   - "nc" = n_closest
#'   - "sa_name" = sa_name
#' For the first wf, wb, and nc, if you provided a vector, then the
#' sum of that vector is used as the value.
#'
#' @param grid_out output from [grid_search_hot_deck_cv()].
#' @param .observation symbol. The observation column name from your data.
#' @param ymax optional numeric. The plots' y max.
#'
#' @examples
#' grid = build_grid(
#'   h = 2,
#'   n_closest = c(5, 10),
#'   # even if you only use a single `build_window_args()`, wrap in `list()`
#'   window_args = list(build_window_args(20)),
#'   # also need to wrap in list
#'   sampler_args = list(
#'     build_sampler_args(sa_name = "nxt",
#'                        sampler = sample_lead(),  # remember to call!
#'                        appender = append_lead)
#'   )
#' )
#' out = grid_search_hot_deck_cv(hotdeckfc::SUGG_temp,
#'                               .datetime = date,
#'                               .observation = observation,
#'                               grid = grid)
#' plot_grid_search_crps(out, observation, 5)
#'
#' @export
plot_grid_search_crps <- function(grid_out, .observation, ymax = NULL) {
  gs_crps = grid_out %>%
    purrr::map(\(x) c(x, list(crps = calc_cv_crps(x$cv_out, rlang::as_name(rlang::ensym(.observation))))))
  gs_crps %>% purrr::walk(\(x) plot_cv_crps(x$arg_list, x$crps, ymax = ymax))
}
