#' Plot imputation
#'
#' Plot output of [hot_deck_impute()].
#'
#' This is a basic plot of your data over time, with the imputations added
#' as colored lines.
#'
#' @param .imputation Output of [hot_deck_impute()].
#' @param .observation symbol. The observations column.
#'
#' @examples
#' 3
#' # imputed = hot_deck_impute(hotdeckts::SUGG_temp %>%
#' #   tsibble::as_tsibble(), observation)
#' # plot_imputation(imputed, observation)
#'
#' @export
plot_imputation <- function(.imputation, .observation) {
  x_axis = tsibble::index(.imputation)
  # b/c tsibble, keeps index col
  imputations = .imputation %>%
    dplyr::select(dplyr::starts_with("imputation_")) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(!{{ x_axis }},
                        names_to = "imputation_num",
                        values_to = "imputed_obs")

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = imputations,
      mapping = ggplot2::aes(
        y = imputed_obs,
        x = {{ x_axis }},
        color = imputation_num
      ),
      show.legend = FALSE,
      alpha = 0.6,
      linewidth = 0.3
    ) +
    ggplot2::geom_line(
      data = .imputation,
      mapping = ggplot2::aes(
        y = {{ .observation }},
        x = {{ x_axis }}
      )
    )
}


#' Plot imputations with [feasts::gg_season()].
#'
#' Plot output of [hot_deck_impute()] using [feasts::gg_season()].
#'
#' Each imputation, plus the original observations, are plotted
#' separately (but in one figure) using [feasts::gg_season()].
#'
#' @inheritParams plot_imputation
#'
#' @examples
#' 3
#' # imputed = hot_deck_impute(hotdeckts::SUGG_temp %>%
#' #   tsibble::as_tsibble(), observation)
#' # plot_imputation_gg_season(imputed, observation)
#'
#' @export
plot_imputation_gg_season <- function(.imputation, .observation) {
  if (!requireNamespace("feasts", quietly = TRUE)) {
    stop(
      "Package \"feasts\" must be installed to use this function.",
      call. = FALSE
    )
  }

  long_imputed = .imputation %>%
    tidyr::pivot_longer(c({{ .observation }}, dplyr::starts_with("imputation_"))) %>%
    tsibble::as_tsibble(key = name)
  feasts::gg_season(long_imputed, value)
}


#' Plot imputations with seasonal facets
#'
#' Plot output of [hot_deck_impute()], faceted by year.
#'
#' This plotting function spreads the data out more, compared to
#' [plot_imputation()], so it's easier to
#' get a feel for how the imputations look. The imputed sections
#' are the divergent colored lines, while non-imputed areas (and also any
#' areas that happen to have all identical imputations) are purple-ish.
#'
#' @inheritParams plot_imputation
#'
#' @examples
#' 3
#' # imputed = hot_deck_impute(hotdeckts::SUGG_temp %>%
#' #   tsibble::as_tsibble(), observation)
#' # plot_imputation_seasonal_facet(imputed, observation)
#'
#' @export
plot_imputation_seasonal_facet <- function(.imputation, .observation) {
  obs_col_name = rlang::as_string(rlang::ensym(.observation))
  date_col = tsibble::index(.imputation)

  long_imputed = .imputation %>%
    dplyr::select(dplyr::starts_with("imputation_")) %>%
    tidyr::pivot_longer(dplyr::starts_with("imputation_"))

  long_imputed = long_imputed %>%
    dplyr::mutate(
      doy = lubridate::yday({{ date_col }}),
      yr = lubridate::year({{ date_col }})
    )

  ggplot2::ggplot(data = long_imputed) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = doy,
        y = value,
        color = as.factor(name),
      ),
      alpha = 0.6,
      show.legend = FALSE,
    ) +
    ggplot2::facet_wrap(~ yr) +
    ggplot2::ylab(obs_col_name) +
    ggplot2::xlab("Day of Year")
}


#' Plot imputations seasonally separately
#'
#' Plot output of [hot_deck_impute()], one figure per year.
#'
#' This plotting function spreads the data out even more than
#' [plot_imputation_seasonal_facet()], compared to [plot_imputation()],
#' The imputed sections
#' are the divergent colored lines, while non-imputed areas (and also any
#' areas that happen to have all identical imputations) are purple-ish.
#'
#' @inheritParams plot_imputation
#' @param ymin optional integer. ymin of the plots.
#' @param ymax optional integer. ymax of the plots.
#'
#' @examples
#' 3
#' # imputed = hot_deck_impute(hotdeckts::SUGG_temp %>%
#' #   tsibble::as_tsibble(), observation)
#' # plot_imputation_seasonal_separate(imputed, observation)
#'
#' @export
plot_imputation_seasonal_separate <- function(.imputation, .observation, ymin = 0, ymax = NULL) {
  date_col = tsibble::index(.imputation)

  long_imputed = .imputation %>%
    dplyr::select(dplyr::starts_with("imputation_")) %>%
    tidyr::pivot_longer(dplyr::starts_with("imputation_"))

  long_imputed = long_imputed %>%
    dplyr::mutate(
      doy = lubridate::yday({{ date_col }}),
      yr = lubridate::year({{ date_col }})
    )

  max_obs = long_imputed %>% dplyr::pull(value) %>% max()
  ymax = if (is.null(ymax)) max_obs else ymax

  yrs = long_imputed %>% dplyr::distinct(yr) %>% dplyr::pull()

  for (yr in yrs) {
    yr_data = long_imputed %>% dplyr::filter(yr == .env$yr)
    plt = ggplot2::ggplot(data = yr_data) +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          x = doy,
          y = value,
          color = as.factor(name),
        ),
        alpha = 0.6,
        show.legend = FALSE,
      ) +
      ggplot2::xlim(0, 366) +
      ggplot2::ylim(ymin, ymax) +
      ggplot2::ggtitle(yr)
    show(plt)
  }
}
