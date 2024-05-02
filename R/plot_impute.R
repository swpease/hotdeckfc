#' Plot imputation
#'
#' Plot output of [impute()].
#'
#' This is a basic plot of your data over time, with the imputations added
#' as colored lines.
#'
#' @param .imputation Output of [impute()].
#' @param .observation symbol. The observations column.
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
