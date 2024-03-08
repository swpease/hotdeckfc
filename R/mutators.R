#' Add a column of leads to data.
#'
#' This mutator should be paired with `basic_hot_deck_sampler` for use in CV.
#' It is applied to the training data after the train-test split,
#' to avoid data leakage.
#'
#' @param .data The data. A tsibble.
#' @param .observation The observation column. Passed via pipe.
#' @returns .data, augmented with a column, named `next_obs`, of leads.
#'
#' @export
lead_mutator <- function(.data, .observation) {
  .data %>% mutate(next_obs = dplyr::lead({{ .observation }}))
}


#' Add a column of differences to the next observation.
#'
#' This mutator should be paired with `NAME` for use in CV.
#' It is applied to the training data after the train-test split,
#' to avoid data leakage.
#'
#' @param .data The data. A tsibble.
#' @param .observation The observation column. Passed via pipe.
#' @returns .data, augmented with a column, named `diff_to_next_obs`,
#' of leads of differences.
#'
#' @export
diff_mutator <- function(.data, .observation) {
  .data %>%
    mutate(diff_to_next_obs = dplyr::lead(tsibble::difference({{ .observation }})))
}


#' Return data, unmodified.
#'
#' This mutator should be paired with any sampler functions that do not have
#' data leakage concerns (as with those that rely on, say, `lead` or `diff`),
#' for use in CV.
#' It is applied to the training data after the train-test split.
#'
#' @param .data The data. A tsibble.
#' @param .observation The observation column. Passed via pipe.
#' @returns .data
#'
#' @export
non_mutator <- function(.data, .observation) {
  .data
}
