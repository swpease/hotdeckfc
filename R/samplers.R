#' Generate a hot deck forecast value using `lead`s.
#'
#' Given a `local_rows` and `current_obs`, it:
#'   - Finds those rows in `local_rows` with observations closest to `current_obs`.
#'   - Takes the `n_closest` of those.
#'   - Randomly samples one of those.
#'   - Returns that sample's "tomorrow" observation as both
#'     the next `current_obs` and the `forecast`.
#'
#' For `n_closest`, `dplyr::slice_min` is used, and tie values are included.
#'
#' `n_bins` works as follows:
#'   - if == 0, operate on entirety of `local_rows`
#'   - if >= 1, one half of the local_rows (either the non-neg or non-pos offsets)
#'   is selected and split evenly into `n_bins`. Any remainder r adds 1 to
#'   the r nearest-to-0 bins' range.
#'
#' @param next_obs_col_name Name of the column in your data containing the next
#' observation (from, e.g. `dplyr::lead`).
#' @param n_bins Number of bins to use. See details for... details.
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration.
#' forecast = The forecasted value.
#'
#' @export
hot_deck_lead_sampler <- function(next_obs_col_name, n_bins = 0) {
  purrr::partial(internal_hot_deck_lead_sampler, ... =, next_obs_col_name, n_bins)
}


#' Wrapped method for `hot_deck_lead_sampler`.
#'
#' @param local_rows tibble (NOT tsibble) The `local_rows` in `simulate_sample_path`.
#' @param .observation The observation column.
#' @param current_obs The `current_obs` in `simulate_sample_path`.
#' @param n_closest Scalar.
#' @param next_obs_col_name Name of the column in your data containing the next
#' observation (from, e.g. `dplyr::lead`).
#' @param n_bins Number of bins to use. See details for... details.
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration,
#'                   i.e. the next forecast.
#' forecast = The forecasted value.
internal_hot_deck_lead_sampler <- function(local_rows,
                                            .observation,
                                            current_obs,
                                            n_closest,
                                            next_obs_col_name,
                                            n_bins) {
  rand_row = local_rows %>% sample_local_rows({{ .observation }},
                                              current_obs = current_obs,
                                              n_closest = n_closest,
                                              derived_col_name = next_obs_col_name,
                                              n_bins = n_bins)
  # What was tomorrow's obs for our randomly selected row that had
  # a similar obs to our current obs?
  # That's our new current obs, as well as our forecast.
  new_current_obs = rand_row %>% dplyr::pull(.data[[next_obs_col_name]])

  list(
    new_current_obs = new_current_obs,
    forecast = new_current_obs
  )
}


#' Generate a hot deck forecast value using `lead`s of `difference`s.
#'
#' Given a `local_rows` and `current_obs`, it:
#'   - Finds those rows in `local_rows` with observations closest to `current_obs`.
#'   - Takes the `n_closest` of those.
#'   - Randomly samples one of those.
#'   - Adds that sample's difference-to-its-tomorrow to the current observation.
#'   - Returns this value as both the next `current_obs` and the `forecast`.
#'
#' For `n_closest`, `dplyr::slice_min` is used, and tie values are included.
#'
#' `n_bins` works as follows:
#'   - if == 0, operate on entirety of `local_rows`
#'   - if >= 1, one half of the local_rows (either the non-neg or non-pos offsets)
#'   is selected and split evenly into `n_bins`. Any remainder r adds 1 to
#'   the r nearest-to-0 bins' range.
#'
#' @param diff_to_next_obs_col_name Name of the column in your data containing the next
#' observation (from, e.g. `diff_mutator`).
#' @param n_bins Number of bins to use. See details for... details.
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration.
#' forecast = The forecasted value.
#'
#' @export
hot_deck_diff_sampler <- function(diff_to_next_obs_col_name = "diff_to_next_obs", n_bins = 0) {
  purrr::partial(internal_hot_deck_diff_sampler, ... =, diff_to_next_obs_col_name, n_bins)
}


#' Wrapped method for `hot_deck_diff_sampler`.
#'
#' @param local_rows tibble (NOT tsibble) The `local_rows` in `simulate_sample_path`.
#' @param .observation The observation column.
#' @param current_obs The `current_obs` in `simulate_sample_path`.
#' @param n_closest Scalar.
#' @param diff_to_next_obs_col_name Name of the column in your data containing
#' the diff to the next observation (from, e.g. `diff_mutator`).
#' @param n_bins Number of bins to use. See details for... details.
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration.
#' forecast = The forecasted value.
internal_hot_deck_diff_sampler <- function(local_rows,
                                           .observation,
                                           current_obs,
                                           n_closest,
                                           diff_to_next_obs_col_name,
                                           n_bins) {
  rand_row = local_rows %>% sample_local_rows({{ .observation }},
                                              current_obs = current_obs,
                                              n_closest = n_closest,
                                              derived_col_name = diff_to_next_obs_col_name,
                                              n_bins = n_bins)
  # What was the difference to tomorrow's obs for our randomly selected
  # row that had a similar obs to our current obs?
  # We add that to our current obs to get our new current obs,
  # which is also our forecast for tomorrow.
  diff_to_tomorrow = rand_row %>% dplyr::pull(.data[[diff_to_next_obs_col_name]])
  new_current_obs = current_obs + diff_to_tomorrow

  list(
    new_current_obs = new_current_obs,
    forecast = new_current_obs
  )
}


#' Sample a random row from local rows.
#'
#' Procedure outline:
#'   - Bin the local rows by offset from `current_obs`
#'   - Sample one non-empty bin (i.e. not all NAs in derived_col) at random
#'   - Take the `n_closest` closest rows
#'   - Randomly sample one of these.
#'
#' The `derived_col_name` may be something like the column of leads or
#' differences, or it could be the observation column itself.
#'
#' TODO: make `derived_col_name` optional?
#'
#' `n_bins` works as follows:
#'   - if == 0, operate on entirety of `local_rows`
#'   - if >= 1, one half of the local_rows (either the non-neg or non-pos offsets)
#'   is selected and split evenly into `n_bins`. Any remainder r adds 1 to
#'   the r nearest-to-0 bins' range.
#'
#' The returned row has its `offset` column removed. This is just easier for
#' this function; it would not be difficult to include, but I don't see a reason
#' to include it; its purpose should have been fulfilled within this function.
#'
#' @param local_rows tibble (NOT tsibble) The `local_rows` in `simulate_sample_path`.
#' @param .observation The observation column.
#' @param current_obs The `current_obs` in `simulate_sample_path`.
#' @param n_closest Scalar.
#' @param derived_col_name string. The name of the column that you want to
#' filter any NAs from.
#' @param n_bins Number of bins to use. See details for... details.
#' @returns A row from `local_rows`, without the `offset` column anymore.
sample_local_rows <- function(local_rows,
                              .observation,
                              current_obs,
                              n_closest,
                              derived_col_name,
                              n_bins) {
  # Do we have something to work with?
  if (all(is.na(local_rows[[derived_col_name]]))) {
    stop("No local values to draw from for:\n", call. = FALSE)
  }

  # Currently, I do not filter out cases where obs is missing but
  # derived_col is not. dplyr's sorting puts NA's at the bottom, so if they
  # are selected, they're the last resorts, which seems reasonable to me.
  local_rows = local_rows %>%
    dplyr::mutate(obs_distance = abs(current_obs - {{ .observation }}))

  if (n_bins > 0) {
    # Getting a randomly selected half
    non_neg = sample(c(TRUE, FALSE), 1)
    if (non_neg) {
      half = local_rows %>% dplyr::filter(offset >= 0)
    } else {
      half = local_rows %>%
        dplyr::filter(offset <= 0) %>%  # yes, 0 in both cases
        dplyr::mutate(offset = abs(offset))
    }
    # Do we have something to work with in this randomly selected half?
    if (all(is.na(half[[derived_col_name]]))) {
      half_name = if (non_neg) "non-negative" else "non-positive"
      stop(paste("No local values to draw from in",
                 half_name, "offset portion of local values.\n"),
           call. = FALSE)
    }

    # Groups assg. setup
    n = max(half$offset) + 1  # no NA filters yet, so == total # of offsets in half
    # equal bin sizes to the extent possible
    sizes = rep(floor(n / n_bins), n_bins)
    rem = n %% n_bins
    # extend the rem closest-to-central bins by 1 each
    bumps = c(rep(1, rem), rep(0, (n_bins - rem)))
    sizes = sizes + bumps
    rle_rep = list(
      lengths = sizes,
      values = 1:n_bins
    )
    grps = inverse.rle(rle_rep)

    # Assigning groups
    half = half %>%
      dplyr::rowwise() %>%
      dplyr::mutate(grp = grps[[offset + 1]]) %>%  # 1-based indexing
      dplyr::ungroup()
    # *now* we filter out NAs; this precludes selecting an all-NA group.
    half = half %>% dplyr::filter(!is.na(.data[[derived_col_name]]))

    # Getting a random group
    rand_grp_num = half %>%
      dplyr::distinct(grp) %>%
      slice_sample(n = 1) %>%
      pull()
    rand_grp = half %>% dplyr::filter(grp == rand_grp_num)
  } else {  # bins == 0, so we just take all the data (minus NAs)
    rand_grp = local_rows %>%
      dplyr::filter(!is.na(.data[[derived_col_name]]))
  }

  # Getting a random row
  # TODO: if using prop, add check here.
  if (nrow(rand_grp) <= n_closest) {
    warning(paste("Selected bin does not have more values than",
                  "the closeness cut-off specifies.",
                  "\n(i.e. nrow(bin) <= n_closest)\n",
                  "Sampling from entire bin."),
            call. = FALSE)
  }
  closest_rows = rand_grp %>%
    dplyr::slice_min(order_by = obs_distance, n = n_closest)
  rand_row = closest_rows %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::select(-dplyr::any_of(c("offset", "obs_distance", "grp")))
    # ^ removing internal cols, plus (TB settled on) "offset"

  rand_row
}
