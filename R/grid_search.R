#' Grid search cross-validation of [hot_deck_forecast()].
#'
#' Perform a grid search of parameters for [hot_deck_forecast()]
#' using [cv_hot_deck_forecast()].
#'
#' Use [build_grid()] to build the tibble to pass to the `grid` argument.
#'
#' `echo` prints progress like: "Finished CV 3 of 8.".
#'
#' @param .data tsibble. The data.
#' @param .datetime symbol. The datetime column of .data.
#' @param .observation symbol. The observation column of .data.
#' @param grid tibble. Created using [build_grid()].
#' @param echo boolean. Print progress?
#' @returns list, containing two items per element:
#'   - args: The arguments that the particular CV was called with.
#'   - cv_out: The output of [cv_hot_deck_forecast()] for those args.
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
#'
#' @export
grid_search_hot_deck_cv <- function(.data,
                                    .datetime,
                                    .observation,
                                    grid,
                                    echo = TRUE) {
  i = 1
  outputs = vector(mode = "list", length = nrow(grid))
  # ref: https://stackoverflow.com/q/63075767/6074637
  arg_lists = grid %>% purrr::pmap(\(...) list(...))
  for (arg_list in arg_lists) {
    # Need to un-list (unwrap from protection against `expand_grid`) the
    # sampler args.
    arg_list = arg_list %>%
      purrr::list_flatten(name_spec = "{inner}")
    # Don't want the sa_name passed; would error as unused arg.
    passed_arg_list = arg_list %>%
      purrr::list_assign(sa_name = rlang::zap())
    cv_out = rlang::inject(.data %>% cv_hot_deck_forecast({{ .datetime }},
                                                          {{ .observation }},
                                                          !!!passed_arg_list))
    output = list(
      arg_list = arg_list,
      cv_out = cv_out
    )
    outputs[[i]] = output
    i = i + 1

    if (isTRUE(echo)) {
      print(paste("Finished CV", i - 1, "of", nrow(grid), "."))
    }
  }

  outputs
}


#' Build grid for [grid_search_hot_deck_cv()]
#'
#' Build the grid to pass as the `grid` argument for [grid_search_hot_deck_cv()].
#'
#' If you want to pass a non-scalar to the grid search, you need to wrap
#' the argument in `list()`. e.g. `n_closest = list(30, c(rep(20,10),19:15)`
#' or `window_args = list(build_window_args(3))`. Ergo, you *always* need
#' to wrap any `window_args` or `sampler_args` in a list.
#'
#' @param times integer (scalar OR vector). The number of simulated sample paths
#'   to produce per hot deck forecast.
#' @param h integer (scalar OR vector). The forecast horizon.
#' @param n_closest integer ((scalar OR vector) OR list of (length-h vector(s)
#'   AND maybe scalar(s))).
#'   The number of closest observations to pick from
#'   per hot deck random sampling.
#' @param offset integer (scalar OR vector). Offset (in +- days) from the
#'   most recent row of .data to use as the starting point.
#' @param window_args list of [build_window_args()] outputs.
#' @param sampler_args list of [build_sampler_args()] outputs.
#' @param train_test_split_type See [cv_hot_deck_forecast()] details for details.
#' @returns tibble. Grid of parameters for use in grid search.
#'
#' @examples
#' build_grid(
#'   h = 20,
#'   n_closest = c(5, 10),
#'   # even if you only use a single `build_window_args()`, wrap in `list()`
#'   window_args = list(
#'     build_window_args(20),  # symmetric window
#'     build_window_args(20, 30),  # asymmetric window
#'     build_window_args(c(10:19, rep(20,10)), 20)  # window_back increases...
#'       #...from 10 to 20 for first 11 forecasts, then stays at 20
#'   ),
#'   # also need to wrap in list
#'   sampler_args = list(
#'     build_sampler_args(sa_name = "nxt",
#'                        sampler = sample_lead(),  # remember to call!
#'                        appender = append_lead),
#'     build_sampler_args(sa_name = "dif",
#'                        sampler = sample_diff(),  # remember to call!
#'                        appender = append_diff)
#'   )
#' )
#'
#' @export
build_grid <- function(times = 30,
                       h = 30,
                       n_closest = 20,
                       offset = rlang::zap(),
                       window_args = list(),
                       sampler_args = list(),
                       train_test_split_type = rlang::zap()) {
  # TODO: validate list args.
  grid = list(
    times = times,
    h = h,
    n_closest = n_closest,
    offset = offset,
    window_args = window_args,
    sampler_args = sampler_args,
    train_test_split_type = train_test_split_type
  )
  grid = grid %>% purrr::compact()  # removes empty args
  # list -> tibble of all combos
  grid = tidyr::expand_grid(!!!grid)
  # TODO: dedupe grid?

  # partial validation
  list_args_names = c("window_args", "sampler_args")
  names_to_check_vec = names(grid) %>% purrr::discard(\(x) x %in% list_args_names)
  for (name in names_to_check_vec) {
    validate_grid_arg_len(grid[[name]], h = h, name = name)
  }

  grid
}

#' Build grid search window argument
#'
#' Use this function to build the `window_args` arg of [build_grid()].
#'
#' @param window_back The window_back. length == 1 or h.
#' @param window_fwd The window_fwd. length == 1 or h.
#' @returns list(window_back = window_back, window_fwd = window_fwd)
#'
#' @examples
#' build_window_args(20)  # symmetric window
#' build_window_args(20, 30)  # asymmetric window
#' build_window_args(c(10:19, rep(20,10)), 20)  # window_back increases...
#'   #...from 10 to 20 for first 11 forecasts, then stays at 20
#'
#' @export
build_window_args <- function(window_back, window_fwd = window_back) {
  list(
    window_back = window_back,
    window_fwd = window_fwd
  )
}


#' Build grid search sampler argument
#'
#' Use this function to build the `sampler_args` arg of [build_grid()].
#'
#' The `sa_name` argument is for downstream usage in performance
#' assessments as a... name.
#'
#' @param sa_name string. The name of your sampler-appender pair.
#' @param sampler called function. The sampler.
#' @param appender function. The appender.
#' @param cov_fc_getter optional function. The cov_fc_getter.
#' @returns list(sa_name = sa_name, sampler = sampler,
#'   appender = appender, cov_fc_getter = cov_fc_getter)
#'
#' @examples
#' build_sampler_args(sa_name = "nxt",
#'                    sampler = sample_lead(),  # remember to call!
#'                    appender = append_lead)
#'
#' @export
build_sampler_args <- function(sa_name, sampler, appender, cov_fc_getter = NULL) {
  list(
    sa_name = sa_name,
    sampler = sampler,
    appender = appender,
    cov_fc_getter = cov_fc_getter
  )
}


#' Validate potentially-length-h grid arguments.
#'
#' Works just like the similar validator in `hot_deck_forecast`,
#' just catches this problem before you're halfway through your grid search.
#'
#' Currently this is an issue for `n_closest` and `window_{fwd, back}`.
#'
#' Raises an error if the argument is neither length-h nor scalar.
#'
#' @param grid_arg An argument for grid search.
#' @param h The `h` grid search argument.
#' @param name The name of the argument.
#'
#' @noRd
validate_grid_arg_len <- function(grid_arg, h, name) {
  invalid_els = grid_arg %>%
    purrr::discard(\(x) length(x) %in% c(1, h))
  if (length(invalid_els) != 0) {
    stop(paste(name, "has elements that are the wrong length:\n",
               invalid_els, "\n",
               "They need to be either length 1 (i.e. 'scalar') (e.g. `x = 3L`)",
               "or length h,", h),
         call. = FALSE)
  }
}
