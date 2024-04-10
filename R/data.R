#' Daily mean surface water temperatures of Lake Suggs, FL
#'
#' [NEON description of Lake Suggs](https://www.neonscience.org/field-sites/sugg)
#'
#' The data comes from the NEON field site at Lake Suggs, Florida.
#' It was processed by the NEON Ecological Forecasting Challenge group,
#' which you can find information on
#' [here](https://projects.ecoforecast.org/neon4cast-ci/targets.html) and
#' [here](https://projects.ecoforecast.org/neon4cast-docs/Aquatics.html).
#' This subset of that data goes from 2017-08-19 to 2024-01-22.
#' It contains ~10% missing observations.
#'
#' @format A `tsibble` with 2348 rows and 2 variables:
#' \describe{
#'   \item{date}{Date}
#'   \item{observation}{Mean surface water temperature (Celcius)}
#' }
#'
#' @source <https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz>
"SUGG_temp"
