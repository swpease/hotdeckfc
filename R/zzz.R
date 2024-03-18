.onLoad <- function(libname, pkgname) {
  get_local_rows <<- memoise::memoise(get_local_rows, omit_args = c(".datetime"))
}
