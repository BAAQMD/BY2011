make_BY2011_legacy_env <- function (
  rda_path,
  verbose = TRUE
) {

  BY2011_legacy_env <- new.env()
  load(rda_path, envir = BY2011_legacy_env)
  return(BY2011_legacy_env)

}
