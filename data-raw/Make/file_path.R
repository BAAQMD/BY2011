#
# TODO: get rid of the need for this.
#
file_path <- function (..., mustWork = TRUE) {
  normalizePath(file.path(...), mustWork = mustWork)
}
