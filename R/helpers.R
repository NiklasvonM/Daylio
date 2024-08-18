add_suffix_to_filename <- function(file_path, suffix) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("Invalid file_path. It must be a single character string.")
  }
  if (!is.character(suffix) || length(suffix) != 1) {
    stop("Invalid suffix. It must be a single character string.")
  }

  file_name <- file_path_sans_ext(basename(file_path))
  extension <- file_ext(file_path)
  directory <- dirname(file_path)

  new_file_name <- paste0(file_name, suffix, ".", extension)
  new_file_path <- file.path(directory, new_file_name)

  return(new_file_path)
}
