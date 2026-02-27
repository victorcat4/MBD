#' Read and Validate Configuration File
#'
#' Reads a YAML configuration file and validates required fields.
#'
#' @param config_path Path to the YAML configuration file
#' @return A list containing the configuration
#' @export
#' @examples
#' \dontrun{
#' config <- read_config("config.yaml")
#' }
read_config <- function(config_path) {
  if (!file.exists(config_path)) {
    stop("Configuration file not found: ", config_path)
  }
  
  config <- yaml::read_yaml(config_path)
  

  required_fields <- c("movebank", "taxa", "sensors", "output_dir")
  missing <- setdiff(required_fields, names(config))
  
  if (length(missing) > 0) {
    stop("Missing required config fields: ", paste(missing, collapse = ", "))
  }
  
  if (is.null(config$movebank$username) || is.null(config$movebank$password)) {
    stop("Config must include movebank.username and movebank.password")
  }
  
  # Set defaults
  if (is.null(config$processing$chunk_size)) {
    config$processing$chunk_size <- 50000
  }
  
  if (is.null(config$incremental$enabled)) {
    config$incremental$enabled <- TRUE
  }
  
  if (is.null(config$incremental$min_new_days)) {
    config$incremental$min_new_days <- 90
  }
  
  config
}

#' Create Directory Structure
#'
#' Creates the output directory structure for the pipeline.
#'
#' @param output_dir Base output directory
#' @return A list with paths to subdirectories
#' @keywords internal
create_dirs <- function(output_dir) {
  dirs <- list(
    base = output_dir,
    metadata = file.path(output_dir, "metadata"),
    raw = file.path(output_dir, "raw_studies"),
    individuals = file.path(output_dir, "individuals")
  )
  
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  dirs
}
