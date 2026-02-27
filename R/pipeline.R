#' Run Complete Movebank Download Pipeline
#'
#' Downloads animal tracking data from Movebank, splits by individual,
#' and filters by polygon and taxa based on a YAML configuration file.
#'
#' @param config_path Path to YAML configuration file
#' @param steps Which steps to run: "all", "download", "split", "filter", or a combination
#' @param force Force re-processing even if already done
#' @return Invisibly returns the configuration used
#' @export
#' @examples
#' \dontrun{
#' # Run complete pipeline
#' run_pipeline("config.yaml")
#'
#' # Run only download step
#' run_pipeline("config.yaml", steps = "download")
#'
#' # Force re-download everything
#' run_pipeline("config.yaml", force = TRUE)
#' }
run_pipeline <- function(config_path, steps = "all", force = FALSE) {
  
  cli::cli_h1("Movebank Data Pipeline")
  cli::cli_alert_info("Config: {config_path}")
  cli::cli_alert_info("Steps: {paste(steps, collapse=', ')}")
  cli::cli_alert_info("Force: {force}")
  
  # Read configuration
  config <- read_config(config_path)
  
  # Create directory structure
  dirs <- create_dirs(config$output_dir)
  
  cli::cli_alert_info("Output directory: {dirs$base}")
  cli::cli_alert_info("Taxa: {paste(config$taxa, collapse=', ')}")
  cli::cli_alert_info("Sensors: {paste(config$sensors, collapse=', ')}")
  
  if (!is.null(config$polygon)) {
    cli::cli_alert_info("Polygon: {length(config$polygon$coordinates)} vertices")
  } else {
    cli::cli_alert_info("Polygon: none (no spatial filtering)")
  }
  
  # Determine which steps to run
  run_download <- "all" %in% steps || "download" %in% steps
  run_split <- "all" %in% steps || "split" %in% steps
  run_filter <- "all" %in% steps || "filter" %in% steps
  
  # ===== STEP 1: DOWNLOAD =====
  if (run_download) {
    download_studies(config, dirs, force = force)
  }
  
  # ===== STEP 2: SPLIT =====
  if (run_split) {
    split_studies(config, dirs, force = force)
  }
  
  # ===== STEP 3: FILTER =====
  if (run_filter) {
    filter_individuals(config, dirs, force = force)
  }
  
  # ===== FINAL SUMMARY =====
  cli::cli_h1("Pipeline Complete")
  
  # Count results
  species_dirs <- list.dirs(dirs$individuals, full.names = FALSE, recursive = FALSE)
  species_dirs <- species_dirs[species_dirs != ""]
  
  total_files <- 0
  for (sp in species_dirs) {
    sp_dir <- file.path(dirs$individuals, sp)
    total_files <- total_files + length(list.files(sp_dir, pattern = "\\.csv$"))
  }
  
  cli::cli_alert_success("Species: {length(species_dirs)}")
  cli::cli_alert_success("Individual files: {total_files}")
  cli::cli_alert_success("Output: {dirs$individuals}")
  
  # ===== EXPORT STUDY LICENSES =====
  export_study_licenses(dirs)
  
  invisible(config)
}

#' Export Study License Information
#'
#' Creates a table with license information for all studies that have
#' individuals in the output directory.
#'
#' @param dirs Directory structure from create_dirs()
#' @return Invisibly returns the license data frame
#' @keywords internal
export_study_licenses <- function(dirs) {
  cli::cli_h2("Exporting Study Licenses")
  
  # Load all_studies.csv which has license info
  all_studies_file <- file.path(dirs$metadata, "all_studies.csv")
  if (!file.exists(all_studies_file)) {
    cli::cli_alert_warning("No all_studies.csv found, skipping license export")
    return(invisible(NULL))
  }
  
  all_studies <- readr::read_csv(all_studies_file, show_col_types = FALSE)
  
  # Find which study IDs are represented in the individuals folder
  # by reading the individuals_*.txt files
  individuals_files <- list.files(dirs$metadata, pattern = "^individuals_[0-9]+\\.txt$", full.names = TRUE)
  
  used_study_ids <- character()
  for (ind_file in individuals_files) {
    # Extract study_id from filename
    study_id <- gsub("^individuals_([0-9]+)\\.txt$", "\\1", basename(ind_file))
    
    # Check if there are actually individuals from this study
    individuals <- readLines(ind_file, warn = FALSE)
    individuals <- individuals[individuals != ""]
    
    # Check if any of these individual files still exist (after filtering)
    existing <- sapply(individuals, function(ind) {
      file.exists(file.path(dirs$individuals, ind))
    })
    
    if (any(existing)) {
      used_study_ids <- c(used_study_ids, study_id)
    }
  }
  
  used_study_ids <- unique(used_study_ids)
  
  if (length(used_study_ids) == 0) {
    cli::cli_alert_warning("No studies with individuals found")
    return(invisible(NULL))
  }
  
  # Filter to used studies and select license columns
  all_studies$id <- as.character(all_studies$id)
  license_df <- all_studies[all_studies$id %in% used_study_ids, ]
  
  # Select relevant columns (if they exist)
  license_cols <- c("id", "name", "license_type", "license_terms", "citation", "acknowledgements",
                    "principal_investigator_name", "principal_investigator_email")
  available_cols <- intersect(license_cols, names(license_df))
  license_df <- license_df[, available_cols, drop = FALSE]
  
  # Rename id to study_id for clarity
  names(license_df)[names(license_df) == "id"] <- "study_id"
  
  # Save to output directory
  license_file <- file.path(dirs$base, "study_licenses.csv")
  readr::write_csv(license_df, license_file)
  
  cli::cli_alert_success("Saved license info for {nrow(license_df)} studies to: {license_file}")
  
  invisible(license_df)
}

#' Quick Status Check
#'
#' Shows the current status of a pipeline output directory.
#'
#' @param output_dir Path to output directory
#' @export
pipeline_status <- function(output_dir) {
  cli::cli_h1("Pipeline Status")
  cli::cli_alert_info("Directory: {output_dir}")
  
  if (!dir.exists(output_dir)) {
    cli::cli_alert_danger("Directory does not exist")
    return(invisible(NULL))
  }
  
  # Check subdirectories
  raw_dir <- file.path(output_dir, "raw_studies")
  ind_dir <- file.path(output_dir, "individuals")
  meta_dir <- file.path(output_dir, "metadata")
  
  # Raw studies
  if (dir.exists(raw_dir)) {
    study_files <- list.files(raw_dir, pattern = "^MB_[0-9]+\\.csv$")
    total_size <- sum(file.size(file.path(raw_dir, study_files))) / 1024^3
    cli::cli_alert_info("Raw studies: {length(study_files)} files ({round(total_size, 2)} GB)")
  } else {
    cli::cli_alert_warning("Raw studies: none")
  }
  
  # Individuals
  if (dir.exists(ind_dir)) {
    species_dirs <- list.dirs(ind_dir, full.names = FALSE, recursive = FALSE)
    species_dirs <- species_dirs[species_dirs != ""]
    
    total_files <- 0
    for (sp in species_dirs) {
      total_files <- total_files + length(list.files(file.path(ind_dir, sp), pattern = "\\.csv$"))
    }
    
    cli::cli_alert_info("Species: {length(species_dirs)}")
    cli::cli_alert_info("Individual files: {total_files}")
    
    if (length(species_dirs) > 0) {
      cli::cli_h2("Species breakdown")
      for (sp in head(species_dirs, 10)) {
        n <- length(list.files(file.path(ind_dir, sp), pattern = "\\.csv$"))
        cli::cli_alert_info("  {sp}: {n} individuals")
      }
      if (length(species_dirs) > 10) {
        cli::cli_alert_info("  ... and {length(species_dirs) - 10} more species")
      }
    }
  } else {
    cli::cli_alert_warning("Individuals: none")
  }
  
  # Timestamps
  ts_file <- file.path(output_dir, "study_timestamps.csv")
  if (file.exists(ts_file)) {
    ts <- readr::read_csv(ts_file, show_col_types = FALSE)
    cli::cli_alert_info("Tracked studies: {nrow(ts)}")
  }
  
  # License file
  license_file <- file.path(output_dir, "study_licenses.csv")
  if (file.exists(license_file)) {
    lic <- readr::read_csv(license_file, show_col_types = FALSE)
    cli::cli_alert_info("Studies with license info: {nrow(lic)}")
  }
  
  invisible(NULL)
}