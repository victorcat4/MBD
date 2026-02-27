#' Download Movebank Studies
#'
#' Downloads studies matching the configured taxa and sensors.
#'
#' @param config Configuration list from read_config()
#' @param dirs Directory structure from create_dirs()
#' @param force Force re-download of all studies
#' @return Data frame of downloaded studies
#' @export
download_studies <- function(config, dirs, force = FALSE) {
  cli::cli_h1("Downloading Movebank Studies")
  
  username <- config$movebank$username
  password <- config$movebank$password
  taxa <- config$taxa
  sensors <- config$sensors
  min_new_days <- config$incremental$min_new_days %||% 90
  
  # Convert sensor names to IDs for API filtering
  sensor_ids <- sensor_names_to_ids(sensors)
  cli::cli_alert_info("Sensor filter: {paste(sensors, collapse=', ')} -> IDs: {paste(sensor_ids, collapse=', ')}")
  
  # Test credentials
  cli::cli_alert_info("Testing credentials...")
  if (!movebank_test_credentials(username, password)) {
    stop("Authentication failed! Check credentials.")
  }
  cli::cli_alert_success("Authentication successful")
  
  # Get studies from Movebank
  cli::cli_alert_info("Retrieving study list...")
  studies <- movebank_get_studies(username, password)
  cli::cli_alert_info("Total studies on Movebank: {nrow(studies)}")
  
  # Filter by sensors
  cli::cli_alert_info("Filtering by sensors: {paste(sensors, collapse=', ')}")
  sensor_pattern <- paste(sensors, collapse = "|")
  studies <- studies[!is.na(studies$sensor_type_ids) & 
                       grepl(sensor_pattern, studies$sensor_type_ids, ignore.case = TRUE), ]
  cli::cli_alert_info("After sensor filter: {nrow(studies)}")
  
  # Debug: show column names and sample taxon_ids
  cli::cli_alert_info("Available columns: {paste(names(studies), collapse=', ')}")
  if ("taxon_ids" %in% names(studies)) {
    sample_taxa <- head(studies$taxon_ids[!is.na(studies$taxon_ids) & nchar(studies$taxon_ids) > 0], 3)
    cli::cli_alert_info("Sample taxon_ids from Movebank:")
    for (t in sample_taxa) {
      cli::cli_alert_info("  '{substr(t, 1, 150)}'")
    }
  } else {
    cli::cli_alert_warning("No 'taxon_ids' column found in Movebank data!")
  }
  
  # Expand and filter by taxa
  cli::cli_alert_info("Expanding taxa...")
  cache_dir <- file.path(dirs$metadata, "taxonomy_cache")
  
  # Clear old cache to ensure fresh data
  if (dir.exists(cache_dir)) {
    old_cache <- list.files(cache_dir, full.names = TRUE)
    if (length(old_cache) > 0) {
      cli::cli_alert_info("Clearing taxonomy cache...")
      unlink(old_cache)
    }
  }
  
  expanded_taxa <- expand_all_taxa(taxa, cache_dir = cache_dir)
  cli::cli_alert_info("Total species after expansion: {length(expanded_taxa)}")
  cli::cli_alert_info("Searching for: {paste(expanded_taxa, collapse=', ')}")
  
  # Filter studies with valid taxon_ids
  studies_before_taxa <- studies[!is.na(studies$taxon_ids) & nchar(studies$taxon_ids) > 0, ]
  cli::cli_alert_info("Studies with taxon info: {nrow(studies_before_taxa)}")
  
  # Filter by taxa match - use simpler matching that handles various formats
  # Movebank taxon_ids can be comma-separated or contain subspecies
  matching_studies <- sapply(studies_before_taxa$taxon_ids, function(taxon_string) {
    if (is.na(taxon_string) || nchar(taxon_string) == 0) return(FALSE)
    
    # Check if any of our taxa appear in the taxon string
    for (taxon in expanded_taxa) {
      # Use case-insensitive matching
      if (grepl(taxon, taxon_string, ignore.case = TRUE, fixed = FALSE)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }, USE.NAMES = FALSE)
  
  studies <- studies_before_taxa[matching_studies, ]
  cli::cli_alert_info("After taxa filter: {nrow(studies)}")
  
  # Debug: show some example taxon_ids if no matches
  if (nrow(studies) == 0) {
    cli::cli_alert_warning("No taxa matches found. Example taxon_ids from Movebank:")
    sample_ids <- head(studies_before_taxa$taxon_ids[!is.na(studies_before_taxa$taxon_ids)], 5)
    for (id in sample_ids) {
      cli::cli_alert_info("  {substr(id, 1, 100)}")
    }
  }
  
  # Filter by download access
  studies <- studies[studies$i_have_download_access == TRUE |
                       studies$i_have_download_access == "true" |
                       tolower(as.character(studies$i_have_download_access)) == "true", ]
  cli::cli_alert_info("With download access: {nrow(studies)}")
  
  if (nrow(studies) == 0) {
    cli::cli_alert_warning("No studies match the criteria")
    return(data.frame())
  }
  
  # Save all matching studies
  studies$id <- as.character(studies$id)
  all_studies_file <- file.path(dirs$metadata, "all_studies.csv")
  readr::write_csv(studies, all_studies_file)
  
  # Check what needs downloading
  timestamp_file <- file.path(dirs$base, "study_timestamps.csv")
  existing_files <- list.files(dirs$raw, pattern = "^MB_[0-9]+\\.csv$")
  existing_ids <- gsub("^MB_([0-9]+)\\.csv$", "\\1", existing_files)
  
  # Load previous timestamps
  if (file.exists(timestamp_file)) {
    previous <- readr::read_csv(timestamp_file, show_col_types = FALSE)
    previous$study_id <- as.character(previous$study_id)
  } else {
    previous <- data.frame(study_id = character(), last_deployed = as.Date(character()))
  }
  
  # Determine what to download
  studies$timestamp_last_deployed_location <- as.Date(
    substr(studies$timestamp_last_deployed_location, 1, 10)
  )
  
  to_download <- data.frame()
  
  for (i in seq_len(nrow(studies))) {
    study_id <- studies$id[i]
    study_name <- studies$name[i]
    mb_timestamp <- studies$timestamp_last_deployed_location[i]
    
    has_file <- study_id %in% existing_ids
    prev_row <- previous[previous$study_id == study_id, ]
    has_timestamp <- nrow(prev_row) > 0
    
    should_download <- FALSE
    reason <- ""
    
    if (force) {
      should_download <- TRUE
      reason <- "forced"
    } else if (!has_file) {
      should_download <- TRUE
      reason <- "new"
    } else if (has_file && has_timestamp && !is.na(mb_timestamp)) {
      prev_deployed <- as.Date(prev_row$last_deployed[1])
      days_new <- as.numeric(mb_timestamp - prev_deployed)
      if (days_new >= min_new_days) {
        should_download <- TRUE
        reason <- sprintf("%d days new data", days_new)
      }
    }
    
    if (should_download) {
      to_download <- rbind(to_download, data.frame(
        study_id = study_id,
        name = study_name,
        timestamp = as.character(mb_timestamp),
        reason = reason,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  cli::cli_h2("Download Summary")
  cli::cli_alert_info("Studies to download: {nrow(to_download)}")
  cli::cli_alert_info("Studies already up-to-date: {nrow(studies) - nrow(to_download)}")
  
  if (nrow(to_download) == 0) {
    cli::cli_alert_success("All studies are up-to-date!")
    return(studies)
  }
  
  # Download each study
  cli::cli_h2("Downloading Studies")
  
  successful <- 0
  failed <- 0
  total_mb <- 0
  start_time <- Sys.time()
  
  for (i in seq_len(nrow(to_download))) {
    study_id <- to_download$study_id[i]
    study_name <- to_download$name[i]
    reason <- to_download$reason[i]
    
    cli::cli_alert_info("[{i}/{nrow(to_download)}] {study_name} ({reason})")
    
    output_file <- file.path(dirs$raw, paste0("MB_", study_id, ".csv"))
    
    # Retry logic
    max_retries <- 5
    success <- FALSE
    
    for (attempt in 1:max_retries) {
      result <- movebank_download_study(study_id, username, password, output_file, 
                                        sensor_type_ids = sensor_ids, quiet = FALSE)
      
      if (isTRUE(result)) {
        file_size <- file.size(output_file) / 1024^2
        total_mb <- total_mb + file_size
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
        cli::cli_alert_success("  Downloaded ({round(file_size, 1)} MB) | Total: {round(total_mb, 1)} MB in {round(elapsed, 1)} min")
        success <- TRUE
        
        # Update timestamp
        new_entry <- data.frame(
          study_id = study_id,
          last_deployed = to_download$timestamp[i],
          last_download = format(Sys.Date(), "%Y-%m-%d")
        )
        
        if (file.exists(timestamp_file)) {
          existing <- readr::read_csv(timestamp_file, show_col_types = FALSE)
          existing$study_id <- as.character(existing$study_id)
          existing <- existing[existing$study_id != study_id, ]
          updated <- rbind(existing, new_entry)
        } else {
          updated <- new_entry
        }
        readr::write_csv(updated, timestamp_file)
        
        break
        
      } else if (identical(result, "RATE_LIMITED")) {
        wait <- min(120 * (2 ^ (attempt - 1)), 600)
        cli::cli_alert_warning("  Rate limited, waiting {wait}s...")
        Sys.sleep(wait)
      } else {
        wait <- min(10 * (2 ^ (attempt - 1)), 300)
        # Extract error details if result is a list with status info
        if (is.list(result) && !is.null(result$status)) {
          error_msg <- sprintf("HTTP %d", result$status)
          if (!is.null(result$message)) {
            error_msg <- paste0(error_msg, ": ", substr(result$message, 1, 80))
          }
          cli::cli_alert_warning("  Failed ({error_msg}), retrying in {wait}s...")
        } else {
          cli::cli_alert_warning("  Failed, retrying in {wait}s...")
        }
        Sys.sleep(wait)
      }
    }
    
    if (success) {
      successful <- successful + 1
      # Standard delay between successful downloads
      Sys.sleep(2)
    } else {
      cli::cli_alert_danger("  Failed after {max_retries} attempts")
      failed <- failed + 1
      # Longer delay after complete failure to let server recover
      Sys.sleep(10)
    }
  }
  
  cli::cli_h2("Download Complete")
  elapsed_total <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)
  cli::cli_alert_success("Successful: {successful} ({round(total_mb, 1)} MB in {elapsed_total} min)")
  if (failed > 0) cli::cli_alert_danger("Failed: {failed}")
  
  studies
}

# Helper for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x