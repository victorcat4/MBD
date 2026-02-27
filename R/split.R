#' Split Studies by Individual
#'
#' Splits downloaded study files into individual files organized by species.
#'
#' @param config Configuration list
#' @param dirs Directory structure
#' @param force Force re-split of all studies
#' @return Invisibly returns the list of created files
#' @export
split_studies <- function(config, dirs, force = FALSE) {
  cli::cli_h1("Splitting Studies by Individual")
  
  chunk_size <- config$processing$chunk_size %||% 50000
  
  # Find study files to split
  study_files <- list.files(dirs$raw, pattern = "^MB_[0-9]+\\.csv$", full.names = TRUE)
  
  if (length(study_files) == 0) {
    cli::cli_alert_warning("No study files found to split")
    return(invisible(character()))
  }
  
  cli::cli_alert_info("Found {length(study_files)} study files")
  
  all_created_files <- character()
  
  for (i in seq_along(study_files)) {
    study_file <- study_files[i]
    study_id <- gsub("^MB_([0-9]+)\\.csv$", "\\1", basename(study_file))
    
    # Check if already split
    split_flag <- file.path(dirs$raw, paste0(".split_", study_id, ".flag"))
    if (file.exists(split_flag) && !force) {
      cli::cli_alert_info("[{i}/{length(study_files)}] Study {study_id}: already split")
      next
    }
    
    cli::cli_alert_info("[{i}/{length(study_files)}] Splitting study {study_id}...")
    
    created_files <- split_single_study(
      study_file = study_file,
      output_dir = dirs$individuals,
      chunk_size = chunk_size
    )
    
    all_created_files <- c(all_created_files, created_files)
    
    # Mark as split
    writeLines(format(Sys.time()), split_flag)
    
    # Save individuals list
    individuals_file <- file.path(dirs$metadata, paste0("individuals_", study_id, ".txt"))
    writeLines(created_files, individuals_file)
    
    cli::cli_alert_success("  Created {length(created_files)} individual files")
  }
  
  cli::cli_h2("Split Complete")
  cli::cli_alert_success("Total individual files: {length(all_created_files)}")
  
  invisible(all_created_files)
}

#' Split a Single Study File
#' @keywords internal
split_single_study <- function(study_file, output_dir, chunk_size = 50000) {
  # Validate file
  first_chunk <- tryCatch({
    readr::read_csv(study_file, n_max = 1000, col_types = readr::cols(.default = readr::col_character()),
                    show_col_types = FALSE)
  }, error = function(e) NULL)
  
  if (is.null(first_chunk) || nrow(first_chunk) == 0) {
    return(character())
  }
  
  required_cols <- c("individual_id", "location_long", "location_lat", "sensor_type_id")
  if (!all(required_cols %in% names(first_chunk))) {
    return(character())
  }
  
  # Track created files
  created_files <- character()
  individuals_seen <- list()
  
  # Define sensor lookup table
  sensor_names <- c(
    "653" = "GPS",
    "82798" = "GPS", 
    "2365682" = "Argos_Doppler_Shift",
    "2365683" = "Acceleration",
    "397" = "Accessory_Measurements",
    "673" = "Argos_Doppler_Shift",
    "3886361" = "Barometer",
    "77740391" = "Bird_Ring",
    "9301403" = "Natural_Mark",
    "819073350" = "Orientation",
    "914097241" = "Solar_Geolocator",
    "1239574236" = "Solar_Geolocator_Raw"
  )
  
  # Process in chunks
  readr::read_csv_chunked(
    study_file,
    callback = readr::SideEffectChunkCallback$new(function(chunk, pos) {
      # Convert coordinates
      chunk$location_long <- as.numeric(chunk$location_long)
      chunk$location_lat <- as.numeric(chunk$location_lat)
      
      # Filter valid rows
      chunk <- chunk[!is.na(chunk$individual_id) & 
                       !is.na(chunk$location_long) & 
                       !is.na(chunk$location_lat), ]
      
      if (nrow(chunk) == 0) return(NULL)
      
      # Filter valid species names
      if ("individual_taxon_canonical_name" %in% names(chunk)) {
        chunk <- chunk[!is.na(chunk$individual_taxon_canonical_name) &
                         grepl("^[A-Z][a-z]+ [a-z]+", chunk$individual_taxon_canonical_name), ]
      }
      
      if (nrow(chunk) == 0) return(NULL)
      
      # Split by individual
      for (ind_id in unique(chunk$individual_id)) {
        ind_chunk <- chunk[chunk$individual_id == ind_id, ]
        
        # Get metadata
        species_full <- ind_chunk$individual_taxon_canonical_name[1]
        
        # --- FIX STARTS HERE ---
        # 1. Get ID as character and TRIM WHITESPACE to ensure matching
        raw_sensor_id <- as.character(ind_chunk$sensor_type_id[1])
        sensor_id_clean <- trimws(raw_sensor_id)
        
        if (is.na(species_full)) next
        
        species <- gsub(" ", "_", species_full)
        species <- gsub("[^A-Za-z0-9_]", "", species)
        
        # 2. Robust Lookup
        if (!is.na(sensor_id_clean) && sensor_id_clean %in% names(sensor_names)) {
          sensor_name <- sensor_names[[sensor_id_clean]]
        } else {
          # Fallback if ID is unknown
          if (is.na(sensor_id_clean) || sensor_id_clean == "") {
            sensor_name <- "unknown"
          } else {
            sensor_name <- paste0("sensor_", sensor_id_clean)
          }
        }
        # --- FIX ENDS HERE ---
        
        # Create species directory
        species_dir <- file.path(output_dir, species)
        if (!dir.exists(species_dir)) {
          dir.create(species_dir, showWarnings = FALSE, recursive = TRUE)
        }
        
        # Create filename
        filename <- sprintf("%s%%%s%%%s.csv", species, sensor_name, ind_id)
        filepath <- file.path(species_dir, filename)
        
        # Append or create
        if (file.exists(filepath)) {
          readr::write_csv(ind_chunk, filepath, append = TRUE)
        } else {
          readr::write_csv(ind_chunk, filepath)
          relative_path <- file.path(species, filename)
          created_files <<- c(created_files, relative_path)
        }
        
        individuals_seen[[ind_id]] <<- TRUE
      }
      
      NULL
    }),
    chunk_size = chunk_size,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  
  unique(created_files)
}