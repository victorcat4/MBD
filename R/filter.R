#' Filter Individuals by Polygon and Taxa
#'
#' Removes individual files that don't have any locations within the
#' specified polygon, and removes species that don't match the configured taxa.
#'
#' @param config Configuration list
#' @param dirs Directory structure
#' @param force Force re-filtering
#' @return Summary of filtering results
#' @export
filter_individuals <- function(config, dirs, force = FALSE) {
  cli::cli_h1("Filtering Individuals")
  
  polygon_config <- config$polygon
  taxa <- config$taxa
  chunk_size <- config$processing$chunk_size %||% 50000
  
  # Results tracking
  results <- list(
    taxa_kept = 0,
    taxa_removed = 0,
    polygon_kept = 0,
    polygon_removed = 0
  )
  
  # ===== TAXA FILTERING (first - removes entire species directories) =====
  cli::cli_h2("Taxa Filtering")
  
  # Expand taxa to species list
  cache_dir <- file.path(dirs$metadata, "taxonomy_cache")
  expanded_taxa <- expand_all_taxa(taxa, cache_dir = cache_dir)
  species_to_keep <- gsub(" ", "_", expanded_taxa)
  
  cli::cli_alert_info("Keeping {length(species_to_keep)} species")
  
  # Check each species folder
  species_dirs <- list.dirs(dirs$individuals, full.names = TRUE, recursive = FALSE)
  
  for (species_dir in species_dirs) {
    species_name <- basename(species_dir)
    n_files <- length(list.files(species_dir, pattern = "\\.csv$"))
    
    if (species_name %in% species_to_keep) {
      cli::cli_alert_success("  KEEP: {species_name} ({n_files} files)")
      results$taxa_kept <- results$taxa_kept + n_files
    } else {
      cli::cli_alert_warning("  REMOVE: {species_name} ({n_files} files)")
      results$taxa_removed <- results$taxa_removed + n_files
      unlink(species_dir, recursive = TRUE)
    }
  }
  
  # ===== POLYGON FILTERING (second - only on remaining files) =====
  if (!is.null(polygon_config) && !is.null(polygon_config$coordinates)) {
    cli::cli_h2("Polygon Filtering")
    
    # Create polygon
    coords <- do.call(rbind, lapply(polygon_config$coordinates, function(pt) {
      c(as.numeric(pt[1]), as.numeric(pt[2]))
    }))
    polygon <- sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
    bounds <- sf::st_bbox(polygon)
    cli::cli_alert_info("Polygon bounds: {round(bounds['xmin'],2)}-{round(bounds['xmax'],2)}E, {round(bounds['ymin'],2)}-{round(bounds['ymax'],2)}N")
    
    # Find all individual files (only from species that passed taxa filter)
    species_dirs <- list.dirs(dirs$individuals, full.names = TRUE, recursive = FALSE)
    
    for (species_dir in species_dirs) {
      species_name <- basename(species_dir)
      individual_files <- list.files(species_dir, pattern = "\\.csv$", full.names = TRUE)
      
      if (length(individual_files) == 0) next
      
      cli::cli_alert_info("Filtering {species_name}: {length(individual_files)} individuals")
      
      for (ind_file in individual_files) {
        # Check if already filtered
        ind_basename <- basename(ind_file)
        filter_flag <- file.path(dirs$metadata, paste0(".filtered_", gsub("\\.csv$", "", ind_basename), ".flag"))
        
        if (file.exists(filter_flag) && !force) {
          results$polygon_kept <- results$polygon_kept + 1
          next
        }
        
        in_polygon <- check_individual_in_polygon(ind_file, polygon, chunk_size)
        
        if (in_polygon) {
          results$polygon_kept <- results$polygon_kept + 1
          writeLines(format(Sys.time()), filter_flag)
        } else {
          results$polygon_removed <- results$polygon_removed + 1
          unlink(ind_file)
        }
      }
    }
    
    cli::cli_alert_success("Polygon filter: kept {results$polygon_kept}, removed {results$polygon_removed}")
  } else {
    cli::cli_alert_info("No polygon specified, skipping spatial filtering")
  }
  
  # ===== SUMMARY =====
  cli::cli_h2("Filtering Complete")
  
  summary_file <- file.path(dirs$metadata, "filtering_summary.txt")
  summary_lines <- c(
    "Filtering Summary",
    "=================",
    sprintf("Date: %s", format(Sys.time())),
    "",
    "Taxa Filtering:",
    sprintf("  Files kept: %d", results$taxa_kept),
    sprintf("  Files removed: %d", results$taxa_removed),
    "",
    "Polygon Filtering:",
    sprintf("  Kept: %d", results$polygon_kept),
    sprintf("  Removed: %d", results$polygon_removed)
  )
  writeLines(summary_lines, summary_file)
  
  # Mark as complete
  writeLines(format(Sys.time()), file.path(dirs$metadata, ".filtering_complete.flag"))
  
  results
}

#' Check if Individual Has Points in Polygon
#' @keywords internal
check_individual_in_polygon <- function(individual_file, polygon, chunk_size = 50000) {
  found_in_polygon <- FALSE
  
  tryCatch({
    readr::read_csv_chunked(
      individual_file,
      callback = readr::DataFrameCallback$new(function(chunk, pos) {
        if (found_in_polygon) return(NULL)
        
        lons <- suppressWarnings(as.numeric(chunk$location_long))
        lats <- suppressWarnings(as.numeric(chunk$location_lat))
        
        valid <- !is.na(lons) & !is.na(lats)
        lons <- lons[valid]
        lats <- lats[valid]
        
        if (length(lons) > 0) {
          points <- sf::st_as_sf(data.frame(lon = lons, lat = lats),
                                 coords = c("lon", "lat"), crs = 4326)
          
          in_polygon <- sf::st_intersects(points, polygon, sparse = FALSE)[, 1]
          
          if (any(in_polygon)) {
            found_in_polygon <<- TRUE
          }
        }
        
        NULL
      }),
      chunk_size = chunk_size,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  }, error = function(e) {
    # On error, remove the file
    found_in_polygon <<- FALSE
  })
  
  found_in_polygon
}