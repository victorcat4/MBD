#' Expand Taxon to Species Using GBIF
#'
#' Takes a taxon name at any rank and returns all species within it.
#'
#' @param taxon_name Scientific name (e.g., "Aves", "Anatidae")
#' @param cache_dir Directory to cache results (NULL to disable)
#' @param cache_days Days before cache expires
#' @return Character vector of species names
#' @export
expand_taxa <- function(taxon_name, cache_dir = NULL, cache_days = 30) {
  # Check cache
  if (!is.null(cache_dir)) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    
    safe_name <- gsub("[^A-Za-z0-9_-]", "_", taxon_name)
    cache_file <- file.path(cache_dir, paste0(safe_name, "_species.rds"))
    
    if (file.exists(cache_file)) {
      cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "days")
      if (cache_age < cache_days) {
        return(readRDS(cache_file))
      }
    }
  }
  
  # Search for taxon
  taxon_info <- gbif_search_taxon(taxon_name)
  if (is.null(taxon_info)) {
    warning("Could not find taxon '", taxon_name, "' in GBIF")
    # Return original name as fallback
    return(taxon_name)
  }
  
  # If already species level, return canonical name (without author)
  # Also include the original input in case of slight differences
  if (tolower(taxon_info$rank) == "species") {
    result <- unique(c(taxon_info$canonicalName, taxon_name))
    
    # Cache results
    if (!is.null(cache_dir)) {
      saveRDS(result, cache_file)
    }
    return(result)
  }
  
  # Get descendants
  descendants <- gbif_get_descendants(taxon_info$key, "species")
  
  # Always include the original taxon name as well
  descendants <- unique(c(descendants, taxon_name))
  
  # Cache results
  if (!is.null(cache_dir)) {
    saveRDS(descendants, cache_file)
  }
  
  descendants
}

#' Search GBIF for Taxon
#' @keywords internal
gbif_search_taxon <- function(taxon_name) {
  response <- tryCatch({
    httr::GET("https://api.gbif.org/v1/species/match",
              query = list(name = taxon_name),
              httr::timeout(30))
  }, error = function(e) NULL)
  
  if (is.null(response) || httr::status_code(response) != 200) return(NULL)
  
  data <- httr::content(response, "parsed")
  if (is.null(data$usageKey) || data$matchType == "NONE") return(NULL)
  
  # Use canonicalName (without author) for matching, not scientificName
  canonical <- data$canonicalName
  if (is.null(canonical)) canonical <- data$scientificName
  
  list(
    key = data$usageKey,
    canonicalName = canonical,
    scientificName = data$scientificName,
    rank = data$rank
  )
}

#' Get Descendants from GBIF
#' @keywords internal
gbif_get_descendants <- function(taxon_key, target_rank, limit = 1000, max_depth = 10, depth = 0) {
  if (depth > max_depth) return(character(0))
  
  results <- character(0)
  offset <- 0
  
  hierarchy <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  target_idx <- which(hierarchy == tolower(target_rank))
  
  repeat {
    url <- sprintf("https://api.gbif.org/v1/species/%s/children", taxon_key)
    response <- tryCatch({
      httr::GET(url, query = list(limit = limit, offset = offset), httr::timeout(30))
    }, error = function(e) NULL)
    
    if (is.null(response) || httr::status_code(response) != 200) break
    
    data <- httr::content(response, "parsed")
    if (length(data$results) == 0) break
    
    for (child in data$results) {
      child_rank <- tolower(child$rank)
      child_name <- child$canonicalName
      if (is.null(child_name)) next
      
      child_idx <- which(hierarchy == child_rank)
      if (length(child_idx) == 0) next
      
      if (child_rank == tolower(target_rank)) {
        results <- c(results, child_name)
      } else if (child_idx < target_idx) {
        child_results <- gbif_get_descendants(child$key, target_rank, limit, max_depth, depth + 1)
        results <- c(results, child_results)
      }
    }
    
    if (isTRUE(data$endOfRecords)) break
    offset <- offset + limit
    Sys.sleep(0.2)
  }
  
  unique(results)
}

#' Expand Multiple Taxa
#' @keywords internal
expand_all_taxa <- function(taxa_list, cache_dir = NULL) {
  all_species <- character(0)
  
  for (taxon in taxa_list) {
    cli::cli_alert_info("Expanding taxon: {taxon}")
    species <- tryCatch({
      expand_taxa(taxon, cache_dir = cache_dir)
    }, error = function(e) {
      cli::cli_alert_warning("Could not expand '{taxon}': {e$message}")
      return(taxon)  # Return original on error
    })
    
    # Always include the original taxon name
    species <- unique(c(species, taxon))
    
    all_species <- c(all_species, species)
    cli::cli_alert_success("  -> {length(species)} species")
  }
  
  unique(all_species)
}