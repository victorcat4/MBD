#' Sensor Name to ID Mapping
#' @keywords internal
SENSOR_ID_MAP <- list(
  "GPS" = "653",
  "Argos Doppler Shift" = "82798",
  "Radio Transmitter" = "673",
  "Bird Ring" = "397",
  "Natural Mark" = "2365682",
  "Solar Geolocator" = "3886361"
)

#' Convert Sensor Names to IDs
#'
#' @param sensor_names Character vector of sensor names from config
#' @return Character vector of sensor IDs for API
#' @export
sensor_names_to_ids <- function(sensor_names) {

  ids <- character()
  for (name in sensor_names) {
    if (name %in% names(SENSOR_ID_MAP)) {
      ids <- c(ids, SENSOR_ID_MAP[[name]])
    } else {
      cli::cli_alert_warning("Unknown sensor name: {name}")
    }
  }
  unique(ids)
}

#' Test Movebank Credentials
#'
#' @param username Movebank username
#' @param password Movebank password
#' @param max_retries Number of retry attempts for transient failures
#' @return TRUE if credentials are valid, FALSE otherwise
#' @keywords internal
movebank_test_credentials <- function(username, password, max_retries = 3) {
  base_url <- "https://www.movebank.org/movebank/service/direct-read"
  
  for (attempt in 1:max_retries) {
    response <- tryCatch({
      httr::GET(
        base_url,
        query = list(entity_type = "study", i_have_download_access = "true"),
        httr::authenticate(username, password, type = "basic"),
        httr::timeout(30)
      )
    }, error = function(e) {
      list(error = TRUE, message = e$message)
    })
    
    # Network error - retry
    if (is.list(response) && isTRUE(response$error)) {
      if (attempt < max_retries) {
        wait <- 5 * attempt
        cli::cli_alert_warning("Network error: {response$message}. Retrying in {wait}s...")
        Sys.sleep(wait)
        next
      } else {
        cli::cli_alert_danger("Network error after {max_retries} attempts: {response$message}")
        return(FALSE)
      }
    }
    
    status <- httr::status_code(response)
    
    # Success
    if (status == 200) {
      return(TRUE)
    }
    
    # Actual authentication failure - don't retry
    if (status == 401 || status == 403) {
      cli::cli_alert_danger("Authentication failed (HTTP {status})")
      return(FALSE)
    }
    
    # Rate limited or server error - retry
    if (status %in% c(429, 500, 502, 503, 504)) {
      if (attempt < max_retries) {
        wait <- ifelse(status == 429, 30, 10) * attempt
        cli::cli_alert_warning("Server returned HTTP {status}. Retrying in {wait}s...")
        Sys.sleep(wait)
        next
      } else {
        cli::cli_alert_danger("Server error (HTTP {status}) after {max_retries} attempts")
        return(FALSE)
      }
    }
    
    # Other unexpected status
    cli::cli_alert_danger("Unexpected response (HTTP {status})")
    return(FALSE)
  }
  
  FALSE
}

#' Get List of Studies from Movebank
#'
#' Retrieves the list of all studies accessible to the user.
#'
#' @param username Movebank username
#' @param password Movebank password
#' @return Data frame with study information
#' @export
movebank_get_studies <- function(username, password) {
  base_url <- "https://www.movebank.org/movebank/service/direct-read"
  
  response <- tryCatch({
    httr::GET(
      base_url,
      query = list(
        entity_type = "study",
        attributes = paste(c(
          "id", "name", "i_have_download_access",
          "timestamp_last_deployed_location",
          "number_of_deployed_locations",
          "taxon_ids", "sensor_type_ids",
          "main_location_long", "main_location_lat",
          "license_type", "license_terms", "citation", "acknowledgements",
          "principal_investigator_name", "principal_investigator_email"
        ), collapse = ",")
      ),
      httr::authenticate(username, password, type = "basic"),
      httr::timeout(120)
    )
  }, error = function(e) {
    stop("Network error getting studies: ", e$message)
  })
  
  if (httr::status_code(response) != 200) {
    stop("API error (status ", httr::status_code(response), ") getting studies")
  }
  
  content_text <- httr::content(response, "text", encoding = "UTF-8")
  read.csv(text = content_text, stringsAsFactors = FALSE)
}

#' Download Study Data from Movebank
#'
#' Downloads event data for a single study.
#'
#' @param study_id Movebank study ID
#' @param username Movebank username
#' @param password Movebank password
#' @param output_file Output CSV file path
#' @param sensor_type_ids Character vector of sensor type IDs to filter (NULL for all)
#' @param quiet Suppress progress messages
#' @return TRUE if successful, FALSE otherwise, "RATE_LIMITED" for 429 errors
#' @export
movebank_download_study <- function(study_id, username, password, output_file, 
                                    sensor_type_ids = NULL, quiet = FALSE) {
  base_url <- "https://www.movebank.org/movebank/service/direct-read"
  
  # Step 1: Get license terms
  license_response <- tryCatch({
    httr::GET(
      base_url,
      query = list(entity_type = "event", study_id = study_id),
      httr::authenticate(username, password, type = "basic")
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(license_response)) return(FALSE)
  
  status <- httr::status_code(license_response)
  if (status == 429) return("RATE_LIMITED")
  if (status != 200) return(FALSE)
  
  # Calculate MD5 of license
  license_content <- httr::content(license_response, "raw")
  license_md5 <- digest::digest(license_content, algo = "md5", serialize = FALSE)
  rm(license_response, license_content)
  gc()
  
  # Step 2: Download data
  event_attributes <- paste(c(
    "timestamp", "location_long", "location_lat", "height_above_msl",
    "individual_id", "individual_taxon_canonical_name", "sensor_type_id",
    "visible", "study_id", "gps_hdop", "gps_vdop", "gps_satellite_count"
  ), collapse = ",")
  

  download_url <- paste0(
    base_url,
    "?entity_type=event",
    "&study_id=", study_id,
    "&license-md5=", license_md5,
    "&attributes=", utils::URLencode(event_attributes),
    if (!is.null(sensor_type_ids) && length(sensor_type_ids) > 0) {
      paste0("&sensor_type_id=", paste(sensor_type_ids, collapse = ","))
    } else ""
  )
  
  data_response <- tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h,
      userpwd = paste0(username, ":", password),
      httpauth = 1,
      noprogress = if (quiet) 1L else 0L,
      connecttimeout = 60,
      timeout = 10000,
      low_speed_limit = 100,
      low_speed_time = 300
    )
    
    if (!quiet) {
      curl::handle_setopt(h, progressfunction = function(down_total, down_now, up_total, up_now) {
        size_mb <- round(down_now / 1024^2, 1)
        if (down_total > 0) {
          pct <- round(down_now / down_total * 100)
          total_mb <- round(down_total / 1024^2, 1)
          cat(sprintf("\r  Downloading: %s / %s MB (%d%%)   ", size_mb, total_mb, pct))
        } else if (down_now > 0) {
          cat(sprintf("\r  Downloading: %s MB...   ", size_mb))
        }
        utils::flush.console()
        TRUE
      })
    }
    
    curl::curl_download(download_url, output_file, handle = h)
    if (!quiet) cat("\n")
    TRUE
  }, error = function(e) {
    if (!quiet) cat("\n")
    cli::cli_alert_warning("  Download error: {substr(e$message, 1, 80)}")
    if (file.exists(output_file)) unlink(output_file)
    return(FALSE)
  })
  
  if (!isTRUE(data_response)) return(FALSE)
  
  # Validate
  if (!file.exists(output_file) || file.size(output_file) == 0) {
    return(FALSE)
  }
  
  first_line <- readLines(output_file, n = 1, warn = FALSE)
  if (!grepl("individual_id", first_line, ignore.case = TRUE)) {
    if (grepl("rate limit|503|error|denied|403", first_line, ignore.case = TRUE)) {
      return(FALSE)
    }
  }
  
  TRUE
}