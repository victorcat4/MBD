<p align="center">

<img src="https://img.shields.io/badge/R-package-blue?logo=r&amp;logoColor=white" alt="R Package"/> <img src="https://img.shields.io/badge/Movebank-API-green" alt="Movebank API"/> <img src="https://img.shields.io/badge/License-GPL--3-yellow" alt="GPL-3 License"/>

</p>

<h1 align="center">

MBD

</h1>

<h3 align="center">

Movebank Data Downloader

</h3>

<p align="center">

<em>A simple R package to download and process animal tracking data from Movebank</em>

</p>

------------------------------------------------------------------------

**MBD** lets you download, split, and filter animal tracking data from [Movebank](https://www.movebank.org) directly in R.
Define your target taxa (from species to entire classes like *Aves*), sensors, and an optional geographic polygon in a single YAML config file.
The package handles API authentication, rate limiting, and large file chunking automatically.
Output is organized into per-individual CSV files, ready for analysis.

------------------------------------------------------------------------

## 📦 Installation

``` r
# Install dependencies first
install.packages(c("httr", "curl", "digest", "readr", "dplyr", "sf", "yaml", "cli"))

# Install from GitHub
devtools::install_github("victorcat4/MBD")

# Install from local files
devtools::install_local()
```

------------------------------------------------------------------------

## 🚀 Quick Start

### 1. Create a Configuration File

Create a `config.yaml` file with your settings:

``` yaml
# config.yaml
movebank:
  username: your_username
  password: your_password

taxa:
  - Falco peregrinus
  - Aquila chrysaetos

sensors:
  - GPS
  - Argos Doppler Shift

output_dir: /path/to/output

# Optional: filter by geographic area
polygon:
  coordinates:
    - [10.0, 45.0]
    - [20.0, 45.0]
    - [20.0, 55.0]
    - [10.0, 55.0]
    - [10.0, 45.0]  # close the polygon

# Optional processing settings
processing:
  chunk_size: 50000

# Optional incremental settings
incremental:
  enabled: yes
  min_new_days: 90  # re-download if >90 days of new data
```

You can select sensor among the following list : 

```
| Name                      | id         | 
| ------------------------- | ---------- |
| Bird Ring                 | 397        |
| GPS                       | 653        |
| Radio Transmitter         | 673        |
| Argos Doppler Shift       | 82798      | 
| Natural Mark              | 2365682    | 
| Solar Geolocator          | 3886361    | 
```


### 2. Run the Pipeline

``` r
library(MBD)

# Run complete pipeline
run_pipeline("config.yaml")

# Run specific steps
run_pipeline("config.yaml", steps = "download")
run_pipeline("config.yaml", steps = c("split", "filter"))

# Force re-download everything
run_pipeline("config.yaml", force = TRUE)
```

### 3. Check Status

``` r
pipeline_status("/path/to/output")
```

------------------------------------------------------------------------

## ⚙️ What It Does

| Step | Description |
|:-----------------------|:-----------------------------------------------|
| **Download** | Connects to Movebank, finds studies matching your taxa and sensors, downloads the data |
| **Split** | Splits large study files into individual animal files, organized by species |
| **Filter** | Removes individuals outside your polygon (if specified) and non-matching taxa |

------------------------------------------------------------------------

## 📁 Output Structure

```         
output_dir/
├── raw_studies/              # Downloaded study CSV files
│   ├── MB_123456.csv
│   └── MB_789012.csv
├── individuals/              # Split individual files by species
│   ├── Falco_peregrinus/
│   │   ├── Falco_peregrinus%GPS%ind001.csv
│   │   └── Falco_peregrinus%GPS%ind002.csv
│   └── Aquila_chrysaetos/
│       └── ...
├── metadata/                 # Processing metadata
│   ├── all_studies.csv
│   └── taxonomy_cache/
└── study_timestamps.csv      # For incremental updates
```

------------------------------------------------------------------------

## 📋 Configuration Reference

| Field | Required | Description |
|:-----------------|:----------------------:|:-----------------------------|
| `movebank.username` | ✓ | Your Movebank username |
| `movebank.password` | ✓ | Your Movebank password |
| `taxa` | ✓ | List of taxa to download (species, genus, family, etc.) |
| `sensors` | ✓ | List of sensor types (GPS, Argos Doppler Shift, etc.) |
| `output_dir` | ✓ | Output directory path |
| `polygon.coordinates` |  | List of `[lon, lat]` coordinates defining filter area |
| `processing.chunk_size` |  | Rows per chunk (default: 50000) |
| `incremental.min_new_days` |  | Days of new data before re-download (default: 90) |

------------------------------------------------------------------------

## 🧬 Taxa Expansion

The package automatically expands higher-level taxa to species using GBIF:

| Input              | Expansion                     |
|:-------------------|:------------------------------|
| `Aves`             | → all bird species            |
| `Anatidae`         | → all duck/goose/swan species |
| `Falco peregrinus` | → just peregrine falcon       |

Results are cached locally to speed up subsequent runs.

------------------------------------------------------------------------

## 🔧 Individual Functions

You can also use the package functions individually:

``` r
# Read config
config <- read_config("config.yaml")

# Get list of studies
studies <- movebank_get_studies(username, password)

# Expand taxa
species <- expand_taxa("Anatidae")

# Download a specific study
movebank_download_study(study_id, username, password, "output.csv")
```

------------------------------------------------------------------------

## ❓ Troubleshooting

<details>

<summary><strong>Rate limiting</strong></summary>

Movebank limits requests. The package handles this automatically with exponential backoff.

</details>

<details>

<summary><strong>Large studies</strong></summary>

Some studies are very large (\>1GB). The package processes them in chunks to manage memory.

</details>

<details>

<summary><strong>Authentication errors</strong></summary>

Make sure your Movebank credentials are correct and you have accepted the license terms for the studies you want to download.

</details>

------------------------------------------------------------------------

## 📄 License

GPL-3