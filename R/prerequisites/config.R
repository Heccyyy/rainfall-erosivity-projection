suppressPackageStartupMessages({
  library(ncdf4)
  library(terra)
  library(tidyverse)
  library(lubridate)
  library(viridis)
  library(RColorBrewer)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(zoo)
})

# ===== Project paths ====================
PATHS <- list(
  raw            = "R/esgf",
  processed      = "R/esgf/processed",
  bias_corrected = "R/esgf/bias_corrected",
  results        = "R/results",
  figures        = "R/results/figures",
  tables         = "R/results/tables",
  models         = "R/results/models"
)

# Create all directories if missing
invisible(lapply(PATHS, dir.create, recursive = TRUE, showWarnings = FALSE))

# ===== Jakarta domain ====================
JAKARTA <- list(
  lat_min  = -7.0,
  lat_max  = -5.5,
  lon_min  = 106.0,
  lon_max  = 107.5,
  center_lat = -6.211,   # Jakarta city center
  center_lon = 106.845
)

# ===== Analysis periods ====================
PERIODS <- list(
  historical  = c(1976, 2005),
  calibration = c(1981, 2005),   # CHIRPS overlap
  near_future = c(2021, 2050),
  far_future  = c(2071, 2100)
)

# ===== Scenario metadata ====================
SCENARIOS <- c("historical", "rcp26", "rcp45", "rcp85")

SCENARIO_LABELS <- c(
  historical = "Historical (1976–2005)",
  rcp26      = "RCP 2.6 (low emission)",
  rcp45      = "RCP 4.5 (intermediate)",
  rcp85      = "RCP 8.5 (high emission)"
)

SCENARIO_COLORS <- c(
  historical = "#676767",
  rcp26      = "#2364a5",
  rcp45      = "#37b133",
  rcp85      = "#b02c25"
)

# ===== Constants ====================
WET_DAY_THRESHOLD  <- 1.0    # mm/day
PR_UNIT_CONVERSION <- 86400  # kg m-2 s-1 → mm/day
RETURN_PERIODS     <- c(2, 5, 10, 25, 50, 100)

FLOOD_THRESHOLDS <- list(
  moderate = 100,  # mm/day
  severe   = 150,
  extreme  = 200
)

# ===== Jakarta base map (used across all spatial plots) ====================
get_jakarta_basemap <- function() {
  list(
    land     = ne_countries(scale = "medium", returnclass = "sf"),
    rivers   = ne_download(scale = "medium", type = "rivers_lake_centerlines",
                           category = "physical", returnclass = "sf"),
    bbox_ext = terra::ext(JAKARTA$lon_min, JAKARTA$lon_max,
                          JAKARTA$lat_min, JAKARTA$lat_max)
  )
}

# ===== NetCDF helpers ====================

# Open a CMIP5 or CHIRPS NetCDF and return a terra SpatRasterDataset cropped to Jakarta domain
#' @param path  Path to .nc file
#' @param var   Variable name (e.g. "pr", "precip")
#' @return SpatRaster with time dimension
open_nc_jakarta <- function(path, var = NULL) {
  r <- terra::rast(path)

  # If var specified, select it
  if (!is.null(var)) {
    idx <- grep(var, names(r))
    if (length(idx) == 0) stop(paste("Variable", var, "not found in", path))
    r <- r[[idx]]
  }

  # Crop to Jakarta
  ext_jak <- terra::ext(JAKARTA$lon_min, JAKARTA$lon_max,
                        JAKARTA$lat_min, JAKARTA$lat_max)
  r_crop <- terra::crop(r, ext_jak)
  return(r_crop)
}

# Convert CMIP5 precipitation from kg m-2 s-1 to mm/day
#' @param r SpatRaster of precipitation
convert_pr_units <- function(r) {
  # Check if conversion needed by examining range
  if (terra::global(r, "max", na.rm = TRUE)[[1]] < 1) {
    message("Converting precipitation: kg m-2 s-1 → mm/day (× 86400)")
    r <- r * PR_UNIT_CONVERSION
  }
  return(r)
}

# Extract time vector from a terra SpatRaster
# Returns a Date or POSIXct vector
get_time_vector <- function(r) {
  t <- terra::time(r)
  if (is.null(t)) stop("SpatRaster has no time dimension.")
  return(t)
}

# Get spatial mean time series over Jakarta domain
#' @param r  SpatRaster (time layers)
#' @return data.frame with columns: date, value
spatial_mean_ts <- function(r, dates = NULL) {
  vals <- terra::global(r, "mean", na.rm = TRUE)[[1]]
  if (is.null(dates)) dates <- get_time_vector(r)
  data.frame(date = as.Date(dates), value = vals)
}

# ===== Plotting theme ====================
theme_jakarta <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1),
      plot.subtitle    = element_text(colour = "grey40", size = base_size - 1),
      axis.title       = element_text(size = base_size - 1),
      legend.position  = "bottom",
      legend.key.width = unit(1.5, "cm"),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}

cat("config.R loaded  |  Jakarta domain:",
    JAKARTA$lon_min, "–", JAKARTA$lon_max, "°E,",
    JAKARTA$lat_min, "–", JAKARTA$lat_max, "°N\n")
