# List of required packages
required_packages <- c(
  "shiny",
  "shinyWidgets",
  "htmltools",
  "sf",
  "stringr",
  "shinythemes",
  "shinyjs",
  "lubridate",
  "memoise",
  "leaflet.extras",
  "leaflet.extras2",
  "remotes" # Needed for GitHub installations
)

# Function to install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    message(paste("Installing package:", package))
    install.packages(package, dependencies = TRUE)
  } else {
    message(paste("Package already installed:", package))
  }
}

# Install CRAN packages
message("Checking and installing required CRAN packages...")
for (package in required_packages) {
  install_if_missing(package)
}

# Install GitHub packages
message("\nChecking and installing GitHub packages...")
if (!require("leaflet.mapboxgl")) {
  message("Installing leaflet.mapboxgl from GitHub...")
  remotes::install_github("rstudio/leaflet.mapboxgl")
} else {
  message("leaflet.mapboxgl already installed")
}

message("\nAll required packages have been installed!")

# Print package versions for reference
message("\nInstalled package versions:")
for (package in c(required_packages, "leaflet.mapboxgl")) {
  if (require(package, character.only = TRUE)) {
    version <- packageVersion(package)
    message(sprintf("%s: %s", package, version))
  }
}
