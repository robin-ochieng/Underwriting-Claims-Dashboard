# Generate manifest.json for Posit Connect deployment
# This script creates a manifest file with exact package versions from your machine

library(rsconnect)

# Set the app directory
app_dir <- getwd()

cat("Generating manifest.json for Posit Connect deployment...\n")
cat("App directory:", app_dir, "\n\n")

# Generate the manifest
tryCatch({
  rsconnect::writeManifest(
    appDir = app_dir,
    appPrimaryDoc = "app.R",
    appMode = "shiny",
    contentCategory = "application"
  )
  
  cat("\n✓ manifest.json created successfully!\n")
  cat("Location:", file.path(app_dir, "manifest.json"), "\n\n")
  
  # Display package versions being used
  cat("Packages included in manifest:\n")
  cat("================================\n")
  
  packages <- c(
    "shiny", "dplyr", "ggplot2", "DT", "scales", "bs4Dash", 
    "bslib", "plotly", "readxl", "lubridate", "treemapify", 
    "tidyr", "colorspace", "RColorBrewer", "forcats", 
    "shinycssloaders", "shinyjs", "extrafont"
  )
  
  for (pkg in packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      version <- packageVersion(pkg)
      cat(sprintf("  %-20s %s\n", pkg, version))
    } else {
      cat(sprintf("  %-20s NOT INSTALLED\n", pkg))
    }
  }
  
  cat("\n")
  cat("Next steps for Posit Connect deployment:\n")
  cat("=========================================\n")
  cat("1. Commit and push manifest.json to your repository\n")
  cat("2. In Posit Connect, create a new deployment\n")
  cat("3. Choose 'Import from Git Repository'\n")
  cat("4. Enter your repository URL\n")
  cat("5. Select the branch: giist_and_aims\n")
  cat("6. Posit Connect will automatically detect app.R\n")
  cat("7. The manifest.json ensures consistent package versions\n")
  
}, error = function(e) {
  cat("\n✗ Error generating manifest:\n")
  cat(e$message, "\n\n")
  cat("Make sure rsconnect package is installed:\n")
  cat("  install.packages('rsconnect')\n")
})
