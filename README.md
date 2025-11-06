# Claims Dashboard

A professional Shiny dashboard for visualizing and analyzing insurance claims data for GIIS and AIMS systems.

## Features

- 📊 Interactive data visualizations with Plotly
- 📈 Real-time claims analytics
- 🎨 Modern UI with gradient backgrounds and glassmorphism effects
- 📱 Responsive design
- 🖨️ PDF export functionality
- 🎯 Dual system support (GIIS & AIMS)

## Deployment to Posit Connect

### Prerequisites

- R (>= 4.0.0)
- rsconnect package installed
- Access to a Posit Connect server

### Step 1: Generate Manifest

Run the manifest generator script to create a `manifest.json` file with your exact package versions:

```r
source("generate_manifest.R")
```

This will:
- Create `manifest.json` in your project root
- Display all package versions being used
- Provide deployment instructions

### Step 2: Commit and Push

```bash
git add manifest.json .gitignore
git commit -m "Add manifest.json for Posit Connect deployment"
git push origin giist_and_aims
```

### Step 3: Deploy to Posit Connect

1. Log into your Posit Connect server
2. Click **"Publish"** → **"Import from Git"**
3. Enter repository URL: `https://github.com/robin-ochieng/Underwriting-Claims-Dashboard.git`
4. Select branch: `giist_and_aims`
5. Posit Connect will automatically detect `app.R`
6. Click **"Deploy"**

The manifest.json ensures all packages are installed with the exact versions from your development environment.

## Required Packages

- shiny
- dplyr
- ggplot2
- DT
- scales
- bs4Dash
- bslib
- plotly
- readxl
- lubridate
- treemapify
- tidyr
- colorspace
- RColorBrewer
- forcats
- shinycssloaders
- shinyjs
- extrafont

## Local Development

```r
# Install required packages
install.packages(c("shiny", "dplyr", "ggplot2", "DT", "scales", "bs4Dash", 
                   "bslib", "plotly", "readxl", "lubridate", "treemapify", 
                   "tidyr", "colorspace", "RColorBrewer", "forcats", 
                   "shinycssloaders", "shinyjs", "extrafont"))

# Run the app
shiny::runApp()
```

## Project Structure

```
├── app.R                          # Main Shiny application
├── manifest.json                  # Posit Connect deployment manifest
├── generate_manifest.R            # Script to generate manifest
├── data/
│   └── Selected_Claims_Data.xlsx  # Claims data
├── modules/
│   ├── GiisModule.R              # GIIS dashboard module
│   ├── AimsModule.R              # AIMS dashboard module
│   ├── customValueBox.R          # Custom value box component
│   └── data_processing.R         # Data processing utilities
├── www/
│   ├── css/
│   │   └── custom_styles.css     # Custom styling
│   ├── svg/
│   │   └── background-pattern.svg # Background pattern
│   ├── images/                    # Logo images
│   └── favicon/                   # Favicon
└── helper_functions/              # Helper functions

```

## Repository

**GitHub Repository:** [https://github.com/robin-ochieng/Underwriting-Claims-Dashboard](https://github.com/robin-ochieng/Underwriting-Claims-Dashboard)

**Branch:** `giist_and_aims`

## License

© 2025 Claims Dashboard. All rights reserved.
 