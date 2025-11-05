library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(bs4Dash)
library(bslib)
library(plotly)
library(readxl)
library(lubridate)
library(treemapify)
library(tidyr)
library(colorspace)
library(RColorBrewer)
library(forcats)
library(shinycssloaders)
library(shinyjs)
library(extrafont)
suppressMessages(loadfonts())

# Load Claims Data 
claims_data <- read_excel("./data/Selected_Claims_Data.xlsx", col_types = c("text", "text", "date", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "date")) %>%
  mutate(
    Year = format(LOSS_DATE, "%Y"),
    Month = format(LOSS_DATE, "%B"),
    Quarter = paste0("Q", lubridate::quarter(LOSS_DATE))
  )

# Source custom modules
source("modules/customValueBox.R")

# Source Dashboard module scripts
source("modules/GiisModule.R", local = TRUE)
source("modules/AimsModule.R", local = TRUE)


# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Nunito"),
  heading_font = font_google("Nunito"),
  code_font = font_google("Nunito"),
  navbar_bg = "#333333", 
  navbar_fg = "#ffffff"  
)

ui <- dashboardPage(
  title = "Claims Dashboard",
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  dashboardHeader(
    title = HTML("<strong style='color: #1A73E8;'>Claims Dashboard</strong>"),
    controlbarIcon = NULL,
    status = "white",
    sidebarIcon = NULL,
    fixed = TRUE
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    tags$div(
      class = "menu-container",
    sidebarMenu(
      menuItem("GIIS", tabName = "dashboard_giis", icon = icon("chart-line")),
      menuItem("AIMS", tabName = "dashboard_aims", icon = icon("bullseye"))
    )),
    div(class = "sidebar-footer",
        img(src = "images/jubilee.png", class = "jubilee-logo"),
        img(src = "images/kenbright.png")
    )
  ),
  dashboardBody(
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('printPage', function(message) {
          window.print();
        });
      ")),
      includeCSS("www/css/custom_styles.css"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon")
    ),
    tabItems(
      tabItem(tabName = "dashboard_giis", giisDashboardUI("giis_dashboard")),
      tabItem(tabName = "dashboard_aims", aimsDashboardUI("aims_dashboard"))
  )
  ),
  footer = bs4DashFooter(
    div(style = "background-color: #ffffff; color: #000000; text-align: center; padding: 6px; font-size: 10px", 
        "© 2025 GIIS & AIMS Dashboard. All rights reserved.")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Claims Data -----------------------------------------------------------------------------------
  # Filtered Claims Data
  filtered_claims_data <- reactive({
    req(claims_data)
    data <- claims_data 
    data
  })

  # GIIS Dashboard Server Module
  giisDashboardServer("giis_dashboard", data = filtered_claims_data)
  
  # AIMS Dashboard Server Module
  aimsDashboardServer("aims_dashboard", data = filtered_claims_data)

}

# Run the application
shinyApp(ui = ui, server = server)