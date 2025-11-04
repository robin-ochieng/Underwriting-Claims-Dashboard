# UI for GIIS dashboard including graphs
giisDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    # 📄 Print-Only Report Title and Logo
    tags$div(
      class = "print-title",
      style = "text-align: center; margin-bottom: 20px;",
      tags$img(src = "images/jubilee.png", style = "height: 60px; margin-bottom: 10px;"),
      tags$h2("GIIS Dashboard Report"),
      tags$p(format(Sys.Date(), "%B %d, %Y"), style = "font-size: 14px;")
    ),
    actionButton(ns("print_dashboard"), "Print as PDF", icon = icon("print"), class = "btn btn-primary control-button"),
    fluidRow(
      class = "value-box-row",
      column(
        width = 4,
        uiOutput(ns("total_paid_os"))
      ),
      column(
        width = 4,
        uiOutput(ns("total_recovery"))
      ),
      column(
        width = 4,
        uiOutput(ns("total_xol_claims"))
      )
    ),
    fluidRow(
      column(12,
        div(class = "filters-section no-print",
            div(class = "filters-header", h5("Filter by Policy Inception Period", class = "filters-title"), actionButton(ns("reset_filters"), "Reset Filters", class = "btn-reset-filters")),
            div(class = "premium-filters-container",
                div(class = "filter-item", selectInput(ns("claims_year"), "Year", choices = NULL, selected = "Select Year")),
                div(class = "filter-item", selectInput(ns("claims_quarter"), "Quarter", choices = NULL, selected = "Select Quarter")),
                div(class = "filter-item", selectInput(ns("claims_month"), "Month", choices = NULL, selected = "Select Month"))
            )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Gross Claims by Month",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("claims_by_month")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Claim Count by Month",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("claimcount_by_month")), type = 6)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Gross Claims by Quarter",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("claims_by_quarter")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Claim Count by Quarter",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("claimcount_by_quarter")), type = 6)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Average Time to Payment by Subclass",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotOutput(ns("approval_time_boxplot")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Average Time to Payment by Branch",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotOutput(ns("approval_time_by_branch")), type = 6)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Gross Claims by Class",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("claims_by_class")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Claim Count by Class",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("count_by_class")), type = 6)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Gross Claims by Branch",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("claims_by_branch")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Claim Count by Branch",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("count_by_branch")), type = 6)
        )
      )
    )

  )
}


# Server logic for GIIS dashboard
giisDashboardServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  

    observeEvent(input$print_dashboard, {
      session$sendCustomMessage(type = "printPage", message = list())
    })

    observe({
      req(data())
      updateSelectInput(session, "claims_year",
                        choices = c("Select Year", sort(unique(data()$Year))),
                        selected = "Select Year")
      quarter_order <- c("Q1", "Q2", "Q3", "Q4")
      available_quarters <- intersect(quarter_order, unique(data()$Quarter))
      updateSelectInput(session, "claims_quarter",
                        choices = c("Select Quarter", available_quarters),
                        selected = "Select Quarter")
      month_order <- month.name
      available_months <- intersect(month_order, unique(data()$Month))
      updateSelectInput(session, "claims_month",
                        choices = c("Select Month", available_months),
                        selected = "Select Month")
    })

    filtered_data <- reactive({
      df <- data()
      req(input$claims_year, input$claims_quarter, input$claims_month)
      if (input$claims_year != "Select Year") {
        df <- df %>% filter(Year == input$claims_year)
      }
      if (input$claims_quarter != "Select Quarter") {
        df <- df %>% filter(Quarter == input$claims_quarter)
      }
      if (input$claims_month != "Select Month") {
        df <- df %>% filter(Month == input$claims_month)
      }
      df
    })

    observeEvent(input$reset_filters, {
      updateSelectInput(session, "claims_year", selected = "Select Year")
      updateSelectInput(session, "claims_quarter", selected = "Select Quarter")
      updateSelectInput(session, "claims_month", selected = "Select Month")
    })

    # Total Paid + Outstanding
    output$total_paid_os <- renderUI({
      df <- filtered_data()
      total_paid_os <- sum(df$PAID_OS, na.rm = TRUE)
      customValueBox("Total Paid/OS", comma(total_paid_os), "#2980B9")
    })

    # Total Recovery
    output$total_recovery <- renderUI({
      df <- filtered_data()
      total_recovery <- sum(df$RECOVARY, na.rm = TRUE)
      customValueBox("Total Recoveries", comma(total_recovery), "#27AE60")
    })

    # Total XOL Claims
    output$total_xol_claims <- renderUI({
      df <- filtered_data()
      total_xol <- sum(df$XOL_AMOUNT, na.rm = TRUE)
      customValueBox("XOL Claims Incurred", comma(total_xol), "#E67E22")
    })

    output$claims_by_month <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(LOSS_DATE)) %>%
        mutate(Month = format(as.Date(LOSS_DATE), "%B")) %>%
        mutate(Month = factor(Month, levels = month.name)) %>%
        group_by(Month) %>%
        summarise(TotalClaims = sum(PAID_OS, na.rm = TRUE)) %>%
        mutate(
          Label = case_when(
            TotalClaims >= 1e6 ~ paste0(formatC(TotalClaims / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalClaims >= 1e3 ~ paste0(formatC(TotalClaims / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalClaims, format = "f", digits = 0, big.mark = ",")
          )
        )
      plot_ly(df, 
              x = ~Month, 
              y = ~TotalClaims, 
              type = 'scatter', 
              mode = 'lines+markers+text',
              text = ~Label,
              textposition = 'top center',              
              textfont = list(size = 8, color = 'black'),
              hoverinfo = 'text',
              line = list(color = '#00BFA5'),
              marker = list(size = 6)) %>%
        layout(
          title = list(
            text = "Monthly Gross Claims Trend",
            x = 0.01,  # left-align title
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 60), 
          xaxis = list(title = "Month", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Total Claims (KES)", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    # Policy Count by SUB_CLASSNAMEN
    output$claimcount_by_month <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(LOSS_DATE)) %>%
        mutate(Month = format(as.Date(LOSS_DATE), "%B")) %>%
        mutate(Month = factor(Month, levels = month.name)) %>%
        group_by(Month) %>%
        summarise(ClaimCount = n()) %>%
        mutate(
          Label = case_when(
            ClaimCount >= 1e3 ~ paste0(formatC(ClaimCount / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(ClaimCount, format = "d", big.mark = ",")
          )
        )

      plot_ly(df, 
              x = ~Month, 
              y = ~ClaimCount, 
              type = 'scatter', 
              mode = 'lines+markers+text',
              text = ~Label,
              textposition = 'top center',
              textfont = list(size = 8, color = 'black'),
              hoverinfo = 'text',
              line = list(color = '#EA80FC'),
              marker = list(size = 6)) %>%
        layout(
          title = list(
            text = "Monthly Claim Count Trend",
            x = 0.01,  # left-align title
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 30, t = 20), 
          xaxis = list(title = "Month", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Count", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$claims_by_quarter <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(LOSS_DATE)) %>%
        mutate(Quarter = paste0("Q", quarter(as.Date(LOSS_DATE)))) %>%
        mutate(Quarter = factor(Quarter, levels = c("Q1", "Q2", "Q3", "Q4"))) %>%
        group_by(Quarter) %>%
        summarise(TotalClaims = sum(PAID_OS, na.rm = TRUE)) %>%
        mutate(
          Label = case_when(
            TotalClaims >= 1e6 ~ paste0(formatC(TotalClaims / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalClaims >= 1e3 ~ paste0(formatC(TotalClaims / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalClaims, format = "f", digits = 0, big.mark = ",")
          )
        )

      plot_ly(df, 
              x = ~Quarter, 
              y = ~TotalClaims, 
              type = 'scatter', 
              mode = 'lines+markers+text',
              text = ~Label,
              textposition = 'top center',
              textfont = list(size = 8, color = 'black'),
              hoverinfo = 'text',
              line = list(color = '#00BFA5'),
              marker = list(size = 6)) %>%
        layout(
          title = list(
            text = "Quarterly Gross Claims Trend",
            x = 0.01,
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 50),
          xaxis = list(title = "Quarter", tickfont = list(size = 10)),
          yaxis = list(title = "Total Claims (KES)", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$claimcount_by_quarter <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(LOSS_DATE)) %>%
        mutate(Quarter = paste0("Q", quarter(as.Date(LOSS_DATE)))) %>%
        mutate(Quarter = factor(Quarter, levels = c("Q1", "Q2", "Q3", "Q4"))) %>%
        group_by(Quarter) %>%
        summarise(ClaimCount = n()) %>%
        mutate(
          Label = case_when(
            ClaimCount >= 1e3 ~ paste0(formatC(ClaimCount / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(ClaimCount, format = "d", big.mark = ",")
          )
        )

      plot_ly(df, 
              x = ~Quarter, 
              y = ~ClaimCount, 
              type = 'scatter', 
              mode = 'lines+markers+text',
              text = ~Label,
              textposition = 'top center',
              textfont = list(size = 8, color = 'black'),
              hoverinfo = 'text',
              line = list(color = '#EA80FC'),
              marker = list(size = 6)) %>%
        layout(
          title = list(
            text = "Quarterly Claim Count Trend",
            x = 0.01,
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 50),
          xaxis = list(title = "Quarter", tickfont = list(size = 10)),
          yaxis = list(title = "Claim Count", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })


    output$approval_time_boxplot <- renderPlot({
      df <- filtered_data()

      # Ensure both dates are properly parsed
      df <- df %>%
        filter(!is.na(LOSS_DATE), !is.na(APPRV_DATE1)) %>%
        mutate(
          LOSS_DATE = as.Date(LOSS_DATE),
          APPRV_DATE1 = as.Date(APPRV_DATE1),
          DaysToApproval = as.numeric(APPRV_DATE1 - LOSS_DATE)
        ) %>%
        filter(DaysToApproval >= 0 & DaysToApproval < 365)  # Remove negative or extreme values

      # Aesthetic boxplot grouped by Subclass
      ggplot(df, aes(x = fct_reorder(SUBCLASS_NAME, DaysToApproval, .fun = median, .desc = FALSE), 
                    y = DaysToApproval, fill = SUBCLASS_NAME)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 1, alpha = 0.7) +
        labs(
          title = "Distribution of Time from Loss to Payment by Subclass",
          x = "Subclass",
          y = "Days Between Loss and Paid Date"
        ) +
        theme_minimal(base_family = "Mulish") +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        scale_fill_viridis_d()
    })

    output$approval_time_by_branch <- renderPlot({
      df <- filtered_data()

      df <- df %>%
        filter(!is.na(LOSS_DATE), !is.na(APPRV_DATE1)) %>%
        mutate(
          LOSS_DATE = as.Date(LOSS_DATE),
          APPRV_DATE1 = as.Date(APPRV_DATE1),
          DaysToApproval = as.numeric(APPRV_DATE1 - LOSS_DATE)
        ) %>%
        filter(DaysToApproval >= 0 & DaysToApproval < 365)

      ggplot(df, aes(x = fct_reorder(BRANCH_NAME1, DaysToApproval, .fun = median, .desc = FALSE), 
                    y = DaysToApproval, fill = BRANCH_NAME1)) +
        geom_boxplot(outlier.color = "darkred", outlier.shape = 16, alpha = 0.7) +
        labs(
          title = "Distribution of Time from Loss to Payment by Branch",
          x = "Branch",
          y = "Days Between Loss and Payment"
        ) +
        theme_minimal(base_family = "Mulish") +
        theme(
          plot.title = element_text(size = 16, hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        scale_fill_viridis_d()
    })

    # Total Gross Claims by CLASS_DESCRIPTION
    output$claims_by_class <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(SUBCLASS_NAME)) %>%
        group_by(SUBCLASS_NAME) %>%
        summarise(TotalClaims = sum(PAID_OS, na.rm = TRUE)) %>%
        arrange(desc(TotalClaims)) %>%
        mutate(
          Label = case_when(
            TotalClaims >= 1e6 ~ paste0(formatC(TotalClaims / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalClaims >= 1e3 ~ paste0(formatC(TotalClaims / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalClaims, format = "f", digits = 0, big.mark = ",")
          )
        )

      plot_ly(df, 
              x = ~fct_reorder(SUBCLASS_NAME, -TotalClaims), 
              y = ~TotalClaims, 
              type = 'bar',
              text = ~Label,
              textfont = list(size = 9, color = "black"),
              textposition = 'outside',
              hoverinfo = 'text',
              hovertext = ~paste("Class:", SUBCLASS_NAME, "<br>Total Claims:", scales::comma(TotalClaims), "KES"),
              marker = list(color = '#00BFA5')) %>%
        layout(
          title = list(
            text = "Gross Claim by Class",
            uniformtext = list(minsize = 11, mode = 'show'), 
            x = 0.01,  # left-align title
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 100), 
          xaxis = list(title = "Class", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Total Claims (KES)", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    # Policy Count by SUBCLASS_NAMEN
    output$count_by_class <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(SUBCLASS_NAME)) %>%
        group_by(SUBCLASS_NAME) %>%
        summarise(ClaimCount = n()) %>%
        arrange(desc(ClaimCount))%>%
        mutate(
          Label = case_when(
            ClaimCount >= 1e3 ~ paste0(formatC(ClaimCount / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(ClaimCount, format = "d", big.mark = ",")
          )
        )

      plot_ly(df, x = ~fct_reorder(SUBCLASS_NAME, -ClaimCount), y = ~ClaimCount, type = 'bar',
              text = ~Label,
              textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(size = 9, color = "black"),
              hovertext = ~paste("Class:", SUBCLASS_NAME, "<br>Policies:", formatC(ClaimCount, format = "d", big.mark = ",")),
              marker = list(color = '#EA80FC')) %>%
        layout(
          title = list(
            text = "Claim Count by Class",
            uniformtext = list(minsize = 11, mode = 'show'), 
            x = 0.01,  # left-align title
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 30, t = 20), 
          xaxis = list(title = "Class", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Count", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$claims_by_branch <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(BRANCH_NAME1)) %>%
        group_by(BRANCH_NAME1) %>%
        summarize(TotalClaims = sum(PAID_OS, na.rm = TRUE)) %>%
        mutate(
          Label = case_when(
            TotalClaims >= 1e6 ~ paste0(formatC(TotalClaims / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalClaims >= 1e3 ~ paste0(formatC(TotalClaims / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalClaims, format = "f", digits = 0, big.mark = ",")
          ),
          Branch = fct_reorder(BRANCH_NAME1, TotalClaims)
        )%>%
        arrange(TotalClaims)

      plot_ly(
        df,
        x = ~TotalClaims,
        y = ~Branch,
        type = 'bar',
        orientation = 'h',
        marker = list(color = '#80FCEB'),
        text = ~Label,
        textposition = 'auto',
        textfont = list(size = 9, color = "#333333"),
        hoverinfo = 'text',
        hovertext = ~paste("Branch:", BRANCH_NAME1, "<br>Total claims:", Label)
      ) %>%
        layout(
          title = list(text = "claims by Branch", x = 0.01, xanchor = "left", font = list(size = 14)),
          yaxis = list(title = "", tickfont = list(size = 8, color = "#333333")),
          xaxis = list(title = "Total claims (KES)", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          margin = list(l = 10, r = 80, b = 10, t = 30),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$count_by_branch <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(BRANCH_NAME1)) %>%
        count(BRANCH_NAME1) %>%
        mutate(
          Label = case_when(
            n >= 1e3 ~ paste0(formatC(n / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(n, format = "d", big.mark = ",")
          ),
          Branch = fct_reorder(BRANCH_NAME1, n)
        )
      plot_ly(
        df,
        x = ~n,
        y = ~Branch,
        type = 'bar',
        orientation = 'h',
        marker = list(color = '#EA80FC'),
        text = ~Label,
        textposition = 'auto',
        textfont = list(size = 9, color = "#333333"),
        hoverinfo = 'text',
        hovertext = ~paste("Branch:", BRANCH_NAME1, "<br>Policies:", Label)
      ) %>%
        layout(
          title = list(text = "Claim Count by Branch", x = 0.01, xanchor = "left", font = list(size = 14)),
          yaxis = list(title = "", tickfont = list(size = 8, color = "#333333")),
          xaxis = list(title = "Claim Count", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          margin = list(l = 100, r = 10, b = 10, t = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })


  })
}