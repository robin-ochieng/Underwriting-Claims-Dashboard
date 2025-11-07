# UI for AIMS dashboard including graphs
aimsDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    # 📄 Print-Only Report Title and Logo
    tags$div(
      class = "print-title",
      style = "text-align: center; margin-bottom: 20px;",
      tags$img(src = "images/jubilee.png", style = "height: 60px; margin-bottom: 10px;"),
      tags$h2("AIMS Dashboard Report"),
      tags$p(format(Sys.Date(), "%B %d, %Y"), style = "font-size: 14px;")
    ),
    actionButton(ns("print_dashboard"), "Print as PDF", icon = icon("print"), class = "btn btn-primary control-button"),
    fluidRow(
      class = "value-box-row",
      column(width = 3, uiOutput(ns("total_paid_os"))),
      column(width = 3, uiOutput(ns("total_outstanding"))),
      column(width = 3, uiOutput(ns("avg_cost_paid"))),
      column(width = 3, uiOutput(ns("total_reported")))
    ),
    fluidRow(
      class = "value-box-row",
      column(width = 3, uiOutput(ns("settlement_ratio"))),
      column(width = 3, uiOutput(ns("median_days_payment"))),
      column(width = 3, uiOutput(ns("largest_claim_paid"))),
      column(width = 3, uiOutput(ns("top_subclass_paid_share")))
    ),
    fluidRow(
      column(12,
        div(class = "filters-section no-print",
            div(class = "filters-header", 
                h5("Filter by Loss Period", class = "filters-title"), 
                actionButton(ns("reset_filters"), HTML("<i class='fa fa-undo'></i> Reset Filters"), class = "btn-reset-filters")
            ),
            div(class = "premium-filters-container",
                div(class = "filter-item", 
                    tags$label(HTML("<i class='fa fa-calendar'></i> Year"), `for` = ns("claims_year")),
                    selectInput(ns("claims_year"), NULL, choices = NULL, selected = "Select Year")
                ),
                div(class = "filter-item", 
                    tags$label(HTML("<i class='fa fa-chart-bar'></i> Quarter"), `for` = ns("claims_quarter")),
                    selectInput(ns("claims_quarter"), NULL, choices = NULL, selected = "Select Quarter")
                ),
                div(class = "filter-item", 
                    tags$label(HTML("<i class='fa fa-calendar-alt'></i> Month"), `for` = ns("claims_month")),
                    selectInput(ns("claims_month"), NULL, choices = NULL, selected = "Select Month")
                )
            )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Gross Claims by Loss Month",
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
          title = "Claim Count by Loss Month",
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
          title = "Claim Count by Day of Week",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("claimcount_by_weekday")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Gross Claims by Day of Week",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("claims_by_weekday")), type = 6)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Gross Claims by Loss Quarter",
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
          title = "Claim Count by Loss Quarter",
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
    )

  )
}


# Server logic for AIMS dashboard
aimsDashboardServer <- function(id, paid_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  

    observeEvent(input$print_dashboard, {
      session$sendCustomMessage(type = "printPage", message = list())
    })

    observe({
      req(paid_data())
      updateSelectInput(session, "claims_year",
                        choices = c("Select Year", sort(unique(paid_data()$Year))),
                        selected = "Select Year")
      quarter_order <- c("Q1", "Q2", "Q3", "Q4")
      available_quarters <- intersect(quarter_order, unique(paid_data()$Quarter))
      updateSelectInput(session, "claims_quarter",
                        choices = c("Select Quarter", available_quarters),
                        selected = "Select Quarter")
      month_order <- month.name
      available_months <- intersect(month_order, unique(paid_data()$Month))
      updateSelectInput(session, "claims_month",
                        choices = c("Select Month", available_months),
                        selected = "Select Month")
    })

    filtered_data <- reactive({
      df <- paid_data()
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

    # Helper reactives for KPI calculations
    get_paid_total <- reactive({
      filtered_data() %>%
        filter(Category == "Paid") %>%
        summarise(val = sum(PAID_OS, na.rm = TRUE)) %>%
        dplyr::pull(val) %>%
        {
          if (length(.) == 0 || is.na(.)) 0 else .
        }
    })

    get_out_total <- reactive({
      filtered_data() %>%
        filter(Category == "Outstanding") %>%
        summarise(val = sum(PAID_OS, na.rm = TRUE)) %>%
        dplyr::pull(val) %>%
        {
          if (length(.) == 0 || is.na(.)) 0 else .
        }
    })

    # Total Paid Claims
    output$total_paid_os <- renderUI({
      total_paid <- get_paid_total()
      customValueBox("Total Paid Claims", scales::comma(total_paid), "#2980B9")
    })

    # Total Outstanding Claims
    output$total_outstanding <- renderUI({
      total_out <- get_out_total()
      customValueBox("Total Outstanding Claims", scales::comma(total_out), "#27AE60")
    })

    # Average Cost per Paid Claim
    output$avg_cost_paid <- renderUI({
      df_paid <- filtered_data() %>% filter(Category == "Paid")
      n_paid <- nrow(df_paid)
      paid_total <- sum(df_paid$PAID_OS, na.rm = TRUE)
      avg_paid <- ifelse(n_paid > 0, paid_total / n_paid, 0)
      customValueBox("Average Cost per Paid Claim", scales::comma(avg_paid), "#E67E22")
    })

    # Total Reported Claims
    output$total_reported <- renderUI({
      df <- filtered_data()
      # Check if the column exists, try both with and without backticks
      if ("Claim No" %in% names(df)) {
        reported_count <- df %>% distinct(`Claim No`) %>% nrow()
      } else if ("Claim.No" %in% names(df)) {
        reported_count <- df %>% distinct(Claim.No) %>% nrow()
      } else if ("CLAIM_NO" %in% names(df)) {
        reported_count <- df %>% distinct(CLAIM_NO) %>% nrow()
      } else {
        # Fallback: count all rows if column not found
        reported_count <- nrow(df)
      }
      reported_count <- ifelse(is.na(reported_count) || length(reported_count) == 0, 0, reported_count)
      customValueBox("Total Reported Claims", scales::comma(reported_count), "#17A2B8")
    })

    # Settlement Ratio
    output$settlement_ratio <- renderUI({
      paid_total <- get_paid_total()
      out_total <- get_out_total()
      denom <- paid_total + out_total
      ratio <- ifelse(denom > 0, paid_total / denom, 0)
      customValueBox("Settlement Ratio", scales::percent(ratio, accuracy = 0.1), "#6C5CE7")
    })

    # ---- 1) Median Days to Payment (Loss -> Approval for Paid claims) ----
    output$median_days_payment <- renderUI({
      df <- filtered_data() %>%
        dplyr::filter(Category == "Paid", !is.na(LOSS_DATE), !is.na(APPRV_DATE1)) %>%
        dplyr::mutate(
          LOSS_DATE = as.Date(LOSS_DATE),
          APPRV_DATE1 = as.Date(APPRV_DATE1),
          DaysToPayment = as.numeric(APPRV_DATE1 - LOSS_DATE)
        ) %>%
        dplyr::filter(DaysToPayment >= 0 & DaysToPayment < 3650)  # guard rails

      med_days <- if (nrow(df) > 0) stats::median(df$DaysToPayment, na.rm = TRUE) else 0
      customValueBox("Median Days to Payment", scales::comma(med_days), "#7F8CFF")
    })

    # ---- 2) Largest Single Claim Paid ----
    output$largest_claim_paid <- renderUI({
      df_paid <- filtered_data() %>%
        dplyr::filter(Category == "Paid")

      max_paid <- if (nrow(df_paid) > 0) max(df_paid$PAID_OS, na.rm = TRUE) else 0
      max_paid <- ifelse(is.na(max_paid) || is.infinite(max_paid), 0, max_paid)
      customValueBox("Largest Single Claim Paid", scales::comma(max_paid), "#FF7675")
    })

    # ---- 3) Top Subclass by Paid (Share %) ----
    output$top_subclass_paid_share <- renderUI({
      df_paid <- filtered_data() %>%
        dplyr::filter(Category == "Paid", !is.na(SUBCLASS_NAME)) %>%
        dplyr::group_by(SUBCLASS_NAME) %>%
        dplyr::summarise(Paid = sum(PAID_OS, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(Paid))

      total_paid <- get_paid_total()
      if (nrow(df_paid) == 0 || total_paid <= 0) {
        return(customValueBox("Top Class by Paid (Share)", "—", "#00B894"))
      }

      top_row <- df_paid[1, ]
      share <- as.numeric(top_row$Paid) / total_paid
      subtitle <- paste0(as.character(top_row$SUBCLASS_NAME), " · ",
                         scales::percent(share, accuracy = 0.1))

      customValueBox("Top Class by Paid (Share)", subtitle, "#00B894")
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

    output$claimcount_by_weekday <- renderPlotly({
      days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      df <- filtered_data() %>%
        filter(!is.na(LOSS_DATE)) %>%
        mutate(
          DayOfWeek = wday(as.Date(LOSS_DATE), label = TRUE, abbr = FALSE, week_start = 1),
          DayOfWeek = factor(as.character(DayOfWeek), levels = days_order)
        ) %>%
        group_by(DayOfWeek) %>%
        summarise(ClaimCount = n()) %>%
        tidyr::complete(DayOfWeek = factor(days_order, levels = days_order), fill = list(ClaimCount = 0)) %>%
        ungroup() %>%
        mutate(
          Label = case_when(
            ClaimCount >= 1e3 ~ paste0(formatC(ClaimCount / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(ClaimCount, format = "d", big.mark = ",")
          ),
          DayLabel = as.character(DayOfWeek)
        )

      plot_ly(
        df,
        x = ~DayOfWeek,
        y = ~ClaimCount,
        type = 'bar',
        text = ~Label,
        textposition = 'outside',
        textfont = list(size = 9, color = "black"),
        hoverinfo = 'text',
        hovertext = ~paste("Day:", DayLabel, "<br>Claim Count:", formatC(ClaimCount, format = "d", big.mark = ",")),
        marker = list(color = '#EA80FC')
      ) %>%
        layout(
          title = list(text = "Claim Count by Day of Week", x = 0.01, xanchor = "left", font = list(size = 14)),
          margin = list(b = 60, t = 40),
          xaxis = list(title = "Day", tickfont = list(size = 10)),
          yaxis = list(title = "Claim Count", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$claims_by_weekday <- renderPlotly({
      days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      df <- filtered_data() %>%
        filter(!is.na(LOSS_DATE)) %>%
        mutate(
          DayOfWeek = wday(as.Date(LOSS_DATE), label = TRUE, abbr = FALSE, week_start = 1),
          DayOfWeek = factor(as.character(DayOfWeek), levels = days_order)
        ) %>%
        group_by(DayOfWeek) %>%
        summarise(TotalClaims = sum(PAID_OS, na.rm = TRUE)) %>%
        tidyr::complete(DayOfWeek = factor(days_order, levels = days_order), fill = list(TotalClaims = 0)) %>%
        ungroup() %>%
        mutate(
          Label = case_when(
            TotalClaims >= 1e6 ~ paste0(formatC(TotalClaims / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalClaims >= 1e3 ~ paste0(formatC(TotalClaims / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalClaims, format = "f", digits = 0, big.mark = ",")
          ),
          DayLabel = as.character(DayOfWeek)
        )

      plot_ly(
        df,
        x = ~DayOfWeek,
        y = ~TotalClaims,
        type = 'bar',
        text = ~Label,
        textposition = 'outside',
        textfont = list(size = 9, color = "black"),
        hoverinfo = 'text',
        hovertext = ~paste("Day:", DayLabel, "<br>Total Claims:", scales::comma(TotalClaims), " KES"),
        marker = list(color = '#00BFA5')
      ) %>%
        layout(
          title = list(text = "Gross Claims by Day of Week", x = 0.01, xanchor = "left", font = list(size = 14)),
          margin = list(b = 60, t = 40),
          xaxis = list(title = "Day", tickfont = list(size = 10)),
          yaxis = list(title = "Total Claims (KES)", tickfont = list(size = 10)),
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
          title = "Distribution of Time from Loss to Payment by Class",
          x = "Class",
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

  })
}