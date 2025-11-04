# UI for sales dashboard including graphs
premiumsDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    # 📄 Print-Only Report Title and Logo
    tags$div(
      class = "print-title",
      style = "text-align: center; margin-bottom: 20px;",
      tags$img(src = "images/jubilee.png", style = "height: 60px; margin-bottom: 10px;"),
      tags$h2("Premium Dashboard Report"),
      tags$p(format(Sys.Date(), "%B %d, %Y"), style = "font-size: 14px;")
    ),
    actionButton(ns("print_dashboard"), "Print as PDF", icon = icon("print"), class = "btn btn-primary control-button"),
    fluidRow(
      class = "value-box-row",
      column(
        width = 4,
        uiOutput(ns("total_gross_premium"))
      ),
      column(
        width = 4,
        uiOutput(ns("total_commission_expense"))
      ),
      column(
        width = 4,
        uiOutput(ns("total_reinsurance_ceded"))
      )
    ), 
    fluidRow(
      column(12,
        div(class = "filters-section no-print",
            div(class = "filters-header", 
                h5("Filter by Policy Inception Period", class = "filters-title"), 
                actionButton(ns("reset_filters"), "Reset Filters", class = "btn-reset-filters")
              ),
            div(class = "premium-filters-container",
                div(class = "filter-item", selectInput(ns("premium_year"), "Year", choices = NULL, selected = "Select Year")),
                div(class = "filter-item", selectInput(ns("premium_quarter"), "Quarter", choices = NULL, selected = "Select Quarter")),
                div(class = "filter-item", selectInput(ns("premium_month"), "Month", choices = NULL, selected = "Select Month"))
            )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Gross Premium by Month",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("premium_by_month")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Premium Count by Month",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("premiumCount_by_month")), type = 6)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Gross Premium by Quarter",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("premium_by_quarter")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Premium Count by Quarter",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotlyOutput(ns("premiumcount_by_quarter")), type = 6)
        )
      )
    ),
    fluidRow( 
        bs4Card(
          title = "Premium by Class",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("premium_by_class")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Policy Count by Class",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("count_by_class")) %>% withSpinner(type = 6)
        ),   
        bs4Card(
          title = "Premium by Customer Category",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotOutput(ns("premium_by_customer_category")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Policy Count by Customer Category",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotOutput(ns("count_by_customer_category")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Premium by Segment (Corporate vs Retail)",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("premium_by_segment")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Policy Count by Segment (Corporate vs Retail)",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("count_by_segment")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Premium by Branch",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("premium_by_branch")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Policy Count by Branch",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("count_by_branch")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Premium by Business Type",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("premium_by_business_type")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Policy Count by Business Type",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("count_by_business_type")) %>% withSpinner(type = 6)
        )         
    )
  )
}


# Server logic for sales dashboard
premiumsDashboardServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace function to handle IDs

    observeEvent(input$print_dashboard, {
      session$sendCustomMessage(type = "printPage", message = list())
    })


    observe({
      req(data())
      updateSelectInput(session, "premium_year",
                        choices = c("Select Year", sort(unique(data()$Year))),
                        selected = "Select Year")
      quarter_order <- c("Q1", "Q2", "Q3", "Q4")
      available_quarters <- intersect(quarter_order, unique(data()$Quarter))
      updateSelectInput(session, "premium_quarter",
                        choices = c("Select Quarter", available_quarters),
                        selected = "Select Quarter")
      month_order <- month.name
      available_months <- intersect(month_order, unique(data()$Month))
      updateSelectInput(session, "premium_month",
                        choices = c("Select Month", available_months),
                        selected = "Select Month")
    })

    filtered_data <- reactive({
      df <- data()
      req(input$premium_year, input$premium_quarter, input$premium_month)
      if (input$premium_year != "Select Year") {
        df <- df %>% filter(Year == input$premium_year)
      }
      if (input$premium_quarter != "Select Quarter") {
        df <- df %>% filter(Quarter == input$premium_quarter)
      }
      if (input$premium_month != "Select Month") {
        df <- df %>% filter(Month == input$premium_month)
      }
      df
    })

    observeEvent(input$reset_filters, {
      updateSelectInput(session, "premium_year", selected = "Select Year")
      updateSelectInput(session, "premium_quarter", selected = "Select Quarter")
      updateSelectInput(session, "premium_month", selected = "Select Month")
    })

    # Total Gross Premium
    output$total_gross_premium <- renderUI({
      df <- filtered_data()
      total <- sum(df$BASE_PREMIUM, na.rm = TRUE)
      customValueBox("Total Gross Premium", comma(total), "#2176C7")
    })

    # Total Commission Expense (Broker + QS + Surplus + FAC)
    output$total_commission_expense <- renderUI({
      df <- filtered_data()
      total_comm <- sum(df$BROKER_COMM, df$QS_COMM, df$SURPLUS_01_COMM, df$FAC_COMM, na.rm = TRUE)
      customValueBox("Commission Expense", comma(total_comm), "#27ae60")
    })

    # Total Reinsurance Ceded (BASE_PREMIUM - RETN)
    output$total_reinsurance_ceded <- renderUI({
      df <- filtered_data()
      ceded <- sum(df$BASE_PREMIUM - df$RETN, na.rm = TRUE)
      customValueBox("Reinsurance Ceded", comma(ceded), "#F39C12")
    })
    


    output$premium_by_month <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(POLICY_FROM_DATE)) %>%
        mutate(Month = format(as.Date(POLICY_FROM_DATE), "%B")) %>%
        mutate(Month = factor(Month, levels = month.name)) %>%
        group_by(Month) %>%
        summarise(TotalPremiums = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        mutate(
          Label = case_when(
            TotalPremiums >= 1e6 ~ paste0(formatC(TotalPremiums / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalPremiums >= 1e3 ~ paste0(formatC(TotalPremiums / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalPremiums, format = "f", digits = 0, big.mark = ",")
          )
        )
      plot_ly(df, 
              x = ~Month, 
              y = ~TotalPremiums, 
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
            text = "Monthly Premium Trend",
            x = 0.01,  # left-align title
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 60), 
          xaxis = list(title = "Month", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Total Premium (KES)", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    # Policy Count by SUB_CLASSNAMEN
    output$premiumCount_by_month <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(POLICY_FROM_DATE)) %>%
        mutate(Month = format(as.Date(POLICY_FROM_DATE), "%B")) %>%
        mutate(Month = factor(Month, levels = month.name)) %>%
        group_by(Month) %>%
        summarise(PremiumCount = n()) %>%
        mutate(
          Label = case_when(
            PremiumCount >= 1e3 ~ paste0(formatC(PremiumCount / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(PremiumCount, format = "d", big.mark = ",")
          )
        )

      plot_ly(df, 
              x = ~Month, 
              y = ~PremiumCount, 
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
            text = "Monthly Premium Count Trend",
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

    output$premium_by_quarter <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(POLICY_FROM_DATE)) %>%
        mutate(Quarter = paste0("Q", quarter(as.Date(POLICY_FROM_DATE)))) %>%
        mutate(Quarter = factor(Quarter, levels = c("Q1", "Q2", "Q3", "Q4"))) %>%
        group_by(Quarter) %>%
        summarise(TotalPremiums = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        mutate(
          Label = case_when(
            TotalPremiums >= 1e6 ~ paste0(formatC(TotalPremiums / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalPremiums >= 1e3 ~ paste0(formatC(TotalPremiums / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalPremiums, format = "f", digits = 0, big.mark = ",")
          )
        )

      plot_ly(df, 
              x = ~Quarter, 
              y = ~TotalPremiums, 
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
            text = "Quarterly Gross Premium Trend",
            x = 0.01,
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 50),
          xaxis = list(title = "Quarter", tickfont = list(size = 10)),
          yaxis = list(title = "Total Premium (KES)", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$premiumcount_by_quarter <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(POLICY_FROM_DATE)) %>%
        mutate(Quarter = paste0("Q", quarter(as.Date(POLICY_FROM_DATE)))) %>%
        mutate(Quarter = factor(Quarter, levels = c("Q1", "Q2", "Q3", "Q4"))) %>%
        group_by(Quarter) %>%
        summarise(PremiumCount = n()) %>%
        mutate(
          Label = case_when(
            PremiumCount >= 1e3 ~ paste0(formatC(PremiumCount / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(PremiumCount, format = "d", big.mark = ",")
          )
        )

      plot_ly(df, 
              x = ~Quarter, 
              y = ~PremiumCount, 
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
            text = "Quarterly Premium Count Trend",
            x = 0.01,
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 50),
          xaxis = list(title = "Quarter", tickfont = list(size = 10)),
          yaxis = list(title = "Premium Count", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })



    # Total Gross Premium by CLASS_DESCRIPTION
    output$premium_by_class <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(SUB_CLASSNAME)) %>%
        group_by(SUB_CLASSNAME) %>%
        summarise(TotalPremium = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        arrange(desc(TotalPremium)) %>%
        mutate(
          Label = case_when(
            TotalPremium >= 1e6 ~ paste0(formatC(TotalPremium / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalPremium >= 1e3 ~ paste0(formatC(TotalPremium / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalPremium, format = "f", digits = 0, big.mark = ",")
          )
        )

      plot_ly(df, 
              x = ~fct_reorder(SUB_CLASSNAME, -TotalPremium), 
              y = ~TotalPremium, 
              type = 'bar',
              text = ~Label,
              textfont = list(size = 9, color = "black"),
              textposition = 'outside',
              hoverinfo = 'text',
              hovertext = ~paste("Class:", SUB_CLASSNAME, "<br>Total Premium:", scales::comma(TotalPremium), "KES"),
              marker = list(color = '#00BFA5')) %>%
        layout(
          title = list(
            text = "Gross Premium by Class",
            uniformtext = list(minsize = 11, mode = 'show'), 
            x = 0.01,  # left-align title
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 100), 
          xaxis = list(title = "Class", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Total Premium(Millions KES)", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    # Policy Count by SUB_CLASSNAMEN
    output$count_by_class <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(SUB_CLASSNAME)) %>%
        group_by(SUB_CLASSNAME) %>%
        summarise(PolicyCount = n()) %>%
        arrange(desc(PolicyCount))%>%
        mutate(
          Label = case_when(
            PolicyCount >= 1e3 ~ paste0(formatC(PolicyCount / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(PolicyCount, format = "d", big.mark = ",")
          )
        )

      plot_ly(df, x = ~fct_reorder(SUB_CLASSNAME, -PolicyCount), y = ~PolicyCount, type = 'bar',
              text = ~Label,
              textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(size = 9, color = "black"),
              hovertext = ~paste("Class:", SUB_CLASSNAME, "<br>Policies:", formatC(PolicyCount, format = "d", big.mark = ",")),
              marker = list(color = '#EA80FC')) %>%
        layout(
          title = list(
            text = "Policy Count by Class",
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

    output$premium_by_customer_category <- renderPlot({
      data <- filtered_data() %>%
        filter(!is.na(CUSTOMER_CATEGORY)) %>%
        group_by(CUSTOMER_CATEGORY) %>%
        summarise(TotalPremium = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        mutate(
          TotalPremiumLabel = case_when(
            TotalPremium >= 1e6 ~ paste0(formatC(TotalPremium / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalPremium >= 1e3 ~ paste0(formatC(TotalPremium / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalPremium, format = "f", digits = 0, big.mark = ",")
          ),
          TotalPremiumMillions = TotalPremium / 1e6  # still used for plotting
        ) %>%
        arrange(desc(TotalPremiumMillions))
        if (nrow(data) == 0) return(NULL)

      max_val <- max(data$TotalPremiumMillions, na.rm = TRUE)      
      cat_order <- factor(data$CUSTOMER_CATEGORY, levels = rev(data$CUSTOMER_CATEGORY))

      ggplot(data, aes(x = cat_order, y = TotalPremiumMillions)) +
        geom_segment(aes(xend = cat_order, yend = 0), color = "#0d6efd", linewidth = 1) +
        geom_point(color = "#198754", size = 3) +
        geom_text(aes(label = TotalPremiumLabel,
                      y = TotalPremiumMillions + 0.02 * max_val),
                  hjust = -0.05, vjust = 0.5, color = "black", size = 3) +
        coord_flip(clip = "off") +
        theme_minimal() +
        theme(
          text = element_text(family = "Mulish"),
          legend.position = "none",
          plot.margin = margin(t = 10, r = 50, b = 10, l = 10, unit = "pt"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "lightgrey", linewidth = 0.5),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(),
          axis.title.y = element_text(),
          axis.text.y = element_text(color = "black"),
          plot.title.position = "plot",  # Ensures title aligns relative to plot area
          plot.title = element_text(hjust = 0, size = 14)
        ) +
        labs(
          x = "Customer Category",
          y = "Total Premium (Millions KES)",
          title = "Premium by Customer Category"
        )
    })

    output$count_by_customer_category <- renderPlot({
      data <- filtered_data() %>%
        filter(!is.na(CUSTOMER_CATEGORY)) %>%
        count(CUSTOMER_CATEGORY) %>%
        mutate(
          CountLabel = case_when(
            n >= 1e3 ~ paste0(formatC(n / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(n, format = "f", digits = 0, big.mark = ",")
          )
        )%>%
        arrange(desc(n))
      
      
      cat_order <- factor(data$CUSTOMER_CATEGORY, levels = rev(data$CUSTOMER_CATEGORY))

      if (nrow(data) == 0) return(NULL)
      max_n <- max(data$n, na.rm = TRUE)
      ggplot(data, aes(x = cat_order, y = n)) +
        geom_segment(aes(xend = cat_order, yend = 0), color = "#6c5ce7", linewidth = 1) +
        geom_point(color = "#00b894", size = 3) +

        geom_text(aes(label = CountLabel,
                      y = n + 0.02 * max_n),
                  hjust = -0.05, vjust = 0.5, color = "black", size = 3) +
        coord_flip(clip = "off") +
        theme_minimal() +
        theme(
          text = element_text(family = "Mulish"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "lightgrey", linewidth = 0.5),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(),
          axis.title.y = element_text(),
          axis.text.y = element_text(color = "black"),
          plot.title = element_text(hjust = 0, size = 14),
          plot.title.position = "plot",
          plot.margin = margin(t = 10, r = 50, b = 10, l = 10, unit = "pt"), 
        ) +
        labs(
          x = "Customer Category",
          y = "Policy Count",
          title = "Count by Customer Category"
        )
    })

    output$premium_by_segment <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(CORPORATE_OR_RETAIL)) %>%
        group_by(CORPORATE_OR_RETAIL) %>%
        summarise(TotalPremium = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        mutate(
          Label = paste0(CORPORATE_OR_RETAIL, ": ", 
                        formatC(TotalPremium / 1e6, format = "f", digits = 1, big.mark = ","), " M")
        )

      plot_ly(
        df, labels = ~CORPORATE_OR_RETAIL, values = ~TotalPremium, type = "pie", hole = 0.4,
        textposition = "outside", textinfo = "label+value+percent", insidetextorientation = "tangential",
        hoverinfo = "text",
        text = ~Label,
        marker = list(colors = c("#4C6EF5", "#5C677D"))  # Customize if you like
      ) %>%
        layout(
          title = list(text = "Premium by Segment (Corporate vs Retail)", x = 0.01, xanchor = "left", font = list(size = 14)),
          showlegend = TRUE,
          font = list(family = "Mulish"),
          margin = list(l = 80, r = 50, t = 50, b = 50) 
        )
    })

    output$count_by_segment <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(CORPORATE_OR_RETAIL)) %>%
        count(CORPORATE_OR_RETAIL) %>%
        mutate(
          Label = paste0(CORPORATE_OR_RETAIL, ": ", formatC(n, format = "d", big.mark = ","), " Policies")
        )

      plot_ly(
        df, labels = ~CORPORATE_OR_RETAIL, values = ~n, type = "pie", hole = 0.4,
        textposition = 'outside', textinfo = "label+value+percent", insidetextorientation = "tangential",
        hoverinfo = "text",
        text = ~Label,
        marker = list(colors = c("#87CEFA", "#6495ED"))  # Optional color scheme
      ) %>%
        layout(
          title = list(
            text = "Policy Count by Segment (Corporate vs Retail)", 
            x = 0.01, 
            xanchor = "left", 
            font = list(size = 14)),
          showlegend = TRUE,
          font = list(family = "Mulish"),
          margin = list(l = 80, r = 50, t = 50, b = 50) 
        )
    })

    output$premium_by_branch <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(BRANCH_NAME1)) %>%
        group_by(BRANCH_NAME1) %>%
        summarize(TotalPremium = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        mutate(
          Label = case_when(
            TotalPremium >= 1e6 ~ paste0(formatC(TotalPremium / 1e6, format = "f", digits = 0, big.mark = ","), " M"),
            TotalPremium >= 1e3 ~ paste0(formatC(TotalPremium / 1e3, format = "f", digits = 0, big.mark = ","), " K"),
            TRUE ~ formatC(TotalPremium, format = "f", digits = 0, big.mark = ",")
          ),
          Branch = fct_reorder(BRANCH_NAME1, TotalPremium)
        )%>%
        arrange(TotalPremium)

      plot_ly(
        df,
        x = ~TotalPremium,
        y = ~Branch,
        type = 'bar',
        orientation = 'h',
        marker = list(color = '#80FCEB'),
        text = ~Label,
        textposition = 'auto',
        textfont = list(size = 9, color = "#333333"),
        hoverinfo = 'text',
        hovertext = ~paste("Branch:", BRANCH_NAME1, "<br>Total Premium:", Label)
      ) %>%
        layout(
          title = list(text = "Premium by Branch", x = 0.01, xanchor = "left", font = list(size = 14)),
          yaxis = list(title = "", tickfont = list(size = 8, color = "#333333")),
          xaxis = list(title = "Total Premium", tickfont = list(size = 10, color = "#333333")),
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
          title = list(text = "Policy Count by Branch", x = 0.01, xanchor = "left", font = list(size = 14)),
          yaxis = list(title = "", tickfont = list(size = 8, color = "#333333")),
          xaxis = list(title = "Policy Count", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          margin = list(l = 100, r = 10, b = 10, t = 40),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })




    output$premium_by_business_type <- renderPlotly({
      # Load and filter the data
      data <- filtered_data()
      
      # Aggregate sales by cover type and order by descending sales
      premium_by_business_type <- data %>%
        group_by(`BUSINESS_TYPE`) %>%
        summarise(Premium = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(desc(Premium))
      
      # Calculate total sales and percentage share for each cover type
      total_premium <- sum(premium_by_business_type$Premium)
      premium_by_business_type <- premium_by_business_type %>%
        mutate(Percentage = Premium / total_premium * 100,
              # Create detailed hover labels showing the cover type, sales figures, and percentage
              Label = paste0(
                `BUSINESS_TYPE`, "<br>",
                "Premium: ", formatC(Premium, format = "f", big.mark = ","), "<br>",
                formatC(Percentage, format = "f", digits = 2), "%"
              ))
      
      # Set a constant pull value to detach (explode) each slice
      pull_values <- rep(0.1, nrow(premium_by_business_type))
      custom_colors <- c("#87CEFA", "#6495ED")
      # Create an interactive donut chart with on-slice labels (label and percent) and detailed hover text
      p <- plot_ly(
        premium_by_business_type,
        labels = ~`BUSINESS_TYPE`,
        values = ~Premium,
        type = 'pie',
        hole = 0.5,                      # Determines the donut hole size
        textinfo = 'label+value+percent',      # Show the category labels and percent on the slices
        insidetextorientation = 'radial',
        hoverinfo = 'text',              # Use custom hover text
        hovertext = ~Label,              # Detailed information on hover
        marker = list(colors = custom_colors[1:nrow(premium_by_business_type)]),
        pull = pull_values               # Detach each slice slightly
      ) %>%
        layout(
          title = list(text = "Premium by Business Type", x = 0.01, xanchor = "left", font = list(family = "Mulish", size = 14)),
          showlegend = TRUE,
          margin = list(l = 100, r = 10, b = 10, t = 40),
          # Optional: centered annotation inside the donut
          annotations = list(
            list(
              text = "",
              x = 0.5,
              y = 0.5,
              font = list(family = "Mulish", size = 16, color = "#555"),
              showarrow = FALSE
            )
          ),
          font = list(family = "Mulish")
        )
      
      p
    })



    output$count_by_business_type <- renderPlotly({
      count_by_business_type <- filtered_data() %>%
        # Filter out rows where Business Category might be NA
        filter(!is.na(BASE_PREMIUM), !is.na(BUSINESS_TYPE)) %>%
        # Group data by 'BUSINESS_TYPE' and count occurrences
        group_by(BUSINESS_TYPE) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        # Calculate percentages
        mutate(percentage = Count / sum(Count) * 100)
      # Generate color palette
      num_categories <- nrow(count_by_business_type)
      colors <- c("#48d1cc", "#00838f") 
      # Create the donut chart
      p <- plot_ly(count_by_business_type, labels = ~BUSINESS_TYPE, values = ~Count, type = 'pie', hole = 0.6,
                  textposition = 'outside',
                  textinfo = 'label+value+percent',
                  insidetextorientation = 'tangential',
                  marker = list(colors = colors),
                  textfont = list(color = 'black', family = "Mulish", size = 12))
      # Add title and display the plot
      p <- p %>% layout(showlegend = TRUE,
                        title = list(text = "Policy Count by Business Type", x = 0.01, xanchor = "left", font = list(size = 14)),
                        font = list(family = "Mulish"),
                        margin = list(l = 110, r = 10, b = 10, t = 40))
      p
    })

  })
}
