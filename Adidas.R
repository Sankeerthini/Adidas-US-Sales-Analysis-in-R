# Load necessary libraries
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(readxl)
library(plotly)
library(shinycssloaders)
library(DT)
library(lubridate)

# Define the path to the Excel file
file_path <- "/Users/sankeerthini/Desktop/Datasets/R/Adidas.xlsx"

# Read the data into a data frame
adidas_data <- read_excel(file_path)

# Adjust column names
adidas_data <- adidas_data %>%
  rename(
    retailer = "Retailer",
    retailer_id = "Retailer ID",
    invoice_date = "Invoice Date",
    region = "Region",
    state = "State",
    city = "City",
    product = "Product",
    price_per_unit = "Price per Unit",
    units_sold = "Units Sold",
    total_sales = "Total Sales",
    operating_profit = "Operating Profit",
    operating_margin = "Operating Margin",
    sales_method = "Sales Method"
  ) %>%
  mutate(invoice_date = as.Date(invoice_date))

# Define key metrics
total_sales <- sum(adidas_data$total_sales, na.rm = TRUE)
total_units_sold <- sum(adidas_data$units_sold, na.rm = TRUE)
total_operating_profit <- sum(adidas_data$operating_profit, na.rm = TRUE)
avg_operating_margin <- mean(adidas_data$operating_margin, na.rm = TRUE)

# Formatting large numbers function
format_large_numbers <- function(value) {
  if (value >= 1e9) {
    paste0(formatC(value / 1e9, format = "f", digits = 2), " B")
  } else if (value >= 1e6) {
    paste0(formatC(value / 1e6, format = "f", digits = 2), " M")
  } else if (value >= 1e3) {
    paste0(formatC(value / 1e3, format = "f", digits = 2), " K")
  } else {
    formatC(value, format = "f", digits = 0)
  }
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Adidas Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Sales by Region", tabName = "region_sales", icon = icon("globe")),
      menuItem("Sales by Sales Method", tabName = "sales_method_sales", icon = icon("chart-pie")),
      menuItem("Top Products", tabName = "top_products", icon = icon("star")),
      menuItem("Retailers", tabName = "retailers", icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales_box", width = 3),
                valueBoxOutput("total_units_sold_box", width = 3),
                valueBoxOutput("total_operating_profit_box", width = 3),
                valueBoxOutput("avg_operating_margin_box", width = 3)
              ),
              fluidRow(
                box(
                  sliderInput("date_range", "Select Date Range:",
                              min = as.Date(min(adidas_data$invoice_date)),
                              max = as.Date(max(adidas_data$invoice_date)),
                              value = c(as.Date(min(adidas_data$invoice_date)),
                                        as.Date(max(adidas_data$invoice_date))),
                              timeFormat = "%b %y",
                              dragRange = TRUE,
                              ticks = FALSE),
                  selectInput("chart_type", "Select Chart Type:", choices = c("Total Sales", "Operating Margin")),
                  highchartOutput("overview_chart"), width = 12
                )
              )
      ),
      tabItem(tabName = "region_sales",
              fluidRow(
                box(highchartOutput("region_sales_chart"), width = 12),
                box(plotlyOutput("region_sales_map"), width = 12)
              )
      ),
      tabItem(tabName = "sales_method_sales",
              fluidRow(
                box(highchartOutput("sales_method_chart"), width = 12)
              )
      ),
      tabItem(tabName = "top_products",
              fluidRow(
                box(highchartOutput("top_products_chart"), width = 12)
              )
      ),
      tabItem(tabName = "retailers",
              fluidRow(
                box(DTOutput("retailers_table"), width = 12)
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    adidas_data %>%
      filter(invoice_date >= input$date_range[1] & invoice_date <= input$date_range[2])
  })
  
  # Key Metrics Boxes with formatted numbers
  output$total_sales_box <- renderValueBox({
    total_sales <- sum(filtered_data()$total_sales, na.rm = TRUE)
    valueBox(
      value = paste0("$", format_large_numbers(total_sales)),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  output$total_units_sold_box <- renderValueBox({
    total_units_sold <- sum(filtered_data()$units_sold, na.rm = TRUE)
    valueBox(
      value = format_large_numbers(total_units_sold),
      subtitle = "Total Units Sold",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$total_operating_profit_box <- renderValueBox({
    total_operating_profit <- sum(filtered_data()$operating_profit, na.rm = TRUE)
    valueBox(
      value = paste0("$", format_large_numbers(total_operating_profit)),
      subtitle = "Total Operating Profit",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$avg_operating_margin_box <- renderValueBox({
    avg_operating_margin <- mean(filtered_data()$operating_margin, na.rm = TRUE)
    valueBox(
      value = paste0(formatC(avg_operating_margin * 100, format = "f", digits = 2), "%"),
      subtitle = "Average Operating Margin",
      icon = icon("percentage"),
      color = "yellow"
    )
  })
  
  # Overview Chart with Month Year format
  output$overview_chart <- renderHighchart({
    if (input$chart_type == "Total Sales") {
      overview_data <- filtered_data() %>%
        group_by(month_year = floor_date(invoice_date, "month")) %>%
        summarise(total_sales = sum(total_sales, na.rm = TRUE))
      
      highchart() %>%
        hc_chart(type = "area") %>%
        hc_title(text = "Total Sales Over Time") %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = '%b %Y')) %>%
        hc_yAxis(title = list(text = "Total Sales ($)")) %>%
        hc_add_series(name = "Total Sales ($)", data = lapply(1:nrow(overview_data), function(i) {
          list(x = datetime_to_timestamp(as.Date(overview_data$month_year[i])), y = overview_data$total_sales[i])
        }),
        color = "#AD74B0",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 0, x2 = 0, y2 = 1),
          stops = list(
            list(0, hex_to_rgba("#AD74B0", 1)),
            list(1, hex_to_rgba("#AD74B0", 0))
          )
        )) %>%
        hc_tooltip(pointFormat = '{point.x:%B %Y}: <b>${point.y:.2f}</b>')
      
    } else if (input$chart_type == "Operating Margin") {
      overview_data <- filtered_data() %>%
        group_by(month_year = floor_date(invoice_date, "month")) %>%
        summarise(operating_margin = mean(operating_margin, na.rm = TRUE) * 100)
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = "Operating Margin Over Time") %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = '%b %Y')) %>%
        hc_yAxis(title = list(text = "Operating Margin (%)")) %>%
        hc_add_series(name = "Operating Margin (%)", data = lapply(1:nrow(overview_data), function(i) {
          list(x = datetime_to_timestamp(as.Date(overview_data$month_year[i])), y = overview_data$operating_margin[i])
        }),
        color = "#04BADE",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 0, x2 = 0, y2 = 1),
          stops = list(
            list(0, hex_to_rgba("#04BADE", 1)),
            list(1, hex_to_rgba("#04BADE", 0))
          )
        )) %>%
        hc_tooltip(pointFormat = '{point.x:%B %Y}: <b>{point.y:.2f}%</b>')
    }
  })
  
  # Helper function to convert hex color to rgba
  hex_to_rgba <- function(hex, alpha) {
    rgb <- col2rgb(hex) / 255
    sprintf("rgba(%d, %d, %d, %.2f)", rgb[1] * 255, rgb[2] * 255, rgb[3] * 255, alpha)
  }
  
  
  # Region Sales Chart
  output$region_sales_chart <- renderHighchart({
    region_sales_data <- filtered_data() %>%
      group_by(region) %>%
      summarise(total_sales = sum(total_sales, na.rm = TRUE))
    
    # Define a vector of pastel colors
    pastel_colors <- c("#E7B5D3", "#DFC3E3", "#D7C8E9", "#BEC3EA", "#7CA1D9")
    
    # Prepare data with colors
    data_with_colors <- region_sales_data %>%
      mutate(color = pastel_colors[1:n()])
    
    hchart(
      data_with_colors, 
      "bar", 
      hcaes(x = region, y = total_sales, color = color)
    ) %>%
      hc_title(text = "Total Sales by Region") %>%
      hc_xAxis(title = list(text = "Region")) %>%
      hc_yAxis(title = list(text = "Total Sales")) %>%
      hc_tooltip(pointFormat = '{point.region}: <b>${point.y:.2f}</b>')
  })
  
  
  
  # Region Sales Map
  output$region_sales_map <- renderPlotly({
    region_sales_data <- filtered_data() %>%
      group_by(state) %>%
      summarise(total_sales = sum(total_sales, na.rm = TRUE))
    
    state_info <- data.frame(
      state = state.name,
      statecode = state.abb
    )
    
    region_sales_data <- merge(region_sales_data, state_info, by.x = "state", by.y = "state", all.x = TRUE)
    
    fig <- plot_geo(region_sales_data, locationmode = 'USA-states') %>%
      add_trace(
        z = ~total_sales, 
        locations = ~statecode, 
        color = ~total_sales, 
        colors = 'Blues',
        text = ~paste(state, "<br>Sales: $", formatC(total_sales, format = "f", big.mark = ",", digits = 2)),
        hoverinfo = "text"
      ) %>%
      colorbar(title = "Total Sales") %>%
      layout(
        title = "Total Sales by US State",
        geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = TRUE,
          lakecolor = toRGB('white')
        )
      )
    fig
  })
  
  # Sales by Sales Method Chart
  output$sales_method_chart <- renderHighchart({
    sales_method_data <- filtered_data() %>%
      group_by(sales_method) %>%
      summarise(total_sales = sum(total_sales, na.rm = TRUE))
    
    total_sales <- sum(sales_method_data$total_sales)
    sales_method_data <- sales_method_data %>%
      mutate(percentage = total_sales / total_sales * 100)
    
    hchart(sales_method_data, "pie", hcaes(name = sales_method, y = total_sales, label = percentage)) %>%
      hc_colors(c("#f48ba9", "#04BADE", "#AD74B0")) %>% # Purple, Blue, Pink
      hc_plotOptions(pie = list(
        dataLabels = list(
          enabled = TRUE,
          format = '{point.name}: {point.percentage:.2f} %'
        )
      )) %>%
      hc_title(text = "Total Sales by Sales Method") %>%
      hc_tooltip(pointFormat = '{point.name}: <b>${point.y:.2f}</b> ({point.percentage:.2f}%)')
  })
  
  # Top Products Chart
  output$top_products_chart <- renderHighchart({
    top_products_data <- adidas_data %>%
      group_by(product) %>%
      summarise(total_units_sold = sum(units_sold, na.rm = TRUE)) %>%
      top_n(5, total_units_sold) %>%
      arrange(desc(total_units_sold))
    
    colors <- c("#AD74B0", "#C598C3", "#D4B3CF", "#E1C9D9", "#F2DBE1")
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Top 5 Products by Units Sold") %>%
      hc_xAxis(categories = top_products_data$product) %>%
      hc_yAxis(title = list(text = "Total Units Sold")) %>%
      hc_add_series(name = "Units Sold", data = top_products_data$total_units_sold, showInLegend = FALSE, colorByPoint = TRUE) %>%
      hc_plotOptions(column = list(
        colorByPoint = TRUE,
        colors = colors
      ))
  })
  
  # Retailers Table
  output$retailers_table <- renderDT({
    adidas_data %>%
      group_by(retailer, retailer_id, state, city) %>%
      summarise(
        total_units_sold = sum(units_sold, na.rm = TRUE),
        total_sales = sum(total_sales, na.rm = TRUE)
      ) %>%
      datatable()
  })
}

# Run the application
shinyApp(ui = ui, server = server)