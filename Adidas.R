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
                  dateRangeInput("date_range", "Select Date Range:",
                                 start = min(adidas_data$invoice_date),
                                 end = max(adidas_data$invoice_date)),
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
  
  # Key Metrics Boxes
  output$total_sales_box <- renderValueBox({
    total_sales <- sum(filtered_data()$total_sales, na.rm = TRUE)
    valueBox(
      value = paste0("$", formatC(total_sales, format = "f", big.mark = ",", digits = 0)),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  output$total_units_sold_box <- renderValueBox({
    total_units_sold <- sum(filtered_data()$units_sold, na.rm = TRUE)
    valueBox(
      value = formatC(total_units_sold, format = "f", big.mark = ",", digits = 0),
      subtitle = "Total Units Sold",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$total_operating_profit_box <- renderValueBox({
    total_operating_profit <- sum(filtered_data()$operating_profit, na.rm = TRUE)
    valueBox(
      value = paste0("$", formatC(total_operating_profit, format = "f", big.mark = ",", digits = 0)),
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
  
  # Overview Chart
  output$overview_chart <- renderHighchart({
    data <- filtered_data() %>%
      group_by(month = floor_date(invoice_date, "month")) %>%
      summarise(
        total_sales = sum(total_sales, na.rm = TRUE),
        operating_margin = mean(operating_margin, na.rm = TRUE) * 100
      )
    
    if (input$chart_type == "Total Sales") {
      hchart(data, "line", hcaes(x = month, y = total_sales)) %>%
        hc_title(text = "Total Sales Over Time") %>%
        hc_yAxis(title = list(text = "Total Sales"), labels = list(format = "${value:,f}")) %>%
        hc_xAxis(title = list(text = "Month"))
    } else {
      hchart(data, "line", hcaes(x = month, y = operating_margin)) %>%
        hc_title(text = "Operating Margin Over Time") %>%
        hc_yAxis(title = list(text = "Operating Margin"), labels = list(format = "{value}%")) %>%
        hc_xAxis(title = list(text = "Month")) %>%
        hc_plotOptions(line = list(color = "#AD74B0"))
    }
  })
  
  # Region Sales Chart
  output$region_sales_chart <- renderHighchart({
    adidas_data %>%
      group_by(region) %>%
      summarise(total_sales = sum(total_sales, na.rm = TRUE)) %>%
      hchart("column", hcaes(x = region, y = total_sales)) %>%
      hc_title(text = "Sales by Region") %>%
      hc_yAxis(title = list(text = "Total Sales")) %>%
      hc_xAxis(title = list(text = "Region"))
  })
  
  # Regional Sales Map
  state_sales <- adidas_data %>%
    group_by(state) %>%
    summarise(total_sales = sum(total_sales, na.rm = TRUE))
  
  state_names <- data.frame(
    statecode = state.abb,
    state = state.name
  )
  
  state_sales <- left_join(state_sales, state_names, by = c("state" = "state"))
  
  output$region_sales_map <- renderPlotly({
    fig <- plot_geo(state_sales, locationmode = 'USA-states') %>%
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
    
    hchart(top_products_data, "column", hcaes(x = product, y = total_units_sold, color = I(colors))) %>%
      hc_title(text = "Top 5 Products by Units Sold") %>%
      hc_yAxis(title = list(text = "Total Units Sold")) %>%
      hc_xAxis(title = list(text = "Product")) %>%
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
