# app.R - ADEM Key Employment Figures Dashboard (English, scrollable, modular, beginner-friendly)

library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)
library(lubridate)

# Source your existing DB connection function
source("connect_db.R")

# --- Data functions (modular, one per chart/indicator) ---

# Example: Get total unemployment by month
get_unemployment_trend <- function() {
  con <- connect_db()
  on.exit(if (!is.null(con)) DBI::dbDisconnect(con))
  dbGetQuery(con, "
    SELECT date, SUM(ouvertures) AS entries, SUM(clotures) AS exits
    FROM de_flux
    GROUP BY date
    ORDER BY date
  ")
}

# Example: Get youth unemployment (16-24)
get_youth_unemployment <- function() {
  con <- connect_db()
  on.exit(if (!is.null(con)) DBI::dbDisconnect(con))
  dbGetQuery(con, "
    SELECT date, SUM(_16_24_ans) AS youth_unemployed
    FROM de_jeunes
    GROUP BY date
    ORDER BY date
  ")
}

# Example: Get job offers
get_job_offers <- function() {
  con <- connect_db()
  on.exit(if (!is.null(con)) DBI::dbDisconnect(con))
  dbGetQuery(con, "
    SELECT date, SUM(stock_postes_vacants) AS open_positions
    FROM offres-series
    GROUP BY date
    ORDER BY date
  ")
}

# Example: Get unemployment by nationality
get_unemployment_by_nationality <- function() {
  con <- connect_db()
  on.exit(if (!is.null(con)) DBI::dbDisconnect(con))
  dbGetQuery(con, "
    SELECT nationalite AS nationality, SUM(personnes) AS unemployed
    FROM de-nationalite
    WHERE date = (SELECT MAX(date) FROM de-nationalite)
    GROUP BY nationalite
    ORDER BY unemployed DESC
    LIMIT 10
  ")
}

# Example: Get unemployment by age group
get_unemployment_by_age <- function() {
  con <- connect_db()
  on.exit(if (!is.null(con)) DBI::dbDisconnect(con))
  dbGetQuery(con, "
    SELECT age, SUM(personnes) AS unemployed
    FROM de-dispo-age
    WHERE date = (SELECT MAX(date) FROM de-dispo-age)
    GROUP BY age
    ORDER BY age
  ")
}

# --- UI ---

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("ADEM Key Employment Figures Dashboard"),
  tags$p("A copy of the official ADEM dashboard. Data from Data.Public.lu. Updated monthly.", class = "text-muted"),
  br(),
  
  # Key indicators
  fluidRow(
    column(3, wellPanel(h4("Total Entries"), textOutput("total_entries"))),
    column(3, wellPanel(h4("Total Exits"), textOutput("total_exits"))),
    column(3, wellPanel(h4("Youth Unemployed (16-24)"), textOutput("youth_unemployed"))),
    column(3, wellPanel(h4("Open Job Offers"), textOutput("open_positions")))
  ),
  br(),
  
  # Main charts
  fluidRow(
    column(6, h4("Unemployment Trend"), withSpinner(plotlyOutput("unemployment_trend"))),
    column(6, h4("Youth Unemployment Trend"), withSpinner(plotlyOutput("youth_trend")))
  ),
  br(),
  fluidRow(
    column(6, h4("Unemployment by Nationality"), withSpinner(plotlyOutput("nationality_chart"))),
    column(6, h4("Unemployment by Age Group"), withSpinner(plotlyOutput("age_chart")))
  ),
  br(),
  
  # Data table
  h4("Recent Unemployment Data"),
  withSpinner(DTOutput("recent_table")),
  br(),
  
  # About
  h4("About"),
  tags$p("This dashboard is a copy of the official ADEM dashboard:"),
  tags$a(href = "https://www.luxdata.lu/dashboardslive/ademchiffresclefs", "Original dashboard", target = "_blank"),
  tags$p("Data source: Data.Public.lu")
)

# --- SERVER ---

server <- function(input, output, session) {
  # Unemployment trend
  unemployment_data <- reactive({ get_unemployment_trend() })
  youth_data <- reactive({ get_youth_unemployment() })
  offers_data <- reactive({ get_job_offers() })
  nationality_data <- reactive({ get_unemployment_by_nationality() })
  age_data <- reactive({ get_unemployment_by_age() })
  
  # Key indicators
  output$total_entries <- renderText({
    data <- unemployment_data()
    if (nrow(data) > 0) format(max(data$entries, na.rm = TRUE), big.mark = ",") else "N/A"
  })
  output$total_exits <- renderText({
    data <- unemployment_data()
    if (nrow(data) > 0) format(max(data$exits, na.rm = TRUE), big.mark = ",") else "N/A"
  })
  output$youth_unemployed <- renderText({
    data <- youth_data()
    if (nrow(data) > 0) format(max(data$youth_unemployed, na.rm = TRUE), big.mark = ",") else "N/A"
  })
  output$open_positions <- renderText({
    data <- offers_data()
    if (nrow(data) > 0) format(max(data$open_positions, na.rm = TRUE), big.mark = ",") else "N/A"
  })
  
  # Charts
  output$unemployment_trend <- renderPlotly({
    data <- unemployment_data()
    plot_ly(data, x = ~date) %>%
      add_lines(y = ~entries, name = "Entries", line = list(color = "#0072B2")) %>%
      add_lines(y = ~exits, name = "Exits", line = list(color = "#D55E00")) %>%
      layout(yaxis = list(title = "People"), xaxis = list(title = "Date"))
  })
  output$youth_trend <- renderPlotly({
    data <- youth_data()
    plot_ly(data, x = ~date, y = ~youth_unemployed, type = "scatter", mode = "lines+markers",
            line = list(color = "#009E73")) %>%
      layout(yaxis = list(title = "Youth Unemployed"), xaxis = list(title = "Date"))
  })
  output$nationality_chart <- renderPlotly({
    data <- nationality_data()
    plot_ly(data, labels = ~nationality, values = ~unemployed, type = "pie") %>%
      layout(showlegend = TRUE)
  })
  output$age_chart <- renderPlotly({
    data <- age_data()
    plot_ly(data, x = ~age, y = ~unemployed, type = "bar", marker = list(color = "#56B4E9")) %>%
      layout(yaxis = list(title = "Unemployed"), xaxis = list(title = "Age Group"))
  })
  
  # Data table
  output$recent_table <- renderDT({
    data <- unemployment_data()
    datatable(data[order(data$date, decreasing = TRUE), ], options = list(pageLength = 10))
  })
}

shinyApp(ui, server)