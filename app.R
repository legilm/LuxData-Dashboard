# Load required libraries
library(shiny)
library(DBI)
library(RPostgreSQL)
library(bslib)
library(ggplot2)
library(DT)
library(shinycssloaders)  # For loading spinners
library(plotly)           # For interactive plots
library(shinyWidgets)

# Source the database connection function
source("connect_db.R")

# Set your schema name here
schema <- "student_gilmar"

# Helper for valueBox
valueBox <- function(value, subtitle, color = "primary", text_color = "white") {
  color_map <- list(
    "primary" = "#007bff",
    "success" = "#28a745", 
    "warning" = "#ffc107",
    "info" = "#17a2b8",
    "danger" = "#dc3545"
  )
  bg_color <- if (!is.null(color_map[[color]])) color_map[[color]] else "#007bff"
  div(
    style = paste0("background-color: ", bg_color, "; color: white; padding: 15px; border-radius: 12px; margin: 5px; text-align: center; font-size: 1.2em;"),
    h2(style = "margin: 0; font-size: 1.8em;", value),
    p(style = "margin: 5px 0 0 0; font-size: 0.9em;", subtitle)
  )
}

# Define UI using bslib for modern look
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  tags$head(
    tags$style(HTML("
      .container-fluid { max-width: 1600px; }
    "))
  ),
  br(),
  titlePanel("ADEM's Key Employment Figures Dashboard"),
  
  # Description below the title
  div(
    style = "margin-bottom: 25px; font-size: 1.15em; color: #333;",
    "Interactive dashboard of the Luxembourg monthly unemployment report of ADEM. Data from Data.Public. Updated monthly as data becomes available."
  ),
  
  # Date filter and reload button
  fluidRow(
    column(
      12,
      airMonthpickerInput(
        inputId = "month_range",
        label = tags$div("📅 Month Range"),
        range = TRUE,
        value = c(
          format(as.Date(Sys.Date()) - 365, "%Y-%m"),
          format(as.Date(Sys.Date()), "%Y-%m")
        ),
        minDate = "2009-01",
        maxDate = format(as.Date(Sys.Date()), "%Y-%m"),
        width = "20%",
        autoClose = TRUE
      ),
      br(),
      actionButton(
        "reload_dates", 
        "Update dates on Dashboard", 
        icon = icon("sync-alt"),
        style = "background: #007bff; 
               color: white; 
               border: none; 
               border-radius: 8px; 
               padding: 10px 20px; 
               font-weight: 500;
               box-shadow: 0 2px 4px rgba(0,123,255,0.3);
               margin-top: 10px;"
      )
    )
  ),
  br(),
  
  # Key indicators with spinners
  fluidRow(
    column(3, withSpinner(uiOutput("entriesBox"), size = 0.5)),
    column(3, withSpinner(uiOutput("exitsBox"), size = 0.5)),
    column(3, withSpinner(uiOutput("youthBox"), size = 0.5)),
    column(3, withSpinner(uiOutput("offersBox"), size = 0.5))
  ),
  br(),
  # Interactive plots with spinners
  tabsetPanel(
    tabPanel("Unemployment Trend", withSpinner(plotlyOutput("unempTrendPlot", height = "500px", width = "100%"), size = 1)),
    tabPanel("Youth Unemployment", withSpinner(plotlyOutput("youthPlot", height = "500px", width = "100%"), size = 1)),
    tabPanel("Job Offers", withSpinner(plotlyOutput("offersPlot", height = "500px", width = "100%"), size = 1)),
    tabPanel("By Age Group", withSpinner(plotlyOutput("ageBar", height = "500px", width = "100%"), size = 1)),
    tabPanel("Raw Data", withSpinner(DT::dataTableOutput("rawTable"), size = 1))
  ),
  br(),
  # Footer
  tags$footer(
    style = "margin-top: 40px; padding: 18px 0; background: #f8f9fa; color: #333; text-align: center; font-size: 1em; border-top: 1px solid #e0e0e0;",
    HTML('Made by Gil during the Module 6: Business Applications of R, on the Full Stack in R Dev course at Digital Learning Hub at Belval - Luxembourg. <br>
          <a href="https://github.com/legilm" target="_blank" style="color: #007bff; text-decoration: underline;">https://github.com/legilm</a>')
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the active date range
  active_dates <- reactiveVal(NULL)
  
  get_connection <- function() {
    connect_db()
  }
  
  safe_query <- function(query) {
    con <- NULL
    result <- NULL
    tryCatch({
      con <- get_connection()
      result <- dbGetQuery(con, query)
      dbDisconnect(con)
      result
    }, error = function(e) {
      if (!is.null(con)) dbDisconnect(con)
      NULL
    })
  }
  
  # Initialize active dates when app starts
  observeEvent(TRUE, {
    # Set initial date range from database
    query <- sprintf("SELECT MIN(date) as min_date, MAX(date) as max_date FROM %s.de_flux", schema)
    df <- safe_query(query)
    if (!is.null(df)) {
      updateDateRangeInput(session, "daterange",
                           start = Sys.Date() - 365,
                           end = Sys.Date(),
                           min = df$min_date,
                           max = df$max_date
      )
      active_dates(c(Sys.Date() - 365, Sys.Date()))
    } else {
      active_dates(input$daterange)
    }
  }, once = TRUE)
  
  observeEvent(input$reload_dates, {
    # input$month_range retorna c("2023-05", "2024-05")
    # Converta para o formato Date se necessário
    dr <- as.Date(paste0(input$month_range, "-01"))
    active_dates(dr)
  })
  
  # Total Entries
  output$entriesBox <- renderUI({
    dr <- active_dates()
    if (is.null(dr)) return(NULL)
    
    query <- sprintf(
      "SELECT SUM(ouvertures) AS entries FROM %s.de_flux WHERE date BETWEEN '%s' AND '%s'",
      schema, dr[1], dr[2]
    )
    result <- safe_query(query)
    entries <- if (!is.null(result) && !is.na(result$entries)) format(result$entries, big.mark = ",") else "0"
    valueBox(entries, "Total Entries", color = "primary")
  })
  
  # Total Exits
  output$exitsBox <- renderUI({
    dr <- active_dates()
    if (is.null(dr)) return(NULL)
    
    query <- sprintf(
      "SELECT SUM(clotures) AS exits FROM %s.de_flux WHERE date BETWEEN '%s' AND '%s'",
      schema, dr[1], dr[2]
    )
    result <- safe_query(query)
    exits <- if (!is.null(result) && !is.na(result$exits)) format(result$exits, big.mark = ",") else "0"
    valueBox(exits, "Total Exits", color = "success")
  })
  
  # Youth Unemployed (16-24) - valor do último mês do filtro
  output$youthBox <- renderUI({
    dr <- active_dates()
    if (is.null(dr)) return(NULL)
    
    query_date <- sprintf(
      "SELECT MAX(date) as max_date FROM %s.de_jeunes WHERE date BETWEEN '%s' AND '%s'",
      schema, dr[1], dr[2]
    )
    max_date <- safe_query(query_date)$max_date
    youth <- 0
    if (!is.null(max_date) && !is.na(max_date)) {
      query <- sprintf(
        'SELECT SUM("_16_24_ans") AS youth FROM %s.de_jeunes WHERE date = \'%s\'',
        schema, max_date
      )
      result <- safe_query(query)
      if (!is.null(result) && !is.na(result$youth)) {
        youth <- format(result$youth, big.mark = ",")
      }
    }
    valueBox(youth, sprintf("Youth Unemployed (16-24)"), color = "warning")
  })
  
  # Open Job Offers - valor do último mês do filtro
  output$offersBox <- renderUI({
    dr <- active_dates()
    if (is.null(dr)) return(NULL)
    
    query_date <- sprintf(
      "SELECT MAX(date) as max_date FROM %s.offres_series WHERE date BETWEEN '%s' AND '%s'",
      schema, dr[1], dr[2]
    )
    max_date <- safe_query(query_date)$max_date
    offers <- 0
    if (!is.null(max_date) && !is.na(max_date)) {
      query <- sprintf(
        "SELECT SUM(stock_postes_vacants) AS offers FROM %s.offres_series WHERE date = '%s'",
        schema, max_date
      )
      result <- safe_query(query)
      if (!is.null(result) && !is.na(result$offers)) {
        offers <- format(result$offers, big.mark = ",")
      }
    }
    valueBox(offers, sprintf("Open Job Offers"), color = "info")
  })
  
  # Unemployment Trend (plotly)
  output$unempTrendPlot <- renderPlotly({
    dr <- active_dates()
    if (is.null(dr)) return(plotly_empty())
    
    query <- sprintf(
      "SELECT date, SUM(ouvertures) AS entries, SUM(clotures) AS exits FROM %s.de_flux WHERE date BETWEEN '%s' AND '%s' GROUP BY date ORDER BY date",
      schema, dr[1], dr[2]
    )
    df <- safe_query(query)
    if (!is.null(df) && nrow(df) > 0) {
      df$date <- as.Date(df$date)
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes(y = entries, color = "Entries"), size = 1.2) +
        geom_line(aes(y = exits, color = "Exits"), size = 1.2) +
        labs(title = "Unemployment Entries and Exits Over Time", y = "Count", x = "Date") +
        scale_color_manual(values = c("Entries" = "red", "Exits" = "blue")) +
        theme_minimal(base_size = 14)
      ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        layout(hovermode = "x unified")
    } else {
      plotly_empty()
    }
  })
  
  # Youth Unemployment (plotly)
  output$youthPlot <- renderPlotly({
    dr <- active_dates()
    if (is.null(dr)) return(plotly_empty())
    
    query <- sprintf(
      'SELECT date, SUM("_16_24_ans") AS youth FROM %s.de_jeunes WHERE date BETWEEN \'%s\' AND \'%s\' GROUP BY date ORDER BY date',
      schema, dr[1], dr[2]
    )
    df <- safe_query(query)
    if (!is.null(df) && nrow(df) > 0) {
      df$date <- as.Date(df$date)
      p <- ggplot(df, aes(x = date, y = youth)) +
        geom_line(color = "orange", size = 1.2) +
        labs(title = "Youth Unemployment (16-24) Over Time", y = "Youth Unemployed", x = "Date") +
        theme_minimal(base_size = 14)
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
    } else {
      plotly_empty()
    }
  })
  
  # Job Offers (plotly)
  output$offersPlot <- renderPlotly({
    dr <- active_dates()
    if (is.null(dr)) return(plotly_empty())
    
    query <- sprintf(
      "SELECT date, SUM(stock_postes_vacants) AS offers FROM %s.offres_series WHERE date BETWEEN '%s' AND '%s' GROUP BY date ORDER BY date",
      schema, dr[1], dr[2]
    )
    df <- safe_query(query)
    if (!is.null(df) && nrow(df) > 0) {
      df$date <- as.Date(df$date)
      p <- ggplot(df, aes(x = date, y = offers)) +
        geom_line(color = "green", size = 1.2) +
        labs(title = "Open Job Offers Over Time", y = "Open Positions", x = "Date") +
        theme_minimal(base_size = 14)
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
    } else {
      plotly_empty()
    }
  })
  
  # Age Bar (plotly)
  output$ageBar <- renderPlotly({
    dr <- active_dates()
    if (is.null(dr)) return(plotly_empty())
    
    query_date <- sprintf(
      "SELECT MAX(date) as max_date FROM %s.de_dispo_age WHERE date BETWEEN '%s' AND '%s'",
      schema, dr[1], dr[2]
    )
    max_date <- safe_query(query_date)$max_date
    if (!is.null(max_date) && !is.na(max_date)) {
      query <- sprintf(
        "SELECT age, SUM(personnes) AS unemployed FROM %s.de_dispo_age WHERE date = '%s' GROUP BY age ORDER BY age",
        schema, max_date
      )
      df <- safe_query(query)
      if (!is.null(df) && nrow(df) > 0) {
        p <- ggplot(df, aes(x = age, y = unemployed, fill = age, text = paste("Age Group:", age, "<br>Unemployed:", unemployed))) +
          geom_col() +
          labs(title = sprintf("Unemployment by Age Group (%s)", max_date), y = "Unemployed", x = "Age Group") +
          theme_minimal(base_size = 14) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
        return(ggplotly(p, tooltip = "text"))
      }
    }
    plotly_empty()
  })
  
  # Raw Data Table
  output$rawTable <- DT::renderDataTable({
    dr <- active_dates()
    if (is.null(dr)) return(data.frame())
    
    query <- sprintf(
      "SELECT * FROM %s.de_flux WHERE date BETWEEN '%s' AND '%s' ORDER BY date DESC LIMIT 100",
      schema, dr[1], dr[2]
    )
    df <- safe_query(query)
    if (!is.null(df)) df else data.frame()
  }, options = list(scrollX = TRUE, pageLength = 10))
}



# Run the application
shinyApp(ui = ui, server = server)