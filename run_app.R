# app.R - ADEM Dashboard 100% em R
library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)
library(lubridate)

# Source existing database connection
source("connect_db.R")

# Data functions
get_unemployment_overview <- function() {
  con <- connect_db()
  if (is.null(con)) return(create_sample_data())
  
  tryCatch({
    # Get latest unemployment data from de_flux
    query <- "
    SELECT 
      date,
      residence,
      SUM(ouvertures) as ouvertures,
      SUM(clotures) as clotures
    FROM de_flux 
    WHERE date >= CURRENT_DATE - INTERVAL '12 months'
    GROUP BY date, residence
    ORDER BY date DESC
    "
    
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data)
    
  }, error = function(e) {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    return(create_sample_data())
  })
}

get_youth_data <- function() {
  con <- connect_db()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    query <- "
    SELECT 
      date,
      niveau_de_diplome,
      SUM(_16_24_ans) as jeunes_16_24,
      SUM(_25_29_ans) as jeunes_25_29
    FROM de_jeunes 
    WHERE date >= CURRENT_DATE - INTERVAL '6 months'
    GROUP BY date, niveau_de_diplome
    ORDER BY date DESC
    "
    
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data)
    
  }, error = function(e) {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    return(data.frame())
  })
}

get_nationality_breakdown <- function() {
  con <- connect_db()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    query <- "
    SELECT 
      nationalite,
      SUM(personnes) as total_personnes
    FROM de_nationalite 
    WHERE date >= CURRENT_DATE - INTERVAL '3 months'
    GROUP BY nationalite
    ORDER BY total_personnes DESC
    LIMIT 8
    "
    
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data)
    
  }, error = function(e) {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    return(data.frame())
  })
}

get_age_distribution <- function() {
  con <- connect_db()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    query <- "
    SELECT 
      age,
      SUM(personnes) as total_personnes
    FROM de_dispo_age 
    WHERE date >= CURRENT_DATE - INTERVAL '3 months'
    GROUP BY age
    ORDER BY age
    "
    
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data)
    
  }, error = function(e) {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    return(data.frame())
  })
}

get_job_offers <- function() {
  con <- connect_db()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    query <- "
    SELECT 
      date,
      nature_contrat,
      SUM(postes_declares) as postes_declares,
      SUM(stock_postes_vacants) as stock_vacants
    FROM offres_series 
    WHERE date >= CURRENT_DATE - INTERVAL '12 months'
    GROUP BY date, nature_contrat
    ORDER BY date DESC
    "
    
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data)
    
  }, error = function(e) {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    return(data.frame())
  })
}

# Sample data function
create_sample_data <- function() {
  dates <- seq(from = as.Date("2024-01-01"), to = Sys.Date(), by = "month")
  data.frame(
    date = rep(dates, 2),
    residence = rep(c("Résident", "Non-résident CTR"), each = length(dates)),
    ouvertures = sample(1500:2500, length(dates) * 2),
    clotures = sample(1400:2400, length(dates) * 2)
  )
}

# Chart functions
create_unemployment_trend <- function(data) {
  if (nrow(data) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "Pas de données disponibles", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  # Aggregate by date
  trend_data <- data %>%
    group_by(date) %>%
    summarise(
      total_ouvertures = sum(ouvertures, na.rm = TRUE),
      total_clotures = sum(clotures, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(date)
  
  p <- plot_ly(trend_data, x = ~date) %>%
    add_lines(y = ~total_ouvertures, name = "Ouvertures", 
              line = list(color = "#e74c3c", width = 3)) %>%
    add_lines(y = ~total_clotures, name = "Clôtures", 
              line = list(color = "#27ae60", width = 3)) %>%
    layout(
      title = "",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Nombre de personnes"),
      hovermode = 'x unified',
      showlegend = TRUE,
      legend = list(x = 0, y = 1)
    )
  
  return(p)
}

create_nationality_chart <- function(data) {
  if (nrow(data) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "Pas de données disponibles", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  colors <- c('#2c3e50', '#3498db', '#e74c3c', '#f39c12', '#27ae60', 
              '#9b59b6', '#1abc9c', '#34495e')
  
  p <- plot_ly(data, labels = ~nationalite, values = ~total_personnes, 
               type = 'pie', marker = list(colors = colors)) %>%
    layout(
      title = "",
      showlegend = TRUE,
      legend = list(orientation = "v", x = 1.02, y = 0.5)
    )
  
  return(p)
}

create_age_chart <- function(data) {
  if (nrow(data) == 0) {
    return(plot_ly() %>% 
             add_annotations(text = "Pas de données disponibles", 
                             x = 0.5, y = 0.5, showarrow = FALSE))
  }
  
  p <- plot_ly(data, x = ~age, y = ~total_personnes, type = 'bar',
               marker = list(color = '#3498db')) %>%
    layout(
      title = "",
      xaxis = list(title = "Tranche d'âge"),
      yaxis = list(title = "Nombre de personnes"),
      showlegend = FALSE
    )
  
  return(p)
}

# UI
ui <- page_navbar(
  title = "ADEM - Chiffres Clés de l'Emploi",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c3e50",
    secondary = "#95a5a6"
  ),
  
  nav_panel(
    title = "Tableau de Bord",
    icon = icon("chart-line"),
    
    # Header
    layout_columns(
      col_widths = 12,
      card(
        card_header("Chiffres Clés de l'Emploi au Luxembourg"),
        card_body(
          p("Tableau de bord interactif du rapport mensuel de chômage luxembourgeois de l'ADEM. 
            Données de Data.Public. Mis à jour mensuellement selon la disponibilité des données.",
            class = "lead text-muted")
        )
      )
    ),
    
    # Key metrics
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      value_box(
        title = "Ouvertures Totales",
        value = textOutput("total_ouvertures"),
        showcase = icon("user-plus"),
        theme = "primary"
      ),
      
      value_box(
        title = "Clôtures Totales", 
        value = textOutput("total_clotures"),
        showcase = icon("user-minus"),
        theme = "success"
      ),
      
      value_box(
        title = "Jeunes (16-24)",
        value = textOutput("jeunes_total"),
        showcase = icon("user-graduate"),
        theme = "warning"
      ),
      
      value_box(
        title = "Offres d'Emploi",
        value = textOutput("offres_total"),
        showcase = icon("briefcase"),
        theme = "info"
      )
    ),
    
    # Charts row 1
    layout_columns(
      col_widths = c(8, 4),
      
      card(
        card_header("Évolution du Chômage"),
        card_body(
          withSpinner(plotlyOutput("unemployment_trend"))
        )
      ),
      
      card(
        card_header("Répartition par Nationalité"),
        card_body(
          withSpinner(plotlyOutput("nationality_chart"))
        )
      )
    ),
    
    # Charts row 2
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Répartition par Âge"),
        card_body(
          withSpinner(plotlyOutput("age_chart"))
        )
      ),
      
      card(
        card_header("Données Récentes"),
        card_body(
          withSpinner(DTOutput("recent_data"))
        )
      )
    )
  ),
  
  nav_panel(
    title = "À Propos",
    icon = icon("info-circle"),
    
    layout_columns(
      col_widths = c(8, 4),
      
      card(
        card_header("À Propos de ce Tableau de Bord"),
        card_body(
          h4("ADEM - Chiffres Clés de l'Emploi"),
          p("Ce tableau de bord fournit un aperçu de la situation de l'emploi au Luxembourg 
            basé sur les données de l'ADEM (Agence pour le développement de l'emploi)."),
          
          h5("Sources de Données"),
          tags$ul(
            tags$li("Statistiques de chômage par âge, nationalité et durée"),
            tags$li("Flux du marché du travail et tendances"),
            tags$li("Offres d'emploi disponibles et analyse sectorielle"),
            tags$li("Programmes d'emploi des jeunes et mesures")
          ),
          
          h5("Fréquence de Mise à Jour"),
          p("Les données sont mises à jour mensuellement selon la disponibilité 
            des nouvelles informations du portail de données ouvertes du gouvernement luxembourgeois.")
        )
      ),
      
      card(
        card_header("Liens & Contact"),
        card_body(
          h5("Tableau de Bord Original"),
          p(a("LuxData ADEM Dashboard", 
              href = "https://www.luxdata.lu/dashboardslive/ademchiffresclefs",
              target = "_blank")),
          
          h5("Source des Données"),
          p(a("Data.Public.lu", 
              href = "https://data.public.lu",
              target = "_blank"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data
  unemployment_data <- reactive({
    get_unemployment_overview()
  })
  
  youth_data <- reactive({
    get_youth_data()
  })
  
  nationality_data <- reactive({
    get_nationality_breakdown()
  })
  
  age_data <- reactive({
    get_age_distribution()
  })
  
  offers_data <- reactive({
    get_job_offers()
  })
  
  # Key metrics
  output$total_ouvertures <- renderText({
    data <- unemployment_data()
    if (nrow(data) > 0) {
      latest_month <- data %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        summarise(total = sum(ouvertures, na.rm = TRUE)) %>%
        pull(total)
      format(latest_month, big.mark = " ")
    } else {
      "N/A"
    }
  })
  
  output$total_clotures <- renderText({
    data <- unemployment_data()
    if (nrow(data) > 0) {
      latest_month <- data %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        summarise(total = sum(clotures, na.rm = TRUE)) %>%
        pull(total)
      format(latest_month, big.mark = " ")
    } else {
      "N/A"
    }
  })
  
  output$jeunes_total <- renderText({
    data <- youth_data()
    if (nrow(data) > 0) {
      latest_month <- data %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        summarise(total = sum(jeunes_16_24, na.rm = TRUE)) %>%
        pull(total)
      format(latest_month, big.mark = " ")
    } else {
      "N/A"
    }
  })
  
  output$offres_total <- renderText({
    data <- offers_data()
    if (nrow(data) > 0) {
      latest_month <- data %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        summarise(total = sum(stock_vacants, na.rm = TRUE)) %>%
        pull(total)
      format(latest_month, big.mark = " ")
    } else {
      "N/A"
    }
  })
  
  # Charts
  output$unemployment_trend <- renderPlotly({
    create_unemployment_trend(unemployment_data())
  })
  
  output$nationality_chart <- renderPlotly({
    create_nationality_chart(nationality_data())
  })
  
  output$age_chart <- renderPlotly({
    create_age_chart(age_data())
  })
  
  # Data table
  output$recent_data <- renderDT({
    data <- unemployment_data()
    if (nrow(data) > 0) {
      data %>%
        arrange(desc(date)) %>%
        head(10) %>%
        datatable(
          options = list(
            pageLength = 5,
            scrollX = TRUE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
          ),
          rownames = FALSE
        )
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)