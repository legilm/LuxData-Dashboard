# Use the official R Shiny image as base
FROM rocker/shiny:4.3.2

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages required by your app
RUN R -e "install.packages(c('shiny', 'DBI', 'RPostgreSQL', 'bslib', 'ggplot2', 'DT', 'shinycssloaders', 'plotly', 'shinyWidgets'))"

# Copy your app files into the image
COPY app.R /srv/shiny-server/
COPY connect_db.R /srv/shiny-server/

# (Opcional) Copie outros arquivos necessários
# COPY data/ /srv/shiny-server/data/

# Expose the default Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]