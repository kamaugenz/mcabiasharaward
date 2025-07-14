FROM rocker/shiny:4

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libglpk-dev

# Install R packages required
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinyMobile",\
              "uuid",\
              "DBI",\
              "RPostgres",\
              "highcharter"))'

WORKDIR /home/shinyusr

# Copy the Shiny app code into the Docker container
COPY app.R app.R

# Expose the port for Shiny (default 3838)
EXPOSE 3838

# Set the command to run your Shiny app
CMD ["R", "-e", "shiny::runApp('/home/shinyusr', port = 3838, host = '0.0.0.0')"]
