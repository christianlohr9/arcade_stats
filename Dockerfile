# Use rocker/shiny as base image for R Shiny apps
FROM rocker/shiny:4.5.1

# Set maintainer
LABEL maintainer="arcade-stats"

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
    libgeos-dev \
    libmagick++-dev \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
RUN mkdir -p /srv/arcade-stats
WORKDIR /srv/arcade-stats

# Copy renv files first for better Docker layer caching
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install renv and restore packages
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')"
RUN R -e "renv::restore()"

# Copy application files
COPY DESCRIPTION DESCRIPTION
COPY R/ R/
COPY modules/ modules/
COPY data/ data/
COPY www/ www/
COPY app.R app.R
COPY update_data.R update_data.R

# Make sure the Shiny app runs as expected
EXPOSE 3838

# Set proper permissions
RUN chmod -R 755 /srv/arcade-stats

# Remove default shiny apps
RUN rm -rf /srv/shiny-server/*

# Copy our app to the shiny server directory  
RUN cp -R /srv/arcade-stats/* /srv/shiny-server/

# Use init system to properly handle container shutdown
CMD ["/init"]