# Use the official R image as the base
FROM rocker/r-ver:4.4

# Set the working directory inside the container
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

COPY renv.lock renv.lock

# Install renv
RUN R -e "install.packages('renv')"

# Install R dependencies from renv.lock
RUN R -e "renv::restore()"
