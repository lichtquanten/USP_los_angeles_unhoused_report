# Use the official R image as the base
FROM rocker/r-ver:4.3.0

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
    && rm -rf /var/lib/apt/lists/*

# Install pacman and here
RUN R -e "install.packages(c('pacman', 'here'))"

CMD ["tail", "-f", "/dev/null"]
