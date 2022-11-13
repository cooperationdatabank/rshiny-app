# Install R version 3.5
FROM rocker/shiny:3.6.3

# Install Ubuntu packages
RUN apt-get update --allow-releaseinfo-change && apt-get install -y \
    libssl-dev libpng-dev libjpeg-dev libxml2-dev zlib1g-dev libgit2-dev cmake libcurl4-openssl-dev ca-certificates \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

# Install R packages
COPY app/loadLibraries.R /tmp/
RUN R -f /tmp/loadLibraries.R
# make this dir fully writable. rshiny app needs it to install deps (that being said, in a refactory, we should definitely remove this line, as it's a bad practice)
RUN chmod 777 /usr/local/lib/R/site-library 
# Add the app
#COPY ./app /srv/shiny-server/app
#COPY ./original-app /srv/shiny-server/original
COPY ./app /srv/shiny-server/

# Make data dir writable for shiny user
#RUN mkdir -p /srv/shiny-server/app/data && chown shiny:shiny /srv/shiny-server/app/data
#RUN mkdir -p /srv/shiny-server/original/data && chown shiny:shiny /srv/shiny-server/original/data
RUN mkdir -p /srv/shiny-server/data && chown shiny:shiny /srv/shiny-server/data

# Make the ShinyApp available at port 80
EXPOSE 80
