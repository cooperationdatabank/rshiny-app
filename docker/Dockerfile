# Install R version 3.5
FROM rocker/shiny:3.6.3

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    libssl-dev libpng-dev libjpeg-dev libxml2-dev zlib1g-dev libgit2-dev

# Install R packages
COPY app/loadLibraries.R /tmp/
RUN R -f /tmp/loadLibraries.R

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
