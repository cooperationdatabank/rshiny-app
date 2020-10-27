# CoDa Shiny app
This repository holds the Cooperation Databank RShiny app [app.cooperationdatabank.org](https://app.cooperationdatabank.org). The app uses the Triply CoDa instance, hosted at [data.cooperationdatabank.org](https://data.cooperationdatabank.org). The data in this public instance is retrieved using pre-defined SPARQL queries.

## Getting started
To run the application you can use either RStudio or Docker. We strongly suggest you use Docker. 

### RStudio
1.  To run the application in RStudio checkout this repository with RStudio (or point your working directory to it, if you already checked it out).
2.  Open either the `app/ui.R` or `app/server.R` file.
3.  Click the `Run App` button in the top right corner of the edit pane.

### Docker
Prerequisites:
 - [Install docker](https://docs.docker.com/get-docker/)
 - [Install docker-compose]()
 
1.  Go to the `docker` directory.
2.  Run `docker-compose build` to build the docker image (the first run will take ~15 minutes. Subsequent runs will be a lot faster).
3.  Run `docker-compose up` to run a docker container of the image.
4.  Go to [localhost:3838](http://localhost:3838) to view the app.
# CoDa
# CoDa
