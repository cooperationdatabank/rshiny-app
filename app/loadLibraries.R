if (!require("devtools", character.only = T)){
  install.packages("devtools")
  library(devtools) 
}

if (!require("XML")){
  install_version("XML", version = "3.99-0.3", repos = "http://cran.us.r-project.org")
}

uiPackages = c("shiny", "shinydashboard",'shinyWidgets', "plotly", "visNetwork", "tippy", "shinycssloaders", "r2d3", 'ISOcodes'
             )
serverPackages = c("shiny", "shinyBS", "scales", "metafor", "ggvis", "ggplot2", 
                   "gridExtra", "pwr", "DT", "visNetwork", "tidyverse", "RColorBrewer", 
                   "plotly", "httr", "lubridate", "ggmap", "maps", "mapdata", "maptools", "jsonlite",
                   "countrycode", "SPARQL", "GetoptLong", "igraph", "sjmisc", "purrr", "readr", "rcrossref", "shinyjs", "r2d3",
                   "metaviz",'shinyWidgets', 'googlesheets4')

for (p in c(uiPackages, serverPackages)) {
  if (!require(p, character.only = T)) {
    install.packages(p)
    require(p,character.only=TRUE)
  } 
}
