###############################################################################
#### RShiny Coda Dashboard ####################################################
###############################################################################
#
# Coda dashboard for meta-analysis of results from academics papers.
# Authors: Ilaria Tiddi, Thomas Bohlken, Jurriaan Duyne, Simon Columbus
#
# Data is retrieved from Triply data.cooperationdatabank.org.

#### Retrieving data ####

# Load file with tests for power analyses
tests <- readRDS("tests.RDS")

# Load file with citations
citations <- read.csv("paperIDs.csv")

# Specify the api;
# for a stable release use: "https://api.coda.triply.cc/queries/coda/", 
# for a nightly release use : "https://api.coda.triply.cc/queries/coda-dev/"
api <- "https://api.coda.triply.cc/queries/coda-dev/"

sparqlendpoint <- "https://api.cooperationdatabank.org/datasets/coda-dev/databank/services/databank/sparql"

# Retrieve all study data from saved sparql queries in the coda instance. 
# Retrieve observations (need two queries)
observations1 <- content(GET(paste0(api, "dashboard/run?page=1&pageSize=10000"), add_headers(Accept = "text/csv")))
observations2 <- content(GET(paste0(api, "dashboard/run?page=2&pageSize=10000"), add_headers(Accept = "text/csv")))
observations3 <- content(GET(paste0(api, "dashboard/run?page=3&pageSize=10000"), add_headers(Accept = "text/csv")))
observationData <- bind_rows(observations1, observations2, observations3) %>%
  mutate(observationName = substr(observationName, 17, nchar(observationName))) # Remove "Effect size no. "

# Retrieve treatment info (also needs 2 queries)
supportData3 <- content(GET(paste0(api, "dashboard-support/run?page=1&pageSize=10000"), add_headers(Accept = "text/csv")))
supportData2 <- content(GET(paste0(api, "dashboard-support/run?page=2&pageSize=10000"), add_headers(Accept = "text/csv")))
supportData1 <- content(GET(paste0(api, "dashboard-support/run?page=3&pageSize=10000"), add_headers(Accept = "text/csv")))
supportData <- bind_rows(supportData1, supportData2, supportData3) %>%
  mutate(observationName = substr(observationName, 17, nchar(observationName))) # Remove "Effect size no. "

# Join observation and treatment information
observationData <- left_join(observationData, supportData,by = "observationName")

# Retrieve and join study info
# Retrieve studyInfo as text/html to preserve fields with multiple values
# If there are fields with multiple values, the values are comma-separated
# without spaces (i.e., a,b)
studyInfo <- content(GET(paste0(api, "study-characteristics/run"), add_headers(Accept = "application/json")))

# getStudyInfo is an alternative to rbind.fill that creates a data.frame from
# a list of (named) lists of unequal length.
# getStudyInfo <- function (...) 
# {
#   dargs <- list(...)
#   all.names <- unique(names(unlist(dargs)))
#   out <- do.call(rbind, lapply(dargs, `[`, all.names))
#   colnames(out) <- all.names
#   as.data.frame(out, stringsAsFactors=FALSE) %>%
#     unnest(., cols = all.names)
# }
# studyInfo <- do.call(getStudyInfo, studyInfo)

studyInfo <- bind_rows(studyInfo)

# Join observation-level data (observationData) and study-level data (studyInfo)
observationData <- inner_join(observationData, studyInfo, 
                              by = 'study', suffix = c("", ".y"))

# Separate the information about treatments into columns
observationData <- observationData %>%
  select(observation, valueNameSupport) %>%
  separate_rows(valueNameSupport,sep = "\\|") %>%
  separate(valueNameSupport, c("treat", "valuepairs"),  " ~ ") %>%
  group_by(observation, treat) %>%
  summarise(valuepairs = paste(valuepairs, collapse = "|")) %>%
  unite(treatprops, treat, valuepairs, sep = "~") %>%
  group_by(observation) %>%
  summarise(treatprops = paste(treatprops, collapse = "+")) %>%
  separate(treatprops, c("treatmentValue1", "treatmentValue2"),  "\\+") %>%
  mutate(treatmentValue1 = str_remove(treatmentValue1, ".*~")) %>%
  mutate(treatmentValue2 = str_remove(treatmentValue2, ".*~")) %>%
  inner_join(observationData, by = "observation")

# Additional data wrangling
observationData <- observationData %>%
  # Replace NA with "" to allow string matching
  mutate_at(vars(treatmentValue1:treatmentValue2), tidyr::replace_na, '') %>%
  # Generate additional identifiers for papers (paperName) and for
  # studies across substudies (studyNameGeneral)
  mutate(paperName = substr(observationName, 1, 8),
         studyNameGeneral = substr(studyName, 7, 16)) %>%
  # paperDate to paperYear only
  mutate(paperYear = substr(paperDate, 1, 4)) %>%
  # Create an indicator for substudies
  mutate(substudy = case_when(nchar(studyName) == 16 ~ 0,
                              nchar(studyName) == 17 ~ 1)) %>%
  modify(~ ifelse(. == "False", "FALSE", .)) %>%
  modify(~ ifelse(. == "True", "TRUE", .)) %>%
  modify(~ ifelse(. == "False,True", "FALSE,TRUE", .))

# Citations should be generated from DOIs, but this information is incomplete.
# Instead of using dois, generate citations from a static file;
# the citations are read into the `citations` object.
# Generate title, year, and author list.
# paperTitle is title;
# paperYear is the year of publication, stripping characters indicating multiple publications (2000a, 2000b, ...)
# authorNames is a list of authors separated by ampersands
observationData <- citations %>%
  select(paper_ID, Title, Authors...year) %>%
  rename(paperTitle = Title,
         citation = Authors...year) %>%
  mutate(paperYear = str_extract_all(citation, "[:digit:]+"),
         authorNames = str_replace_all(str_replace_all(str_replace_all(str_replace(citation, 
                                                                                   " \\([:alnum:]+\\)", ""), 
                                                                       ", &", ","), 
                                                       " &", ","),
                                       ", ", ",")) %>%
  unnest(paperYear) %>%
  merge(observationData, ., 
        by.x = "paperName", by.y = "paper_ID",
        all.x = TRUE,
        suffixes = c("DOI", ""))

#### Generating selectors ####

# The authornames in the data are concatenated per paper in the SPARQL query, we keep that structure but also use a
# list with all the Authors separately for selection.
# authorList <- unique(observationData$authorNames)
authors <- trimws(unlist(str_split(unique(observationData$authorNames),",")))

# For the selections of the Independent variable and its categories we only use the list of relations between the ivs and their categories.
subPropertyList <- unique(observationData$treatmentSubproperties)
selections <- content(GET(paste0(api, "taxonomy-iv-prop/run"), add_headers(Accept = "text/csv")))

# The same is done for the values that are relevant for effect sizes
# To accommodate booleans, set the valueName based on the range (to 0,1), then separate into rows
selectionsValues <- content(GET(paste0(api, "taxonomy-iv-prop-values/run"), add_headers(Accept = "text/csv"))) %>%
  mutate(valueName = ifelse(treatmentSubpropertyRange == "http://www.w3.org/2001/XMLSchema#boolean", "0,1", valueName)) %>%
  separate_rows(valueName, sep = ",")

# We get a list of moderators with labels from a query
studyMods <- content(GET(paste0(api, "moderators/run"), add_headers(Accept = "text/csv")))
# For ranges, replace the range with low and high limits
mod_list <- c("Age Low" = "ageLow", "Age High" =  "ageHigh",
              "Lowest number of choices" ="choiceLow",
              "Highest number of choices" =  "choiceHigh", setNames(studyMods$moderator[-c(1,2)], studyMods$label[-c(1,2)]))

# Get Selectors for External Country Data moderators
external_country_moderators <- content(GET(paste0(api, "country-moderators/run"), add_headers(Accept = "text/csv")))

countryMods <- external_country_moderators$pLabel

# some are "simple". some return blank nodes. here we split them
country_moderators_simple <- external_country_moderators %>%
  filter ( !startsWith(p, "https://data.cooperationdatabank.org/") )

country_moderators_bn <- external_country_moderators %>%
  filter ( startsWith(p, "https://data.cooperationdatabank.org/") )

# read citation data
citation_nodes <- read.csv("www/data/nodesTd.csv", header=T)
citation_edges <-  read.csv("www/data/edgesTd.csv")   %>%
  rename(
    from = Source,
    to = Target
  )

#### Shiny App ####

shinyServer(function(input, output, session) {

  ####### Variables and whole dashboard functions ######
  current_es_val <- reactiveVal(0.1)

  observeEvent(input$reset_input, {
    shinyjs::reset("whole_dashboard")
  })

  ####### Populate input fields #####
       ## Helper functions ######

    generateCategoryString <- function (inputfield) {
      if (is.null(inputfield) ){
       NULL
      } else if (inputfield == "") {
        NULL } else {
        description <- selectionsValues %>%
          filter(ivname %in% inputfield) %>%
          select(ivname, ivDescription) %>%
          distinct() %>%
          select(ivDescription)
        description <- paste0("",inputfield,": ", description)
      }
    }

    generateSubpropString <- function (inputfield) {
      if (is.null(inputfield)){
        NULL
      } else if (inputfield == "") {
        NULL } else {
        description <- selectionsValues %>%
          filter(treatmentSubproperty %in% inputfield) %>%
          select(treatmentSubproperty, treatmentSubpropertyDescription) %>%
          distinct() %>%
          select(treatmentSubpropertyDescription)
        description <- paste0("", inputfield,": ", description)
      }
    }

    generateValueString <- function (inputfield, treatment) {
      if (is.null(inputfield)){
        NULL
      } else if (inputfield == "") {
        NULL } else {
          description <- selectionsValues %>%
            filter(treatmentSubproperty %in% treatment, valueName %in% inputfield) %>%
            select(valueName, valueDescription) %>%
            distinct() %>%
            select(valueDescription)
          description <- paste0("", inputfield,": ", description)
        } 
      
    }
    
    
    ## TODO: what does this do?
    ivLabelsGen <- function(genIVselection){
      if (is.null(genIVselection)) {
        return(NULL)
      } else {
        subprops <- selections %>%
          filter(
            ( # independent variable filter
              is.null(genIVselection) | ivname %in% genIVselection
            )
          )
        subprops <- subprops$treatmentSubproperty
        return(sort(unique(subprops)))
      }
    }

    valueOptionUpdateGen <- function(subProp) {
      if (is.null(subProp)) {
        NULL
      } else{
        values <- selectionsValues %>%
          filter(
            ( # independent variable filter
              treatmentSubproperty %in% subProp
            )
          )
        result <- values$valueName
        return(sort(unique(result)))
      }
    }

       ## Make a selection for treatment 1 ######

    observe({
      updateSelectInput(session, "genIVselection1a", choices = c("",sort(selections$ivname)))
    })

    observe({
      updateSelectInput(session, "treatmentSubpropSelection1a", choices = c("",ivLabelsGen(input$genIVselection1a)))
    })

    observe({
      updateSelectInput(session, "valueOptionsSelection1a", choices = c("",valueOptionUpdateGen(input$treatmentSubpropSelection1a)))
    })

    observe({
      updateSelectInput(session, "addGenIVselectionTreatment1", choices = c("",sort(selections$ivname)))
    })

    observe({
      updateSelectInput(session, "addTreatmentSubpropSelectionTreatment1", choices = c("",ivLabelsGen(input$addGenIVselectionTreatment1)))
    })

    observe({
      value_choice <- valueOptionUpdateGen(input$addTreatmentSubpropSelectionTreatment1)
      allow_create <- FALSE
      if (identical(value_choice, character(0))){
        allow_create <- TRUE
      }
      updateSelectizeInput(session, "addValueOptionsSelectionTreatment1",
                        choices = c("", value_choice), options = list(create = allow_create))
    })

    output$descriptionCategory1a <- renderUI({
      em(class = "text-muted",
        generateCategoryString(input$genIVselection1a)
      )
    })

    output$descriptionSubprop1a <- renderUI({
      em(class = "text-muted",
        generateSubpropString(input$treatmentSubpropSelection1a)
      )
    })
    
    output$descriptionValue1a <- renderUI({
      em(class = "text-muted",
         generateValueString(input$valueOptionsSelection1a,input$treatmentSubpropSelection1a)
      )
    })

    output$selectionCriteria1optional <- renderUI({
      if(!is.null(input$extraCriteria1) & input$extraCriteria1 > 0){
          lapply(1:input$extraCriteria1, function(i) {
            fluidPage(
            selectInput(inputId = paste0("genIVselection1",letters[i+1]), #name of input used to be "gen_iv", also removed selectInput for current_iv
                        label = paste0("Extra Generic IV (T1",letters[i],")"), #label displayed in ui
                        choices = selections$ivname, multiple= FALSE),
            selectInput(inputId = paste0("treatmentSubpropSelection1",letters[i+1]), #name of input used to be "gen_iv", also removed selectInput for current_iv
                        label = paste0("Extra Specific IV (T1",letters[i],")"), #label displayed in ui
                        choices = "", multiple= FALSE, selected = ""),
            selectInput(inputId = paste0("valueOptionsSelection1",letters[i+1]),
                        label = paste0("Extra Specific IV values (T1",letters[i],")"), #label displayed in ui
                        choices = "", selected = "", multiple= FALSE),
            hr()
            )
          })
      }
    })

    observe({
      lapply(1:input$extraCriteria1, function(i) {
          updateSelectInput(session, paste0("treatmentSubpropSelection1",letters[i+1]), choices = c("",ivLabelsGen(input[[paste0("genIVselection1",letters[i+1])]])),
                            selected = input[[paste0("treatmentSubpropSelection1",letters[i+1])]])
        })
      })

    observe({
      lapply(1:input$extraCriteria1, function(i) {
        updateSelectInput(session, paste0("valueOptionsSelection1",letters[i+1]), choices = c("",valueOptionUpdateGen(input[[paste0("treatmentSubpropSelection1",letters[i+1])]])),
                          selected = input[[paste0("valueOptionsSelection1",letters[i+1])]])
      })
    })

       ## Make a selection for treatment 2 ######


    observe({
      updateSelectInput(session, "genIVselection2a", choices = c("",sort(selections$ivname)))
    })

    observe({
      updateSelectInput(session, "treatmentSubpropSelection2a", choices = c("",ivLabelsGen(input$genIVselection2a)))
    })

    observe({
      updateSelectInput(session, "valueOptionsSelection2a", choices = c("",valueOptionUpdateGen(input$treatmentSubpropSelection2a)))
    })

    output$descriptionCategory2a <- renderUI({
      em(class = "text-muted",
        generateCategoryString(input$genIVselection2a)
      )
    })

    output$descriptionSubprop2a <- renderUI({
      em(class = "text-muted",
        generateSubpropString(input$treatmentSubpropSelection2a)
      )
    })
    
    output$descriptionValue2a <- renderUI({
      em(class = "text-muted",
         generateValueString(input$valueOptionsSelection2a,input$treatmentSubpropSelection2a)
      )
    })
    

    output$selectionCriteria2optional <- renderUI({
      if(!is.null(input$extraCriteria2) & input$extraCriteria2 > 0){
        lapply(1:input$extraCriteria2, function(i) {
          fluidPage(
            selectInput(inputId = paste0("genIVselection2",letters[i+1]), #name of input used to be "gen_iv", also removed selectInput for current_iv
                        label = paste0("Extra Generic IV (T2",letters[i],")"), #label displayed in ui
                        choices = selections$ivname, multiple= FALSE),
            selectInput(inputId = paste0("treatmentSubpropSelection2",letters[i+1]), #name of input used to be "gen_iv", also removed selectInput for current_iv
                        label = paste0("Extra Specific IV (T2",letters[i],")"), #label displayed in ui
                        choices = "", multiple= FALSE, selected = ""),
            selectInput(inputId = paste0("valueOptionsSelection2",letters[i+1]),
                        label = paste0("Extra Specific IV values (T2",letters[i],")"), #label displayed in ui
                        choices = "", selected = "", multiple= FALSE),
            hr()
          )
        })
      }
    })


    observe({
      lapply(1:input$extraCriteria2, function(i) {
        updateSelectInput(session, paste0("treatmentSubpropSelection2",letters[i+1]), choices = c("",ivLabelsGen(input[[paste0("genIVselection2",letters[i+1])]])),
                          selected = input[[paste0("treatmentSubpropSelection2",letters[i+1])]])
      })
    })

    observe({
      lapply(1:input$extraCriteria2, function(i) {
        updateSelectInput(session, paste0("valueOptionsSelection2",letters[i+1]), choices = c("",valueOptionUpdateGen(input[[paste0("treatmentSubpropSelection2",letters[i+1])]])),
                          selected = input[[paste0("valueOptionsSelection2",letters[i+1])]])
      })
    })





       ## Author selection #####

    # Author selection update
    authorUpdate <- reactive({
      if (is.null(input$treatmentSubpropSelection1a)|input$treatmentSubpropSelection1a == "") {
        return(sort(authors))
      } else if (!is.null(input$treatmentSubpropSelection1a)) {
         tempData <- observationData %>%
          filter(
              ( # independent variable filter
                is.null(input$treatmentSubpropSelection1a) | treatmentSubproperties %in% input$treatmentSubpropSelection1a
              )
          )
        authors <- sort(unlist(str_split(unique(tempData$authorNames)," & ")))
        return(authors)
      }
    })

  observe({
    updateSelectizeInput(session, "author", choices = authorUpdate(), server = TRUE)
  })
  
  ## lang
  
  observe({
    updateSelectizeInput(session, "lang", choices = sort(unique(substr(observationData$observationName, 1,3))) )
  })
  
  # countries
  
  observe({
    updateSelectInput(session, "filter_countries", choices = sort(unique(countrycode(unlist(str_split(observationData$country, ",")), origin = "iso3c", destination = "country.name"))))
  })
  
  observe({
    updateSelectInput(session, "studyAcquaintance", choices = sort(unique(unlist(str_split(observationData$studyAcquaintance, ",")) )))
  })

  observe({
    updateSelectInput(session, "addStudyAcquaintance", choices = sort(unique(unlist(str_split(observationData$studyAcquaintance, ",")) )))
  })


       ## Moderator selection meta-analysis #####

  observe({
    updateSelectInput(session, "moderator_characteristics", choices=mod_list[sort(names(mod_list))] )
  })

  ivLabelsMod <- reactive({
    if (is.null(input$genIVselection1a) | input$genIVselection1a == "") {
      return(NULL)
    } else {
      filterSet <- c(input$genIVselection1a,input$genIVselection1b,input$genIVselection1c,input$genIVselection1d,input$genIVselection1e,
                     input$genIVselection2a,input$genIVselection2b,input$genIVselection2c,input$genIVselection2d,input$genIVselection2e)
      values <- selections %>%
        filter(ivname %in% filterSet)

      exclude <- c(input$treatmentSubpropSelection1a,input$treatmentSubpropSelection1b,input$treatmentSubpropSelection1c,input$treatmentSubpropSelection1d,input$treatmentSubpropSelection1e,
                       input$treatmentSubpropSelection2a,input$treatmentSubpropSelection2b,input$treatmentSubpropSelection2c,input$treatmentSubpropSelection2d,input$treatmentSubpropSelection2e)
      result <- sort(unique(setdiff(values$treatmentSubproperty,exclude)))
    }
  })

  observe({
    updateSelectInput(session, "vis_moderator_variables", choices = get_all_mods())
  })

  observe({
    updateSelectInput(session, "moderator_variables", choices = ivLabelsMod())
  })

  observe({
    updateSelectInput(session, inputId = "country_moderators", choices=countryMods)
  })
  
  observe({
    updateSelectInput(session, "multilevel_variables", choices = c("Study" = "studyNameGeneral", "Paper" = "paperName", "Country/region" = "country"))
  })


       ## Moderator selection meta-regression #####
  
  observe({
    updateSelectInput(session, "moderator_characteristics_reg", choices=mod_list[sort(names(mod_list))])
  })
  
  observe({
    updateSelectInput(session, "vis_moderator_variables_reg", choices = get_all_mods_reg())
  })
  
  observe({
    updateSelectInput(session, "moderator_variables_reg", choices = ivLabelsMod())
  })
  
  observe({
    updateSelectInput(session, inputId = "country_moderators_reg", choices=countryMods)
  })
  
  observe({
    updateSelectInput(session, "multilevel_variables_reg", choices=c("Study" = "studyNameGeneral", "Paper" = "paperName", "Country/region" = "country"))
  })
  
  ####### Data reslicing in App ######

       ## Helper functions ####
  # helper function for evaluating selection input
  # evaluateList <- function(datalist, criteria){
  #   return (sapply(datalist, function(x) { all(str_detect(x, fixed(criteria, ignore_case = TRUE)))}, USE.NAMES = FALSE))
  # }
  
  evaluateList <- function(datalist, criteria){
    return (sapply(datalist, function(x) { all(str_contains(x, criteria, ignore.case = TRUE))}, USE.NAMES = FALSE))
  }

  remove_url <- function(x) {
    x <- gsub(".*[\\/#]", "", x)
    x <- gsub(">", "", x)
    return(x)
  }
  
  # A function to select observations based on treatmentSubpropSelection and valueOptionsSelection
  # selectorNumber indicates the first vs. second treatment in the selection interface
  # treatmentNumber indicates the treatmentValue column (treatmentValue1 vs. treatmentValue2)
  
  selectObservations <- function(data, selectorNumber, treatmentNumber){
    selectedObservations <- list()
    for(number in 1:(as.numeric(input[[paste0("extraCriteria", selectorNumber)]])+1)){
      letter <- letters[number]
      
      treatmentSelection <- ifelse(is.null(input[[paste0("treatmentSubpropSelection", selectorNumber, letter)]]) |
                                     input[[paste0("treatmentSubpropSelection", selectorNumber, letter)]] == "",
                                   "",
                                   paste0(input[[paste0("treatmentSubpropSelection", selectorNumber, letter)]], 
                                          " : ", 
                                          input[[paste0("valueOptionsSelection", selectorNumber, letter)]]))
      
      selectedObservations[[number]] <- (evaluateList(data[[paste0("treatmentValue", treatmentNumber)]],
                                            treatmentSelection))
    }
    
    selectedObservations <- if(input[[paste0("combinator", selectorNumber)]] == "AND"){
      Reduce("&", selectedObservations)
    } else {
      Reduce("|", selectedObservations)
    }
    
    return(selectedObservations)
  }
  
  # Function to split valueName columns and select only the value(s)
  # for the subproperty specified by var
  getValueName <- function(var, valueName){
    value <- lapply(valueName,
                    function(x) unlist(strsplit(x, "\\|"))[grepl(var, unlist(strsplit(x, "\\|")))]) %>%
      lapply(., function(x) strsplit(x, " : ")) %>% 
      lapply(., function(x) map_chr(x, 2)) %>% 
      sapply(., function(s) if (length(s) == 0) NA_character_ else paste(s, collapse = ","))
    value <- unlist(value)
    return(value)
  }

  # Function to create moderator variables for meta-analysis
  # `mod` takes a value from input$definemod
  # valueName1 and valueName2 take filteredObservationData$valueName1 and
  # filteredObservationData$valueName2
  # Creates three types of moderator variables:
  # 1) Both Treatment 1 and Treatment 2 have a value
  # 2) Only Treatment 1 has a non-numeric value
  # 3) Only Treatment 1 has a numeric value (continuous)
  
  defineModerators <- function(mod, valueName1, valueName2){
    value1 <- getValueName(mod, valueName1)
    value2 <- getValueName(mod, valueName2)
    
    if(any(!is.na(value2))){
      values <- cbind(value1, value2) %>%
      data.frame() %>%
      mutate(!!mod := paste("Treatment 1: ", value1, " vs. Treatment 2: ", value2)) %>%
      select(!!mod) %>%
      mutate(across(everything(), ~gsub("NA", "none/NA", .)))
    } else if(all(is.na(value2)) & any(grepl("^[A-Za-z]+$", value1))){
      values <- cbind(value1, value2) %>%
        data.frame() %>%
        mutate(!!mod := paste("Treatment 1: ", value1)) %>%
        select(!!mod) %>%
        mutate(across(everything(), ~gsub("NA", "none/NA", .)))
    } else if(all(is.na(value2)) & all(!grepl("^[A-Za-z]+$", value1))){
      values <- cbind(value1, value2) %>%
        data.frame() %>%
        mutate(!!mod := as.numeric(value1)) %>%
        select(!!mod)
    }
    return(values)
  }

  # Function to create the moderator columns for each moderator in
  # input$definemod and bind them to the dataframe.
  bindModerators <- function(mod, data){
    for (i in 1:length(mod)){
      mod_def <- defineModerators(mod[[i]],
                                  data$treatmentValue1,
                                  data$treatmentValue2)
      data <- cbind(data, mod_def)
    }
    return(data)
  }
  
  # Function to create moderators for metaregression
  # `mod` takes a value from input$definemod
  # valueName1 takes filteredObservationData$valueName1
  defineRegModerators <- function(mod, valueName1){
    value <- getValueName(mod, valueName1)
    
    if(any(grepl("^[A-Za-z]+$", value))){
      values <- value %>%
        data.frame() %>%
        mutate(!!mod := value) %>%
        select(!!mod) %>%
        mutate(across(everything(), ~gsub("NA", "none/NA", .)))
    } else if(all(!grepl("^[A-Za-z]+$", value))){
      values <- value %>%
        data.frame() %>%
        mutate(!!mod := as.numeric(value)) %>%
        select(!!mod)
    }
    
    return(values)
  }
  
  # Function to create the moderator columns for each moderator in
  # input$definemod and bind them to the dataframe.
  bindRegModerators <- function(mod, data){
    for (i in 1:length(mod)){
      mod_def <- defineRegModerators(mod[[i]],
                                     data$treatmentValue1)
      data <- cbind(data, mod_def)
    }
    return(data)
  }
  
  # Function to split and paste observationName into treatment_IDs
  splitObservationName <- function(observationName){
    vec <- lapply(observationName,
                  function(x){
                    paste(strsplit(as.character(x),
                                   "\\.")[[1]][c(1, 2, 4)], collapse = ".")
                  })
    vec <- unlist(vec)
    return(vec)
  }

       ## Filter data based on selections ####
  filteredObservationData <- reactive({
    
    # Function to check whether any value within a comma-separated string
    # matches any value in a vector of strings.
    checkSelection <- function(value, criterion){
      map_lgl(str_split(value, ","), ~ any(.x %in% criterion))
    }
    
    # Function to check whether any value within a comma-separated string falls
    # within a given range, with the range given by a vector with two elements.
    checkRange <- function(value, range){
      map_lgl(str_split(value, ","), ~ any(as.numeric(.x) >= range[1] & as.numeric(.x) <= range[2]))
    }

    filteredObservationData <- observationData
    
    ##### treatment selection #####
    if(!(is.null(input$treatmentSubpropSelection1a) | input$treatmentSubpropSelection1a == "")){
      filteredObservationData <- filteredObservationData %>%
        dplyr::filter((selectObservations(., 1, 1) & selectObservations(., 2, 2)) |
                        (selectObservations(., 1, 2) & selectObservations(., 2, 1)))
    }
    
    ##### metadata #####
    # The list of papers is updated with the selection of individual authors
    
    if (!is.null(input$author) && input$author != "") {
      filteredObservationData <- filteredObservationData %>%
        filter(
          checkSelection(authorNames, input$author)
        )
    }
    
    if (!is.null(input$lang) && input$lang != "") {
      filteredObservationData <- filteredObservationData %>%
        filter(substr(observationName, 1,3) %in% input$lang
        )
    }
    
    if ( input$overallN[1] > 0 || input$overallN[2] < 3000){
      filteredObservationData <- filteredObservationData %>%
        filter( # sample size filter
          checkRange(overallN, input$overallN)
        )
    }
    
    if ( !is.null(input$publicationStatus) ) { 
      filteredObservationData <- filteredObservationData %>%
        filter( 
          checkSelection(publicationStatus, input$publicationStatus)
        )   
    }
    
    ##### qualitative #####
    if ( input$deception != 'All'){
      filteredObservationData <- filteredObservationData %>%
        filter(  # deception filter
          checkSelection(deception, input$deception)
        ) 
    }
    
    if ( input$studySymmetric != "All"){
      filteredObservationData <- filteredObservationData %>%
        filter(  # symmetric boolean filter
          checkSelection(studySymmetric, input$studySymmetric)
        )
    }
    
    if (input$studyKnownEndgame != 'All'){
      filteredObservationData <- filteredObservationData %>%
        filter(   # endGameEffect boolean filter
          checkSelection(studyKnownEndgame, input$studyKnownEndgame)
        )
    }
    
    if (!is.null(input$studyDilemmaType)){
      filteredObservationData <- filteredObservationData %>%
        filter(  # gameType filter
          checkSelection(studyDilemmaType, input$studyDilemmaType)
        )
    }
    
    if (input$studyContinuousPGG != "All"){
      filteredObservationData <- filteredObservationData %>%
        filter(  # continuous pgg
          checkSelection(studyContinuousPGG, input$studyContinuousPGG)
        )
    }
    
    if (!is.null(input$studyExperimentalSetting)){
      filteredObservationData <- filteredObservationData %>%
        filter(   # experimental setting filter
          checkSelection(studyExperimentalSetting, input$studyExperimentalSetting)
        )
    }

    if ( input$studyOneShot != 'All' ){
      filteredObservationData <- filteredObservationData %>%
        filter( # one shot or not (one shot vs iterated)
          checkSelection(studyOneShot, input$studyOneShot)
        )
    }
    
    if ( input$studyOneShotRepeated != 'All' ){
      filteredObservationData <- filteredObservationData %>%
        filter( # one shot repeated
          checkSelection(studyOneShotRepeated, input$studyOneShotRepeated)
        )
    }

    if ( input$studyMatchingProtocol != 'All'){
      filteredObservationData <- filteredObservationData %>%
        filter(  # partner matching filter
          checkSelection(studyMatchingProtocol, input$studyMatchingProtocol)
        )
    }
    
    if ( !is.null(input$studyShowUpFee)  ){
      filteredObservationData <- filteredObservationData %>%
        filter(   # showup fee filter
          checkSelection(studyShowUpFee, input$studyShowUpFee)
        )
    }
    
    if ( !is.null(input$studyGameIncentive) ){
      filteredObservationData <- filteredObservationData %>%
        filter( # paymentGame filter
          checkSelection(studyGameIncentive, input$studyGameIncentive)
        )
    }
    
    if ( !is.null(input$discussion) ){
      filteredObservationData <- filteredObservationData %>%
        filter(  # discussion filter
          checkSelection(discussion, input$discussion)
        )
    }
    
    if (!is.null(input$participantDecision) ){
      filteredObservationData <- filteredObservationData %>%
        filter( # participant decision
          checkSelection(participantDecision, input$participantDecision)
        )
    }
    
    if ( !is.null(input$studyRealPartner) ){
      filteredObservationData <- filteredObservationData %>%
        filter(  # real participants
          checkSelection(studyRealPartner, input$studyRealPartner)
        )
    }
    
    if (  !is.null(input$studyAcquaintance) ){
      filteredObservationData <- filteredObservationData %>%
        filter( # acquaintance filter
          checkSelection(studyAcquaintance, input$studyAcquaintance)
        )
    }

    if ( input$sanction != 'All' ){
      filteredObservationData <- filteredObservationData %>%
        filter( # sanction filter
          checkSelection(sanction, input$sanction)
        )
    }
    
    if ( input$studyNumberOfChoices[1] > 0 || input$studyNumberOfChoices[2] < 20 ) {
      filteredObservationData <- filteredObservationData %>%
        filter(  # choice number filter
          checkRange(studyNumberOfChoices, input$studyNumberOfChoices)
        )
    }
    
     if (  input$choiceLow[1] > 0 || input$choiceLow[2] < 3 ){
       filteredObservationData <- filteredObservationData %>%
         filter( # Choice low filter
           checkRange(choiceLow, input$choiceLow)
         )
     }

    if ( input$choiceHigh[1] > 0 || input$choiceHigh[2] < 15  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # Choice high filter -very high numbers found, should they be filtered out?
           checkRange(choiceHigh, input$choiceHigh)
        )
    }
    
    if ( input$studyKindex[1] > 0 || input$studyKindex[2] < 1  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # k-index filter
          checkRange(studyKindex, input$studyKindex)
        )
    }
    
    if ( input$studyMPCR[1] > 0 || input$studyMPCR[2] < 1  ){
      filteredObservationData <- filteredObservationData %>%
        filter(# MPCR filter
          checkRange(studyMPCR, input$studyMPCR)
        )
    }
    
    if ( input$studyGroupSize[1] > 0 || input$studyGroupSize[2] < 400  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # group size filter
            checkRange(studyGroupSize, input$studyGroupSize)
        )
    }

    if ( input$replenishmentRate[1] > 0 || input$replenishmentRate[2] < 25  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # Replenishment rate
          checkRange(replenishmentRate, input$replenishmentRate)
        )
    }

    if ( input$studyPGDThreshold[1] > 0 || input$studyPGDThreshold[2] < 25  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # Provision point filter
          checkRange(studyPGDThreshold, input$studyPGDThreshold)
        )
    }
    
    ##### sample char #####
    if (!is.null(input$yearOfDataCollection[1]) && input$yearOfDataCollection[1] != "" && !is.null(input$yearOfDataCollection[2]) && input$yearOfDataCollection[2] != "") {
      filteredObservationData <- filteredObservationData %>%
        filter(# year of paper filter
          checkRange(yearOfDataCollection, input$yearOfDataCollection)
        )
    }
    
    if ( input$studyStudentSample != 'All'  ){
      filteredObservationData <- filteredObservationData %>%
        filter(  # studentsOnly filter
          checkSelection(studyStudentSample, input$studyStudentSample)
        )
    }
    
    if ( !is.null(input$studyAcademicDiscipline) ){
      filteredObservationData <- filteredObservationData %>%
        filter( # discipline filter
          checkSelection(studyAcademicDiscipline, input$studyAcademicDiscipline)
        )
    }
    
    if ( !is.null( input$yearSource )  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # yearSource filter
          checkSelection(yearSource, input$yearSource)
        )
    }
    # country filter 
    if (! is.null(input$filter_countries) & nrow(filteredObservationData) > 0) {
      filteredObservationData <- filteredObservationData %>% filter(
        checkSelection(country, countrycode(input$filter_countries,  destination = "iso3c", origin  = "country.name"))
        )
    } 
    
    if ( !is.null(input$countrySource)   ){
      filteredObservationData <- filteredObservationData %>%
        filter( # countrySource filter
          checkSelection(countrySource, input$countrySource)
        )
    }
    
    if ( !is.null(input$recruitmentMethod)  ){
      filteredObservationData <- filteredObservationData %>%
        filter(  # recruitment filter
          checkSelection(recruitmentMethod, input$recruitmentMethod)
        )
    }
    
    if ( input$meanAge[1] > 0 || input$meanAge[2] < 100  ){
      filteredObservationData <- filteredObservationData %>%
        filter(  # mean age filter
          checkRange(meanAge, input$meanAge)
        )
    }
    
    if ( input$maleProportion[1] > 0 || input$maleProportion[2] < 1  ){
      filteredObservationData <- filteredObservationData %>%
        filter(  # male proportion filter
          checkRange(maleProportion, input$maleProportion)
        )
    }
    if ( input$ageHigh[1] > 0 || input$ageHigh[2] < 100  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # age high filter
          checkRange(ageHigh, input$ageHigh)
        )
    }
    if ( input$ageLow[1] > 0 || input$ageLow[2] < 100  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # age low filter
          checkRange(ageLow, input$ageLow)
        )
    }
    if ( input$overallN[1] > 0 || input$overallN[2] < 3000  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # sample size filter
          checkRange(overallN, input$overallN)
        )
    }
    
    ##### quantitative #####
    if ( input$numberOfObservations[1] > 0 || input$numberOfObservations[2] < 2500  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # numberOfObs filter
          checkRange(numberOfObservations, input$numberOfObservations)
         )
    }
    
    if ( input$overallMeanContributions[1] > 0 || input$overallMeanContributions[2] < 15  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # mean cooperation filter
          checkRange(overallMeanContributions, input$overallMeanContributions)
        )
    }
    
    if ( input$overallStandardDeviation[1] > 0 || input$overallStandardDeviation[2] < 10  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # overall SD filter
          checkRange(overallStandardDeviation, input$overallStandardDeviation)
        )
    }
    
    if ( input$overallProportionCooperation[1] > 0 || input$overallProportionCooperation[2] < 10  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # overall PC filter
          checkRange(overallProportionCooperation, input$overallProportionCooperation)
        )
    }
    
    if ( input$overallMeanWithdrawal[1] > 0 || input$overallMeanWithdrawal[2] < 10  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # mean withdrawal filter
          checkRange(overallMeanWithdrawal, input$overallMeanWithdrawal)
        )
    }
    
    if ( input$overallPercentageEndowmentContributed[1] > 0 || input$overallPercentageEndowmentContributed[2] < 1  ){
      filteredObservationData <- filteredObservationData %>%
        filter( # Proportion of endowment contributed
          checkRange(overallPercentageEndowmentContributed, input$overallPercentageEndowmentContributed)
        )
    }
    
    if ( !is.null(input$studyTrialOfCooperation)  ){
      filteredObservationData <- filteredObservationData %>%
        filter(  # periods filter
          checkSelection(studyTrialOfCooperation, input$studyTrialOfCooperation)
        )
    }

     # end filters
    
    # Return output 
    filteredObservationData
  }
  )

       ## Align direction of effects with selection ####

  matchedData <- reactive({
    filteredObservationData <- filteredObservationData()

    # Helper functions
    ## Checks whether a variable has a certain level.
    checkLevel <- function(data, var, level){
      evaluateList(data,
                   paste0(var,
                          " : ",
                          level))
    }
    
    ## For a 'level' type variable to be aligned with a continuous individual
    ## difference variable, checks whether the levels of treatments
    ## are ordered in line with the individual difference variable
    matchLevel <- function(var, level1, level2, level3){
      filteredObservationData %>% 
        dplyr::mutate(match = case_when(!evaluateList(treatmentValue1, var) ~ NA,
                                 checkLevel(treatmentValue1, var, level3) ~ TRUE,
                                 checkLevel(treatmentValue1, var, level2) & 
                                   checkLevel(treatmentValue2, var, level1) ~ TRUE,
                                 checkLevel(treatmentValue1, var, level2) & 
                                   checkLevel(treatmentValue2, var, level3) ~ FALSE,
                                 checkLevel(treatmentValue1, var, level1) ~ FALSE))
    }
    
    # Check whether the first selected property is contained in treatment 1
    orderMatch <- selectObservations(filteredObservationData, 1, 1)
    
    # For each continuous variable with a levels equivalent, check whether 
    # the level treatments were ordered in line with selection.
    if(input$treatmentSubpropSelection1a == "individual difference" & 
       !input$valueOptionsSelection1a == "svo_value_orientation"){
      levelMatch <- matchLevel("individual difference level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    # How to deal with unclassified?
    if(input$valueOptionsSelection1a == "social_value_orientation"){
      levelMatch <- filteredObservationData %>% 
        mutate(match = case_when(!evaluateList(treatmentValue1, "svo type") ~ NA,
                                 checkLevel(treatmentValue1, "svo type", "altruist") ~ TRUE,
                                 checkLevel(treatmentValue1, "svo type", "prosocial") & 
                                   (checkLevel(treatmentValue2, "svo type", "individualist") |
                                      checkLevel(treatmentValue2, "svo type", "proself") |
                                      checkLevel(treatmentValue2, "svo type", "competitor")) ~ TRUE,
                                 checkLevel(treatmentValue1, "svo type", "individualist") & 
                                   (checkLevel(treatmentValue2, "svo type", "proself") |
                                      checkLevel(treatmentValue2, "svo type", "competitor")) ~ TRUE,
                                 checkLevel(treatmentValue1, "svo type", "proself") & 
                                   checkLevel(treatmentValue2, "svo type", "competitor") ~ TRUE,
                                 checkLevel(treatmentValue1, "svo type", "proself") & 
                                   (checkLevel(treatmentValue2, "svo type", "individualist") |
                                      checkLevel(treatmentValue2, "svo type", "prosocial") |
                                      checkLevel(treatmentValue2, "svo type", "altruist")) ~ FALSE,
                                 checkLevel(treatmentValue1, "svo type", "individualist") & 
                                   (checkLevel(treatmentValue2, "svo type", "prosocial") |
                                      checkLevel(treatmentValue2, "svo type", "altruist")) ~ FALSE,
                                 checkLevel(treatmentValue1, "svo type", "prosocial") & 
                                   checkLevel(treatmentValue2, "svo type", "altruist") ~ FALSE,
                                 checkLevel(treatmentValue2, "svo type", "altruist") ~ FALSE,
                                 checkLevel(treatmentValue1, "svo type", "competitor") ~ FALSE))
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "age"){
      levelMatch <- matchLevel("age cohort", "young", "middle", "old")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "academic grade"){
      levelMatch <- matchLevel("academic grade level", "junior", "middle", "senior")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "expectations"){
      levelMatch <- matchLevel("expectations level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "identification"){
      levelMatch <- matchLevel("identification level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "entitativity"){
      levelMatch <- matchLevel("entitativity level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "hormone"){
      levelMatch <- matchLevel("hormone level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "social capital"){
      levelMatch <- matchLevel("social capital level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "state trust"){
      levelMatch <- matchLevel("state trust level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "participant's own behavior (correlation)"){
      levelMatch <- matchLevel("participant's behavior level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "partner's behavior (correlation)"){
      levelMatch <- matchLevel("partner's behavior level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "game comprehension"){
      levelMatch <- matchLevel("game comprehension level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$treatmentSubpropSelection1a == "participant's behavior (correlation)"){
      levelMatch <- matchLevel("participant's behavior level", "low", "medium", "high")
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    if(input$valueOptionsSelection1a == "decision time (correlation)"){
      levelMatch <- filteredObservationData %>% 
        mutate(match = case_when(!evaluateList(treatmentValue1, "decision time") ~ NA,
                                 checkLevel(treatmentValue1, "decision time", "fast") ~ TRUE,
                                 checkLevel(treatmentValue1, "decision time", "slow") ~ FALSE))
      orderMatch <- ifelse(is.na(levelMatch$match), orderMatch, levelMatch$match)
    }
    
    
    # Reverse effect size estimate
    filteredObservationData <- filteredObservationData %>%
      mutate(effectSize = ifelse(orderMatch, effectSize, effectSize * (-1)),
             LL = effectSizeLowerLimit,
             UL = effectSizeUpperLimit,
             effectSizeLowerLimit = ifelse(orderMatch, LL, UL * (-1)),
             effectSizeUpperLimit = ifelse(orderMatch, UL, LL * (-1))) %>%
      select(-LL, -UL)
    
    # Reverse the two 'valueName' columns
    filteredObservationData <- filteredObservationData %>%
      mutate(vN1 = ifelse(orderMatch, treatmentValue1, treatmentValue2),
             vN2 = ifelse(orderMatch, treatmentValue2, treatmentValue1)) %>%
      select(-treatmentValue1, -treatmentValue2) %>%
      rename(treatmentValue1 = vN1,
             treatmentValue2 = vN2)
    
    filteredObservationData
  })
  
       ## Data overview table and selection ####
  
  # Create the table and table output. This has three parts: 
  # Create the wide-format (spread) data frame as reactive; 
  # Output the rendered table;
  # Return the selected observations in long (gather) format.
  
  # Create a wide-format data frame
  selectableObservationData <- reactive({
    
    selectable <- matchedData() %>%
      select(paper, study, citation, paperTitle, DOI, 
             observationName, studyName, effectSize, effectSizeMeasure) %>%
      # Drop the last part of observationName, indicating the effectSizeMeasure
      mutate(observationName = sub('^([^.]+(?:.[^.]+){3}).*', '\\1', observationName),
             effectSize = round(effectSize, 3)) %>%
      distinct() %>%   #This is a bit of an ugly hack to make sure that an observation is not displayed multiple times
      # Spread to get separate columns for d and r
      spread(., effectSizeMeasure, effectSize) %>%
      # Add d or r column if it doesn't exist
      add_column(!!!c(d = NA_real_, r = NA_real_)[setdiff(c("d", "r"), names(.))])
    selectable
  })
  
  # Render data overview table
  output$render_data <- DT::renderDataTable(DT::datatable(

    selectableObservationData() %>%
      # Recreate links
      mutate(linkR = paste0(paper, "_", sub('^[^_]*_', '\\1', observationName), ".r"),
             linkD = paste0(paper, "_", sub('^[^_]*_', '\\1', observationName), ".d"),
             linkNA = paste0(paper, "_", sub('^[^_]*_', '\\1', observationName), ".0")) %>%
      # Create linked table inputs
      mutate(Title = paste0("<a href='", paper,"' target='_blank'>", paperTitle,"</a>"),
             DOI = paste0("<a href='", DOI,"' target='_blank'>", DOI,"</a>"),
             Study = paste0("<a href='", study,"' target='_blank'>", studyName,"</a>"),
             `Effect ID` = ifelse(!is.na(d), 
                                  paste0(observationName,
                                         " [<a href='", linkD,"' target='_blank'>d</a>]",
                                         " [<a href='", linkR,"' target='_blank'>r</a>]"),
                                  paste0("<a href='", linkNA,"' target='_blank'>", observationName,"</a>"))) %>%
      rename(Citation = citation) %>%
      select(c(`Effect ID`, Citation, Title, d, r, Study, DOI)),
    options = list(pageLength = 10, 
                   scrollX = TRUE, 
                   dom = 'lftp'),  # dom : control (see https://datatables.net/reference/option/dom)
    class="stripe" , #selection = 'none' ,
    
    escape = FALSE )
  )
  
  # Data with rows selected in table __excluded__
  selectedObservationData <- reactive({
    
    filtered <- matchedData() %>%
      mutate(observationNameGeneric = sub('^([^.]+(?:.[^.]+){3}).*', '\\1', observationName))
    selected <- selectableObservationData()
    if (!is.null(input$render_data_rows_selected)) {
      selected <- selected %>% filter(! (row_number() %in% input$render_data_rows_selected))
      filtered <- filtered %>%
        filter(observationNameGeneric %in% selected$observationName)
    }
    
    shiny::validate( need(nrow(filtered) > 0, "Your selection did not result in a valid output, please change your selection") )
    
    filtered
  })

  # Counter for Forest plots height,
  metaRegressionForestHeightCount <- 0
  metaAnalysisForestHeightCount <- 0
  
       ## metaAnalysisSelection ####
  
  metaAnalysisSelection <- reactive({
    
    filteredObservationData <- selectedObservationData()
    
    # Filter to selected effect size measure
    filteredObservationData <- filteredObservationData %>%
      filter(effectSizeMeasure == input$es_measure)
    
    # Filter out overall study iff substudies are present
    filteredObservationData <- filteredObservationData %>%
      group_by(studyNameGeneral) %>%
      filter(all(substudy == 1) | all(substudy == 0) | substudy == 1) %>%
      ungroup()
    
    # Filter out cases where both treatments have equivalent values
    filteredObservationData <- filteredObservationData %>%
      filter(!(selectObservations(., 1, 1) == selectObservations(., 1, 2) & selectObservations(., 2, 1) == selectObservations(., 2, 2)))
    
    # Create references for meta-analysis output
    filteredObservationData <- filteredObservationData %>%
      mutate(reference = paste(sub(" &.*"," et al.", authorNames),
                               "(", paperYear, ")", ", ",
                               paste(gsub("(?:[^.]+\\.){2}([^.]+).*", "\\1", observationName),
                                     gsub("(?:[^.]+\\.){3}([^.]+).*", "\\1", observationName),
                                     sep = " vs. "))) %>%
      # Sort by effect size
      arrange(effectSize)
    
    # Create treatment moderators
    if(!is.null(input$moderator_variables)){
      filteredObservationData <- bindModerators(mod = input$moderator_variables,
                                                data = filteredObservationData)
    }
    
    # Mutate multi-valued cases to NA
    filteredObservationData <- filteredObservationData %>%
      mutate_at(vars(mod_list), ~ifelse(grepl(",", .), NA, .))

    # Munge study moderators
    filteredObservationData <- purrr::map_at(filteredObservationData, mod_list, type.convert, na.strings = c(NA, "-999", "")) %>%
      bind_cols() 
    
    # External country moderator 
    if (!is.null(input$country_moderators)) {
      for(moderator in input$country_moderators){
        predicate <- external_country_moderators$p[external_country_moderators$pLabel == moderator]
        pAltName <- external_country_moderators$pAltName[external_country_moderators$pLabel == moderator]

        if(moderator %in% country_moderators_simple$pLabel){
          query <- qq("
                      SELECT ?country (?value as ?@{pAltName}) WHERE {
                      ?country <@{predicate}> ?value .
                      }")
         
            query_result <- SPARQL(sparqlendpoint, query)$results
            query_result["country"] <- lapply(query_result["country"], remove_url)
            query_result[pAltName] <- lapply(query_result[pAltName], remove_url)
            
            if(!is.na(as.numeric(query_result[[pAltName]]))){
              query_result[pAltName] <- as.numeric(query_result[[pAltName]])
            }
            
            names(query_result)[2] <- moderator
            
            filteredObservationData <- left_join(filteredObservationData, query_result, by = "country")
        } else if(moderator %in% country_moderators_bn$pLabel){
          query <- qq("
                      PREFIX cdp: <https://data.cooperationdatabank.org/vocab/prop/>
                      SELECT ?country (STR(?Year) as ?year) (?value as?@{pAltName}) WHERE {
                      ?country <@{predicate}> [
                      cdp:year ?Year;
                      cdp:value ?value
                      ]
                      }")
          
            query_result <- SPARQL(sparqlendpoint, query)$results
            
            query_result <- query_result %>%
              mutate(country = remove_url(country),
                     year = as.numeric(year))
            
            names(query_result)[3] <- moderator
            
            filteredObservationData <- filteredObservationData %>%
              merge(., query_result, by.x = c("country", "yearOfDataCollection"), by.y = c("country", "year"))
        }
    }
    }

    metaAnalysisForestHeightCount <<- nrow(filteredObservationData)
    
    filteredObservationData
    
    })
  
       ## metaRegressionSelection ####
  
  get_all_mods_reg <- reactive({
    return(c(input$moderator_variables_reg, 
             input$moderator_characteristics_reg, 
             input$country_moderators_reg))
  })
  
  metaRegressionSelection <- reactive({
    filteredObservationData <- selectedObservationData()
    
    # Create moderators
    if(!is.null(input$moderator_variables_reg)){
      filteredObservationData <- bindRegModerators(mod = c(input$moderator_variables_reg),
                                                   data = filteredObservationData)
    }
    
    # Mutate multi-valued cases to NA
    filteredObservationData <- filteredObservationData %>%
      mutate_at(vars(mod_list), ~ifelse(grepl(",", .), NA, .))
    
    # Munge study moderators
    filteredObservationData <- purrr::map_at(filteredObservationData, mod_list, type.convert, na.strings = c(NA, "-999", "")) %>%
      bind_cols() 
    
    # External Country moderator adding JD added for country moderators
    if (!is.null(input$country_moderators_reg)) {
      for(moderator in input$country_moderators_reg){
        predicate <- external_country_moderators$p[external_country_moderators$pLabel == moderator]
        pAltName <- external_country_moderators$pAltName[external_country_moderators$pLabel == moderator]

        if(moderator %in% country_moderators_simple$pLabel){
          query <- qq("
                      SELECT ?country (?value as ?@{pAltName}) WHERE {
                      ?country <@{predicate}> ?value .
                      }")
          
          query_result <- SPARQL(sparqlendpoint, query)$results
          query_result["country"] <- lapply(query_result["country"], remove_url)
          query_result[pAltName] <- lapply(query_result[pAltName], remove_url)
          
          if(!is.na(as.numeric(query_result[[pAltName]]))){
            query_result[pAltName] <- as.numeric(query_result[[pAltName]])
          }
          
          names(query_result)[2] <- moderator
          
          filteredObservationData <- left_join(filteredObservationData, query_result, by = "country")
        } else if(moderator %in% country_moderators_bn$pLabel){
          query <- qq("
                      PREFIX cdp: <https://data.cooperationdatabank.org/vocab/prop/>
                      SELECT ?country (STR(?Year) as ?year) (?value as?@{pAltName}) WHERE {
                      ?country <@{predicate}> [
                      cdp:year ?Year;
                      cdp:value ?value
                      ]
                      }")
          
          query_result <- SPARQL(sparqlendpoint, query)$results
          
          query_result <- query_result %>%
            mutate(country = remove_url(country),
                   year = as.numeric(year))
          
          names(query_result)[3] <- moderator
          
          filteredObservationData <- filteredObservationData %>%
            merge(., query_result, by.x = c("country", "yearOfDataCollection"), by.y = c("country", "year"))
        }
      }
    }
    
    # Filter out rows with NAs for any of the selected moderators
    # These rows would be dropped from the analysis in any case.
    if (!is.null(c(input$moderator_variables_reg, 
                   input$moderator_characteristics_reg, 
                   input$country_moderators_reg))){
      filteredObservationData <- filteredObservationData %>%
        drop_na(get_all_mods_reg())
    }
    
    filteredObservationData <- filteredObservationData %>%
      # Sample the first variable per study, retaining all treatments that fit the
      # selection criteria. Sampling one variable avoids duplicates if multiple
      # variables fit the selection criteria.
      mutate(variable_ID = sub( "(^[^.]+[.][^.]+)(.+$)", "\\1", observationName),
             treatment_1 = sub( "(^[^.]+[.][^.]+[.][^.]+)(.+$)", "\\1", observationName),
             treatment_2 = splitObservationName(observationName)) %>%
      group_by(studyName) %>%
      # filter(variable_ID %in% sample(unique(variable_ID), 1)) %>%
      # slice(sample(row_number())) %>%
      filter(as.numeric(sub('.*\\.', '', variable_ID)) == min(as.numeric(sub('.*\\.', '', variable_ID)))) %>%
      # Transform wide to long; one row per treatment
      as.data.frame() %>%
      reshape(.,
              varying = list(c("treatmentValue1", "treatmentValue2"),
                             c("treatment_1", "treatment_2")),
              v.names = c("treatmentValue", "treatment_ID"),
              direction = "long") %>%
      # Filter treatment by selection
      group_by(treatment_ID) %>%
      dplyr::filter(str_contains(treatmentValue,
                                 paste0(input$treatmentSubpropSelection1a, " : ", 
                                        input$valueOptionsSelection1a), 
                                 ignore.case = TRUE)) %>%
      # Sample one row per treatment
      # This is necessary because data are organised by observation; i.e., a
      # treatment may occur in more than one observation.
      # sample_n(1) %>%
      filter(as.numeric(sub('.*\\.', '', treatment_ID)) == min(as.numeric(sub('.*\\.', '', treatment_ID)))) %>%
      ungroup() %>%
      # Filter out overall study iff substudies are present
      group_by(studyNameGeneral) %>%
      filter(all(substudy == 1) | all(substudy == 0) | substudy == 1) %>%
      ungroup() %>%
      # Reference for plotting
      mutate(reference = paste(sub(" &.*"," et al.", authorNames),
                               "(", paperYear, ")", ", ",
                               gsub("(?:[^.]+\\.){2}([^.]+).*", "\\1", observationName))) %>%
      # Get log-transformed proportion of contribution and coefficient of variance
      mutate(logPropContributed = as.numeric(getValueName("Log-transformed proportion of endowment contributed / rate of cooperation", treatmentValue)),
             coefficientOfVariation = as.numeric(getValueName("Coefficient of variation", treatmentValue)),
             sampleSize = getValueName("Sample size in the experimental condition", treatmentValue)) %>%
      # Drop NAs to show labels on plots
      drop_na(logPropContributed, coefficientOfVariation) %>%
      arrange(logPropContributed)

    metaRegressionForestHeightCount <<- nrow(filteredObservationData)
    
    filteredObservationData
    
    })
  
  observeEvent(input$helpSelection, {
    sendSweetAlert(
      title = "",
      text = div(
        style = "text-align:left;",
        p(class="text-muted", "You can select treatments that manipulate or measure one or more independent variables used to predict the dependent variable: cooperation (all effect sizes in the databank include cooperation as the dependent variable). Define Treatments 1 and 2 to meta-analyse observations that compare the two treatments. To run a meta-regression on mean cooperation rates, only define Treatment 1"),
        p(class="text-muted", "For each treatment, follow the steps below:"),
        tags$ul(
          tags$li(p(class="text-muted",  " Step 1: Select a Generic Independent Variable")),
          tags$li(p( class="text-muted", " Step 2: Select a Specific Independent Variable")),
          tags$li(p(class="text-muted",  " Step 3: Select a Specific Independent Variable Value")),
          tags$li(p(class="text-muted",  " Step 4 (optional): Additionally specify your selection by selecting 'Would you like to select more criteria?' (up to 5 additional criteria)"))
        ),
        p(class="text-muted","For additional criteria you can combine your selections using 'AND' or 'OR'."), 
        tags$ul(
          tags$li(p(class="text-muted",  " The 'AND' option will further restrict the treatments to ensure the treatments have both criteria")),
          tags$li(p( class="text-muted", " The 'OR' option will add to the selection of treatments all treatments that have either criteria"))
          ),
        p(class="text-muted", "You can further select studies based on the sample and study characteristics (e.g., proportion of males in the sample, group size), quantitative study results (e.g., trial of cooperation), and paper metadata (e.g., year of publication)."),
        p(class="text-muted", "For further information see our ", a(href = "https://cooperationdatabank.org/tutorials/", "tutorials", target="_blank"), ".")
        ),
      type = "success",
      session = session
    )  
  })
  
  observeEvent(input$clearFilters, {
    shinyjs::reset("whole_dashboard")
    output$forests_reg_ui <- renderUI({plotOutput("get_forests_reg", height = paste0(toString(metaRegressionForestHeightCount*10 + 400),'px')) })
    output$get_forests_ui <- renderUI({
      plotOutput("get_forests", height = paste0(toString(metaAnalysisForestHeightCount*10 + 400),'px'))
    })
    regressionForms <<- data.frame(matrix(ncol = countColumnsRegression, nrow = 0))
    analysisForms <<-  data.frame(matrix(ncol = countColumns, nrow = 0))

  })
  

  ####### Front page ######
       ## World map #####
  output$countries <- renderPlotly({
    # summarise effect size by country
    
    sel <- selectedObservationData()
    
    shiny::validate(
      need(nrow(sel) > 0, "Your selection did not result in a valid output, please change your selection")
    )
    
    summm <- sel %>%
      # split comma-separated values for country into separate rows
      separate_rows(country, sep = ",") %>%
      group_by(country) %>%
      summarise(number_of_measurements =  log(length(unique(study)))) %>%
      # need to convert country codes (iso3) to country names
      mutate(countryName = countrycode(country, origin = "iso3c", destination = "country.name"))
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )
    
    p <- plot_geo(summm) %>%
      add_trace(
        z = ~number_of_measurements, color = ~number_of_measurements, colors = 'Oranges',
        text = ~countryName, locations = ~country, marker = list(line = l)
      ) %>%
      colorbar(title = 'Number of studies\n(log-scale) per country') %>%
      layout(
        geo = g
      )
    
    p
  })
  
       ## Alternative to worldmap #####
  output$show_studies <- renderPlotly({
    
    sel <- selectedObservationData()
    
    shiny::validate(
      need(nrow(sel) > 0, "Your selection did not result in a valid output, please change your selection")
    )
    
    if (input$moderator == "overallN") {
      x_axis <- 'Study sample size'
      df <- sel %>%
        group_by(overallN) %>%
        summarise(number_of_measurements = length(unique(study))) %>%
        mutate(overallN = as.numeric(overallN))
      
      plot_ly(x = df %>% pull(input$moderator), y= df$number_of_measurements, hoverinfo="text",
              mode = "markers",   type="scatter",  source = "source", size = df$number_of_measurements,
              text= paste0("<b>Sample size :",df$overallN, "</b><br>number of studies: <b>",df$number_of_measurements,"</b>")
      )   %>%
        layout(xaxis = list(title= x_axis ), yaxis = list( title="Number of published studies"))
      
      
    } else {
      x_axis <- "Year of data collection"
      
      df <- sel %>%
        separate_rows(yearOfDataCollection, sep = ",") %>%
        group_by(yearOfDataCollection) %>%
        summarise(number_of_measurements = length(unique(study)))
      
      plot_ly(x = df %>% pull(input$moderator), y= df$number_of_measurements, hoverinfo="text",
              mode = "markers",   type="scatter",  source = "source", size = df$number_of_measurements
              , text= paste0("<b>",df$yearOfDataCollection, "</b><br>Published studies: <b>",df$number_of_measurements,"</b>") 
      )   %>%
        layout(xaxis = list(title= x_axis ), yaxis = list( title="Number of published studies"))
      
    } 
    
  })
  
       ## Counters ######
  output$summary <- renderUI({
    
    n <- selectedObservationData() %>%
      group_by(studyNameGeneral) %>%
      filter(all(substudy == 1) | all(substudy == 0) | substudy == 1) %>%
      ungroup() %>%
      group_by(study) %>%
      sample_n(1) %>%
      ungroup()
    
    fluidRow(
      valueBox(width = 3,  subtitle =  "Number of papers", length(unique(selectedObservationData()$paper)), icon = icon("copy"), color = "light-blue"),
      valueBox( width = 3, subtitle = "Number of studies", length(unique(selectedObservationData()$study)), icon = icon("vial"), color = "purple"),
      valueBox( width = 3, subtitle = "Number of effects", length(unique(selectedObservationData()$observationNameGeneric)), icon = icon("search"), color = "aqua"),
      valueBox( width = 3, subtitle = "Total participants", sum(as.numeric(n$overallN), na.rm = TRUE) , icon = icon("users"), color = "red")
    )
  })
  
  
       ## Example filters ######
  
  observeEvent(input$treatmentSubpropSelection1a, {
    if (is.null(input$treatmentSubpropSelection1a) | 
        input$treatmentSubpropSelection1a == ""){
      hide(id = "resultBox" )
      hide(id ="modelBox" )
      hide(id = "resultBoxReg" )
      hide(id ="modelBoxReg" )
      show(selector = "div.validationBox" )
      hide(id = "addedPaperData")
      hide(id = "analysisForm")
      hide(id = "addedPaperDataRegression")
      hide(id = "regressionForm")
    } else {
      show(id = "resultBox" )
      show(id ="modelBox" )
      show(id = "resultBoxReg" )
      show(id ="modelBoxReg" )
      hide(selector = "div.validationBox" )
    }
  })
  
  observeEvent(input$hidePlots,{
    if (input$hidePlots == TRUE) {
      hide(selector ='div#showSummary' )
    } else {
      show(selector ='div#showSummary' )
    }
  })

  observeEvent(input$selectionExample , {
    if (input$selectionExample == 1){
      shinyjs::reset("All Selections") #does not work will reset input$selectionExample too of course
      shinyjs::delay(500, {
        updateSelectInput(session, "genIVselection1a", choices = c("",sort(selections$ivname)), selected = "Personality")
        updateSelectInput(session, "treatmentSubpropSelection1a", choices = c("","Individual difference"), selected = "Individual difference")
        updateSelectInput(session, "valueOptionsSelection1a", choices = c("","Honesty-humility"), selected = "Honesty-humility")
        updateSelectInput(session, inputId = "selectionExample", label=NULL,
                          choices = c("Analyze the relationship between Honesty-Humility and cooperation." = 1, "Reset and display all options" = 4),
                          selected  = 1)
      })
    } else if (input$selectionExample == 2) {
      #   "Analyse the effect of Punishment on cooperation, comparing a punishment treatment vs any other treatment."
      shinyjs::reset("All Selections") #does not work will reset input$selectionExample too of course
      shinyjs::delay(500, {
        updateSelectInput(session, "genIVselection1a", choices = c("",sort(selections$ivname)), selected = "Punishment")
        updateSelectInput(session, "treatmentSubpropSelection1a", choices = c("","Punishment treatment"), selected = "Punishment treatment")
        updateSelectInput(session, "valueOptionsSelection1a", choices = c("","1"), selected = "1")
      })
      updateSelectInput(session, inputId = "selectionExample", label=NULL,
                        choices = c("Analyze the effect of punishment on cooperation, comparing a punishment treatment vs. any other treatment." = 2, "Reset and display all options" = 4),
                        selected  = 2)
    } else if (input$selectionExample == 3) {
      #  "Compare the effects of punishment vs. reward on cooperation."
      shinyjs::reset("All Selections") #does not work will reset input$selectionExample too of course
      shinyjs::delay(500, {
        updateSelectInput(session, "genIVselection1a", choices = c("",sort(selections$ivname)), selected = "Punishment")
        updateSelectInput(session, "treatmentSubpropSelection1a", choices =  c("","Punishment treatment"), selected = "Punishment treatment")
        updateSelectInput(session, "valueOptionsSelection1a", choices = c("","1"), selected = "1")
         updateSelectInput(session, "genIVselection2a", choices = c("","Reward"), selected = "Reward")
          updateSelectInput(session, "treatmentSubpropSelection2a", choices = c("","Reward treatment"), selected = "Reward treatment")
          updateSelectInput(session, "valueOptionsSelection2a", choices = c("","1"), selected = "1")
      })
      updateSelectInput(session, inputId = "selectionExample", label=NULL,
                        choices = c("Compare the effects of punishment vs. reward on cooperation." = 3, "Reset and display all options" = 4),
                        selected  = 3)
    } else if (input$selectionExample == 4) {
      shinyjs::reset("whole_dashboard")
      updateSelectInput(session,inputId = "selectionExample", choices = c("Select..." = 0,
                                                                          "Analyze the relationship between Honesty-Humility and cooperation." = 1,
                                                                          "Analyze the effect of punishment on cooperation, comparing a punishment treatment vs. any other treatment." = 2,
                                                                          "Compare the effects of punishment vs. reward on cooperation." = 3
      ))
      output$forests_reg_ui <- renderUI({plotOutput("get_forests_reg", height = paste0(toString(metaRegressionForestHeightCount*10 + 400),'px')) })
      output$get_forests_ui <- renderUI({
        plotOutput("get_forests", height = paste0(toString(metaAnalysisForestHeightCount*10 + 400),'px'))
      })
      analysisForms <<-  data.frame(matrix(ncol = countColumns, nrow = 0))
      regressionForms <<- data.frame(matrix(ncol = countColumnsRegression, nrow = 0))
    }
    #print(input$selectionExample)
  })
  
       ## Downloads ######
  
  downloadableData <- reactive({
    
    filtered <- selectedObservationData() 
    
    col1 <- filtered %>%
      select(observationName, treatmentValue1) %>%
      separate_rows(treatmentValue1, sep = "\\|") %>%
      separate(treatmentValue1, sep = " : ", into = c("var", "val")) %>%
      group_by(observationName, var) %>%
      mutate(val = paste0(val, collapse = "|")) %>%
      ungroup() %>%
      distinct() %>%
      filter(var != "NA")
    
    col2 <- filtered %>%
      select(observationName, treatmentValue2) %>%
      separate_rows(treatmentValue2, sep = "\\|") %>%
      separate(treatmentValue2, sep = " : ", into = c("var", "val")) %>%
      drop_na() %>%
      group_by(observationName, var) %>%
      mutate(val = paste0(val, collapse = "|")) %>%
      ungroup() %>%
      distinct()
    
    test <- merge(col1, col2, by = c("observationName", "var"), all = TRUE) %>%
      mutate(valuepair = paste(val.x, val.y, sep = " vs. ")) %>%
      mutate(valuepair = gsub("NA", "none", valuepair)) %>%
      drop_na()
      
    test <- test %>%  
      pivot_wider(id_cols = observationName, names_from = var, values_from = valuepair) 
    
    filtered <- test %>%
      merge(filtered, ., by = "observationName", all.x = TRUE) %>%
      select(observationName, citation, paperTitle, effectSize:effectSizeUpperLimit,
             country:yearSource, observationNameGeneric, sort(names(test)))
    
    filtered
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(downloadableData(), con, col.names = TRUE, row.names = FALSE)
    }
  )
  
  get_refs <- reactive({
    DOIS <- selectedObservationData() %>%
      select(DOI) %>%
      unique()
    
    withProgress(message = 'Downloading reference data', detail = "starting download", value = 0, {
      result <- character()
      for (i in 1:length(DOIS[[1]])) {
        # Each time through the loop, add another row of data. This a stand-in
        # for a long-running computation.
        result <- c(result, cr_cn(DOIS[[1]][i],  format = "bibtex"))
        # Increment the progress bar, and update the detail text.
        incProgress(1/length(DOIS[[1]]), detail = paste("reference", i))
      }
    })
    
    return(result)
  })
  
  output$downloadRefs <- downloadHandler(
    filename = function() {
      paste('bibref-', Sys.Date(), '.bib', sep='')
    },
    content = function(con) {
      write.csv(selectedObservationData(), con, col.names = TRUE)
      readr::write_lines(get_refs(), con)
    }
  )
  
  get_refs_apa <- reactive({
    DOIS <- selectedObservationData() %>%
      select(DOI) %>%
      unique()
    
    withProgress(message = 'Downloading reference data', detail = "starting download", value = 0, {
      result <- character()
      for (i in 1:length(DOIS[[1]])) {
        # Each time through the loop, add another row of data. This a stand-in
        # for a long-running computation.
        result <- c(result, cr_cn(DOIS[[1]][i],  format = "text", style = "apa"))
        # Increment the progress bar, and update the detail text.
        incProgress(1/length(DOIS[[1]]), detail = paste("reference", i))
      }
    })
    
    #result <- cr_cn(DOIS[[1]],  format = "text", style = "apa") %>%
    #  purrr::map_chr(., purrr::pluck, 1)
    
    return(result)
  })
  
  output$downloadRefsAPA <- downloadHandler(
    filename = function() {
      paste('aparef-', Sys.Date(), '.rtf', sep='')
    },
    content = function(con) {
      write.csv(selectedObservationData(), con, col.names = TRUE)
      readr::write_lines(get_refs_apa(), con)
    }
  )
  
  ####### Meta-analysis tab #####
       ## Interpretation message #####
  get_interpretation <- reactive({
    
    # Function to generate strings describing treatments from inputs
    # Each string has the form treatmentSubprop1: valueOption1 AND/OR ...
    treatmentString <- function(treatment){
      treatmentSubprops <- list()
      valueOptions <- list()
      numSubprop <- ifelse(is.null(input[[paste0("extraCriteria", treatment)]]), 1, 
                           as.numeric(input[[paste0("extraCriteria", treatment)]]) + 1)
      for (i in 1:numSubprop){
        treatmentSubprops[[i]] <- input[[paste0("treatmentSubpropSelection", treatment, letters[[i]])]]
        valueOptions[[i]] <- input[[paste0("valueOptionsSelection", treatment, letters[[i]])]]
      }  
      treatmentSubprops <- unlist(treatmentSubprops)
      valueOptions <- unlist(valueOptions)
      
      output <- paste(treatmentSubprops, valueOptions, sep = ": ", collapse = paste0(" ", input[[paste0("combinator", treatment)]], " "))
      return(output)
    }
    
    # Function to check whether any treatmentSubprop == "individual difference"
    checkIndividualDifference <- function(treatment){
      treatmentSubprops <- list()
      numSubprop <- ifelse(is.null(input[[paste0("extraCriteria", treatment)]]), 1, 
                           as.numeric(input[[paste0("extraCriteria", treatment)]]) + 1)
      for (i in 1:numSubprop){
        treatmentSubprops[[i]] <- input[[paste0("treatmentSubpropSelection", treatment, letters[[i]])]]
      }  
      return(any(treatmentSubprops == "individual difference"))
    }
    
    metaInterpretation <- case_when(
      #### Only Treatment 1 ####
      # Single individual difference variable
      input$extraCriteria1 == 0 &
        input$treatmentSubpropSelection2a == "" &
        input$treatmentSubpropSelection1a == "individual difference" ~ paste0(
          "Meta-analysis of the effect of ", input$valueOptionsSelection1a, " on cooperation. A positive estimate indicates that cooperation is higher for larger values of ", input$valueOptionsSelection1a, "."),
      # Two individual difference variable, OR
      input$extraCriteria1 == 1 &
        input$treatmentSubpropSelection2a == "" &
        input$treatmentSubpropSelection1a == "individual difference" &
        any(!is.null(input$treatmentSubpropSelection1b) & input$treatmentSubpropSelection1b == "individual difference") &
        input$combinator1 == "OR" ~ paste0(
          "Meta-analysis of the effect of ", input$valueOptionsSelection1a, " OR ", input$valueOptionsSelection1b, " on cooperation. A positive estimate indicates that cooperation is higher for larger values of ", input$valueOptionsSelection1a, " OR ", input$valueOptionsSelection1b, "."),
      # Three individual difference variable, OR
      input$extraCriteria1 == 2 &
        input$treatmentSubpropSelection2a == "" &
        input$treatmentSubpropSelection1a == "individual difference" &
        any(!is.null(input$treatmentSubpropSelection1b) & input$treatmentSubpropSelection1b == "individual difference") &
        any(!is.null(input$treatmentSubpropSelection1c) & input$treatmentSubpropSelection1c == "individual difference") &
        input$combinator1 == "OR" ~ paste0(
          "Meta-analysis of the effect of ", input$valueOptionsSelection1a, " OR ", input$valueOptionsSelection1b, " OR ", input$valueOptionsSelection1c, " on cooperation. A positive estimate indicates that cooperation is higher for larger values of ", input$valueOptionsSelection1a, " OR ", input$valueOptionsSelection1b, " OR ", input$valueOptionsSelection1b, "."),
      # Four individual difference variable, OR
      input$extraCriteria1 == 3 &
        input$treatmentSubpropSelection2a == "" &
        input$treatmentSubpropSelection1a == "individual difference" &
        any(!is.null(input$treatmentSubpropSelection1b) & input$treatmentSubpropSelection1b == "individual difference") &
        any(!is.null(input$treatmentSubpropSelection1c) & input$treatmentSubpropSelection1c == "individual difference") &
        any(!is.null(input$treatmentSubpropSelection1d) & input$treatmentSubpropSelection1d == "individual difference") &
        input$combinator1 == "OR" ~ paste0(
          "Meta-analysis of the effect of ", input$valueOptionsSelection1a, " OR ", input$valueOptionsSelection1b, " OR ", input$valueOptionsSelection1c, " OR ", input$valueOptionsSelection1d, " on cooperation. A positive estimate indicates that cooperation is higher for larger values of ", input$valueOptionsSelection1a, " OR ", input$valueOptionsSelection1b, " OR ", input$valueOptionsSelection1b, " OR ", input$valueOptionsSelection1d, "."),
      # Five individual difference variable, OR
      input$extraCriteria1 == 4 &
        input$treatmentSubpropSelection2a == "" &
        input$treatmentSubpropSelection1a == "individual difference" &
        any(!is.null(input$treatmentSubpropSelection1b) & input$treatmentSubpropSelection1b == "individual difference") &
        any(!is.null(input$treatmentSubpropSelection1c) & input$treatmentSubpropSelection1c == "individual difference") &
        any(!is.null(input$treatmentSubpropSelection1d) & input$treatmentSubpropSelection1d == "individual difference") &
        any(!is.null(input$treatmentSubpropSelection1e) & input$treatmentSubpropSelection1e == "individual difference") &
        input$combinator1 == "OR" ~ paste0(
          "Meta-analysis of the effect of ", input$valueOptionsSelection1a, " OR ", input$valueOptionsSelection1b, " OR ", input$valueOptionsSelection1c, " OR ", input$valueOptionsSelection1d, " OR ", input$valueOptionsSelection1e, " on cooperation. A positive estimate indicates that cooperation is higher for larger values of ", input$valueOptionsSelection1a, " OR ", input$valueOptionsSelection1b, " OR ", input$valueOptionsSelection1c, " OR ", input$valueOptionsSelection1d, " OR ", input$valueOptionsSelection1e, "."),
      # Single individual difference AND single categorical variable
      input$extraCriteria1 == 1 &
        input$treatmentSubpropSelection2a == "" &
        input$treatmentSubpropSelection1a == "individual difference" &
        any(is.null(input$treatmentSubpropSelection1b) | !(input$treatmentSubpropSelection1b == "individual difference")) &
        input$combinator1 == "AND" ~ paste0(
          "Meta-analysis of the effect of ", input$valueOptionsSelection1a, " on cooperation, under the condition of ", input$treatmentSubpropSelection1b, ": ", input$valueOptionsSelection1b, " A positive estimate indicates that in treatments of ", input$treatmentSubpropSelection1b, ": ", input$valueOptionsSelection1b, " cooperation is higher for larger values of ", input$valueOptionsSelection1a, "."),
      # Single categorical variable AND single individual difference variable
      input$extraCriteria1 == 1 &
        input$treatmentSubpropSelection2a == "" &
        !(input$treatmentSubpropSelection1a == "individual difference") &
        any(!is.null(input$treatmentSubpropSelection1b) & input$treatmentSubpropSelection1b == "individual difference") &
        input$combinator1 == "AND" ~ paste0(
          "Meta-analysis of the effect of ", input$valueOptionsSelection1b, " on cooperation, under the condition of ", input$treatmentSubpropSelection1a, ": ", input$valueOptionsSelection1a, " A positive estimate indicates that in treatments of ", input$treatmentSubpropSelection1a, ": ", input$valueOptionsSelection1a, " cooperation is higher for larger values of ", input$valueOptionsSelection1b, "."),
      # Single individual difference OR single categorical variable
      ## Not yet implemented
      # Any other individual difference variable
      input$treatmentSubpropSelection2a == "" &
        checkIndividualDifference(1) ~ "Unfortunately, no interpretation is available for this analysis.",
      !input$treatmentSubpropSelection2a == "" &
        checkIndividualDifference(2) ~ "Unfortunately, no interpretation is available for this analysis.",
      # Categorical variables, only treatment 1
      input$treatmentSubpropSelection2a == "" ~ paste0(
        "Meta-analysis of Treatment 1 (", treatmentString(1), ") compared to any other treatment on cooperation. A positive effect size indicates that cooperation is higher in Treatment 1 (", treatmentString(1), ") compared to any other treatment."),
      #### Treatment 1 vs. Treatment 2 ####
      !input$treatmentSubpropSelection2a == "" ~ paste0(
        "Meta-analysis comparing Treatment 1 (", treatmentString(1), ") against Treatment 2 (", treatmentString(2),") on cooperation. A positive effect size indicates that cooperation is higher in Treatment 1 (", treatmentString(1), ") than in Treatment 2 (", treatmentString(2), ")."),
      # All else
      TRUE ~ "Unfortunately, no interpretation is available for this analysis."
    )
    
    return(metaInterpretation)
  })
  
  get_moderator_baseline <- reactive({

    # Select factors, converting chr to fct
    data <- metaAnalysisSelection() %>%
      select(get_all_mods()) %>%
      mutate_if(is_character, as_factor) %>%
      select_if(is.factor) 
    
    if(ncol(data) > 0){
      
      baselines <- list()
      
      # Find reference levels
      referenceLevels <- lapply(data, function(x) sort(levels(x))[1])
      
      for(i in 1:ncol(data)){
      
        baselines[[i]] <- paste0("Reference level for ", names(data)[i], ": ", referenceLevels[[i]], "\n")
      }
      
      output <- do.call(paste, c(baselines, sep = "<br/>"))
    } else {
      output <- ""
    }
    
    return(output)
  })
  
  output$interpretation <- renderUI({
    tagList(
      p("The following interpretation is derived from the selections that you have made:"),
      p(paste0(get_interpretation())),
      p(
        HTML(get_moderator_baseline())
      ),
      br()
    )
  })
  
       ## Meta-analytic models ####

  get_all_mods <- reactive({
    return(c(input$moderator_variables, input$moderator_characteristics, input$country_moderators))
  })
  
  output$moderator_description <- renderUI({
     mods <- get_all_mods()
     modsno <- length(mods)
     
     
     if(!is.null(modsno) & modsno > 0){
        lapply(1:modsno, function(val) {
          column(12,
                 
                 uiOutput(paste0("line_", val)))
        })
     }
     
  })
  
  observe({
    mods <- get_all_mods()
    modsno <- length(mods)
    
    lapply(1:modsno, function(val) {
        output[[paste0("line_", val)]] <- renderUI(p(strong(mods[val]), 
                                                     em(selections[which(selections$treatmentSubproperty == mods[val] ) , ]$treatmentDesc), 
                                                     em(studyMods[which(studyMods$moderator == mods[val]),]$desc),
                                                     em(external_country_moderators[which(external_country_moderators$pLabel == mods[val] ) , ]$desc) ) )
      }) 
  
  })

  calculate_meta_analysis <- function(dataSelection) {
    # Helper functions
    changeRoundingOutput <- function(numberToRound){
      result <- as.numeric(round(numberToRound,3))
    }

    # Define moderator input
    ## One vector containing all selected or created moderators
    all_mods <- get_all_mods()

    ## Moderator formula
    if(length(all_mods) == 0){  # Or use a different condition
      mods <- NULL
    }
    if(length(all_mods) > 0){
      mods <- as.formula(paste("~", paste0("`", all_mods,"`" , collapse = " + ")))
    }

    # Multilevel formula
    # Note that this is hardcoded to only allow study, paper and country,
    # with studies nested within papers and otherwise crossed factors.
    if(length(input$multilevel_variables) == 0){
      random = NULL
    } else if(length(input$multilevel_variables) == 1){
      random = as.formula(paste0("~ 1 | ", input$multilevel_variables))
    } else if(length(input$multilevel_variables) == 3){
      random = list(as.formula("~ 1 | paperName/studyNameGeneral"), as.formula("~ 1 | country"))
    } else if(all(c("studyNameGeneral", "paperName") %in% input$multilevel_variables)){
      random = as.formula("~ 1 | paperName/studyNameGeneral")
    } else if(length(input$multilevel_variables) == 2){
      random = list(as.formula(paste0("~ 1 | ", input$multilevel_variables[[1]])),
                    as.formula(paste0("~ 1 | ", input$multilevel_variables[[2]])))
    } else random = NULL

    # Run meta-analysis
    ## Simple or multilevel model
    if(is.null(random)){

      result_rma <- rma.uni(yi=effectSize,
                            vi=variance,
                            data=dataSelection,
                            method = input$ma_model,
                            mods = mods,
                            slab = reference)
    } else {
      result_rma <- rma.mv(yi=effectSize,
                           V=variance,
                           data=dataSelection,
                           method = input$ma_model,
                           random = random,
                           mods = mods,
                           slab = reference)
    }

    # Format output tables for meta-analysis or moderated meta-analysis
    if(is.null(mods)){
      resultsTable <- data.frame(Effect = paste0(input$treatmentSubpropSelection1a, ": ", input$valueOptionsSelection1a,
                                                 " vs. ",
                                                 input$treatmentSubpropSelection2a, ": ", input$valueOptionsSelection2a),
                                 k = result_rma$k,
                                 d = changeRoundingOutput(result_rma$b),
                                 CI = paste0("[", changeRoundingOutput(result_rma$ci.lb),
                                             ", ",
                                             changeRoundingOutput(result_rma$ci.ub),
                                             "]"),
                                 Z = changeRoundingOutput(result_rma$zval),
                                 p = ifelse(changeRoundingOutput(result_rma$pval) == 0,
                                            "<.001",
                                            changeRoundingOutput(result_rma$pval)),
                                 PI = ifelse(input$ma_model == "FE",
                                             "",
                                             paste0("[", changeRoundingOutput(predict(result_rma)$cr.lb), ", ",
                                                    changeRoundingOutput(predict(result_rma)$cr.ub), "]")),
                                 T2 = changeRoundingOutput(result_rma$tau2),
                                 I2 = ifelse(is.null(random), changeRoundingOutput(result_rma$I2), "NA")
      )
    }
    if(!is.null(mods)){
      resultsTable <- data.frame(Effect = rownames(coef(summary(result_rma))),
                                 Estimate = changeRoundingOutput(result_rma$b),
                                 CI = paste0("[", changeRoundingOutput(result_rma$ci.lb),
                                             ", ",
                                             changeRoundingOutput(result_rma$ci.ub),
                                             "]"),
                                 Z = changeRoundingOutput(result_rma$zval),
                                 p = ifelse(changeRoundingOutput(result_rma$pval) == 0,
                                            "<.001",
                                            changeRoundingOutput(result_rma$pval)))
    }

    # Return output
    result <- list()
    result$rma <- result_rma
    result$table <- resultsTable
    result$mods <- mods

    return(result)
  }

  # Short metaAnalysis input form: Store form data from a single form
  analysisFormData <- reactive({
    analysisFormData <- sapply(metaAnalysisFields, function(x) input[[x]])
    analysisFormData <- c(analysisFormData)
    analysisFormData <- t(analysisFormData)
    analysisFormData <- data.frame(analysisFormData, stringsAsFactors=F)
    return(analysisFormData)
  })

  # Short metaAnalysis input form: Init a data frame to store multiple records
  countColumns <- length(metaAnalysisFields)
  analysisForms <- data.frame(matrix(ncol = countColumns, nrow = 0))
  colnames(analysisForms) <- names(metaAnalysisFields)

  formDataReady <- eventReactive(input$addAnalysisPaper,{
    shinyjs::show(id="addedPaperData")
    shinyjs::hide(id="analysisForm")
    analysisForms <<- rbind(analysisForms, analysisFormData())

    output$addedPaper <- DT::renderDataTable({
      datatable(analysisForms,
                colnames = c('Title', 'Authors', 'Year', 'Effect Size', 'Variance','Sample Size'),
                options = list(columnDefs = list(list(className = 'dt-right', targets = 2:5)),
                               pageLength = 10,
                               scrollX = TRUE,
                               dom = 'lftp'),
                class="stripe" ,
                escape = FALSE
      )
    })
    return(analysisForms)
  })
  observe(formDataReady())

  get_meta_analysis <- reactive({
    # Get inputs
    dataSelection <- metaAnalysisSelection()
    cat('%%%%% return original meta analysis result %%%%%%')
    # Get results
    result <- calculate_meta_analysis(dataSelection)
    result
  })

  # Record self-added paper reference for coloring the element in forest plot
  addedPaperReference <- c()

  # Update metaAnalysisSelection() with user's input
  get_updated_data_for_meta_analysis <- reactive({
    dataSelection <- metaAnalysisSelection()
    dataSelectionNames <- names(dataSelection)
    new_rows <- c()
    for (i in dataSelectionNames){
      new_rows[[i]] <- NA
    }
    formData <- formDataReady()
    new_rows$effectSize<-as.numeric(formData$effectSizeEstimate)
    new_rows$yearOfDataCollection<-as.numeric(formData$analysisPublicationYear)
    new_rows$paperTitle<-formData$analysisTitle
    new_rows$authorNames<-formData$analysisAuthor
    new_rows$effectSizeMeasure<-input$es_measure
    new_rows$variance<-as.numeric(formData$effectSizeVariance)
    new_rows$effectSizeSampleSize<-as.numeric(formData$effectSizeSampleSize)
    new_rows$reference <- paste0(new_rows$authorNames, ' (', new_rows$yearOfDataCollection, ')')
    new_rows$citation <- new_rows$reference

    addedPaperReference <<- append(addedPaperReference, new_rows$reference)
    if (dim(analysisForms)[1] != 0) {
      selfAddedPaper <- data.frame(new_rows)
      dataSelection <- rbind(dataSelection, selfAddedPaper) %>% arrange(effectSize)
    }

    metaAnalysisForestHeightCount <<- nrow(dataSelection)
    return(dataSelection)
  })

  # Get updated results with updated user input data
  get_updated_meta_analysis <- reactive({
    dataSelection <- get_updated_data_for_meta_analysis()
    updatedResult <- calculate_meta_analysis(dataSelection)
    cat('%%%%% return updated meta analysis result %%%%%%')
    return(updatedResult)
  })

  get_rma_output <- function(metaAnalysisResult) {
    analysis <- metaAnalysisResult
    mods <- analysis$mods
    result_rma <- analysis$rma
    resultsTable <- analysis$table

    effsize <- input$es_measure

    # Container for datatable
    metaoutput = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(align = "centre", colspan = 1, ''),
          th(align = "centre", colspan = 5, 'Overall effect size'),
          th(align = "centre", colspan = 1, ''),
          th(align = "centre", colspan = 2, 'Heterogeneity estimates')
        ),
        tr(
          lapply(c("Effect", "k", effsize, "95% CI", "Z", "p", "95% PI", "T²", "I²"), th)
        )
      )
    ))

    # Formatted table output, conditional on presence of moderators
    if(is.null(mods)){
      DT::datatable(resultsTable, rownames = FALSE, container = metaoutput,
                    options = list(columnDefs = list(list(className = 'dt-right', targets = 1:7)),
                                   pageLength = 10,
                                   scrollX = TRUE,
                                   dom = 'lftp'),  # dom : control (see https://datatables.net/reference/option/dom)
                    class="stripe" , #selection = 'none' ,

                    escape = FALSE )
    } else {
      DT::datatable(resultsTable,
                    options = list(columnDefs = list(list(className = 'dt-right', targets = 2:5)),
                                   pageLength = 10,
                                   scrollX = TRUE,
                                   dom = 'lftp'),  # dom : control (see https://datatables.net/reference/option/dom)
                    class="stripe" , #selection = 'none' ,

                    escape = FALSE )
    }
  }

  get_metaforSummary_output <- function (metaAnalysisResult) {
    analysis <- metaAnalysisResult
    result_rma <- analysis$rma

    ### td : update estimate (needed for power analysis)
    current_es_val(result_rma$beta)
    ###

    summ <- summary(result_rma)

    return(summ)
  }

  get_metatable_output <- function (metaTableData) {
    DT::datatable(metaTableData,
              options = list(pageLength = 10,
                             scrollX = TRUE,
                             dom = 'lftp'),  # dom : control (see https://datatables.net/reference/option/dom)
              class="stripe" , #selection = 'none' ,

              escape = FALSE )
  }

  get_forest_output <- function (metaAnalysisResult) {
    result_rma <- metaAnalysisResult$rma
    effsize <- ifelse(input$es_measure == "d", "Cohen's d", "Correlation coefficient r")
    # add match_list for highlighting the self-added studies
    match_list <- c()
    for (i in result_rma[['slab']]) {
      if (i %in% addedPaperReference || str_sub(i,1,-3) %in% addedPaperReference) {
        match_list <- append(match_list, 1)
      } else {
        match_list <- append(match_list, 2)
      }
    }
    fplot <- viz_forest(x = result_rma,
                        study_labels = result_rma$slab,
                        summary_label = "Meta-analytic estimate",
                        xlab = effsize,
                        col = c("red","#87C211")[match_list],
                        variant = "classic",
                        summary_col = c("steelblue4"),
                        annotate_CI = TRUE,
                        text_size = 4,
                        )
    #ggplotly has open issues with coloring point: https://github.com/ropensci/plotly/issues/1234
    #ggplotly(fplot, height = result_rma$k * 9 + 100, tooltip = c("x"))
    fplot
  }

  get_violin_output <- function (metaAnalysisSelectionData) {
    if (is.null(input$vis_moderator_variables) | input$vis_moderator_variables == ""){
      all_mods <- input$moderator_variables
    } else {
      all_mods <- input$vis_moderator_variables
    }

    if(length(all_mods) == 0){  # Or use a different condition
      facetFormula <- NULL
    }

    # Uses first moderator only
    if(length(all_mods > 0)){
      facetFormula <- as.formula(paste0("`", all_mods[[1]], "`", " ~ ", "."))
    }

    # NB needed for Violin plot
    data_summary <- function(x) {
      m <- mean(x)
      ymin <- m-sd(x)
      ymax <- m+sd(x)
      return(c(y=m,ymin=ymin,ymax=ymax))
    }

    d <- metaAnalysisSelectionData %>%
      separate_rows(treatmentSubproperties,sep = ",") %>%
      filter(treatmentSubproperties %in% input$treatmentSubpropSelection1a)

    violin_p <- d %>%
      mutate(reference = paste0(sub(" &.*"," et al.", authorNames),
                               "(", paperYear, ")", ", ",
                               paste(gsub("(?:[^.]+\\.){2}([^.]+).*", "\\1", observationName),
                                     gsub("(?:[^.]+\\.){3}([^.]+).*", "\\1", observationName),
                                     sep = " vs. "),
                               sep = ", ")) %>%
      # Remove duplicate measures
      group_by(reference) %>%
      sample_n(1) %>%
      ungroup() %>%
      # Plot
      ggplot(., aes(x=treatmentSubproperties, y=effectSize, fill=treatmentSubproperties, text=effectSize )) +
      geom_violin(trim=FALSE, na.rm=TRUE,scale = "count") +
      # add mean point
      stat_summary(na.rm=TRUE, fun.data=data_summary, geom="pointrange",  size=1, color="black") +
      geom_jitter(shape=4, alpha=0.5,  fill="#000000", position=position_jitter(0.05)) +
      # Theme
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # add custom color
      scale_fill_manual(values=c( "#87C211", "#FBAC0C",  "#026EA4", "#D8D8D8", "#CEE3E8"))+

      #Specify the limits of the x-axis and relabel it to something more meaningful
      scale_y_continuous(name='Effect Size') +
      #Give y-axis a meaningful label
      xlab('') +
      #Add a vertical dashed line indicating an effect size of zero, for reference
      geom_hline(yintercept=0, color='black', linetype='dashed', size=0.2)

    if(length(all_mods > 0)){
      violin_p <- violin_p + facet_grid(facetFormula, space = "free_y")
    }

    res <- ggplotly(violin_p, height = 500, tooltip=c("text") )
    res$x$data[[1]]$hoverinfo <- "none"

    return(res)
  }

  get_publication_bias_table <- function (publication_bias_result) {
    publication_bias <- publication_bias_result
    effsize = input$es_measure

    # Container for datatable
    metaoutput = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 2, ''),
          th(colspan = 4, 'Overall effect size'),
          th(colspan = 1, 'Pred. Int.'),
          th(colspan = 2, 'Heterogeneity estimates')
        ),
        tr(
          lapply(c("Effect", "k", effsize, "95% CI", "Z", "p", "PI", "T2", "I2"), th)
        )
      )
    ))

    # Formatted table output, conditional on presence of moderators
    DT::datatable(publication_bias$resultsTable,
                  rownames = FALSE,
                  container = metaoutput,
                  options = list(dom = '',
                                 columnDefs = list(list(className = 'dt-right', targets = 1:7)),
                                 "pageLength" = 40)
    )
  }

  get_funnels_output <- function (metaAnalysisSelectionData, metaAnalysisResult) {
    shiny::validate(
      need(input$treatmentSubpropSelection1a !="", "We have too little information to perform this analysis.  Please change your selection criteria on the left-hand side.")
    )

    shiny::validate(
      need(nrow(metaAnalysisSelectionData) != 0, "Your selection did not result in a valid output, please change your selection")
    )

    if(input$trimfill){
      funnel(trimfill(metaAnalysisResult$rma),
             level=c(90, 95, 99),
             xlab="Study result (effect size)",
             ylab="Study precision (std err)",
             shade=c("#F7FBFD", "#87C211", '#FBAC0C'))
    }
    else
      funnel(metaAnalysisResult$rma,
             level=c(90, 95, 99),
             xlab="Study result (effect size)",
             ylab="Study precision (std err)",
             shade=c("#F7FBFD", "#87C211", "#FBAC0C"))
  }

  get_es_val_output <- function (metaAnalysisResult) {
    shiny::validate(
      need(is.null(metaAnalysisResult$rma$formula.mods), "Please run a meta-analysis without moderators to obtain an effect size estimate.")
    )

    t <- tests
    t <- t %>% filter(t$Test == input$test_type)

    print(t)
    es <- t$ES[1]

    fluidRow(
      #Changed es to declarative structure, otherwise there was no guarantee it would update when the power analysis is opened (otherwise it would be required to run meta-analysis first)
      column(3, textInput("pwr.es", label = p("Effect size"), value = metaAnalysisResult$rma$beta )),
      column(3, textInput("pwr.power", label = p("Power"), value = 0.8)),
      column(3, textInput("pwr.siglev", label = p("Significance level"), value = 0.05)),
      column(12, code( style = "font-size:8pt", class = "text-muted",t$Par))
    )
  }

  get_power_analysis_output <- function (metaAnalysisResult) {
    shiny::validate(
      need(is.null(metaAnalysisResult$rma$formula.mods), "")
    )

    pwr <- as.numeric(input$pwr.power)
    siglev <- as.numeric(input$pwr.siglev)
    #Changed es to declarative structure, otherwise there was no guarantee it would update when the power analysis is opened (otherwise it would be required to run meta-analysis first)
    es <- as.numeric(metaAnalysisResult$rma$beta)

    if (input$test_type == "Single proportion test") {
      p.out <- pwr.p.test( h = es, power=pwr , sig.level = siglev)
    } else if (input$test_type == "R test") {
      p.out <- pwr.r.test(r = es,  power=pwr , sig.level = siglev)
    } else if (input$test_type == "One-way analysis variance") {
      p.out <- pwr.anova.test(k = 2, f = es,  power=pwr , sig.level = siglev) #todo make k selectable in browser
    } else if (input$test_type == "two sample t-test") {
      p.out <- pwr.t.test(d = es,  power=pwr , sig.level = siglev, type = "two.sample")
    } else if (input$test_type == "one sample t-test") {
      p.out <- pwr.t.test(d = es,  power=pwr , sig.level = siglev, type = "one.sample")
    } else if (input$test_type == "paired t-test") {
      p.out <- pwr.t.test(d = es,  power=pwr , sig.level = siglev, type = "paired")
    } else if (input$test_type == "unequal size t-test") {
      p.out <- pwr.t2n.test(n = 50, d = es,  power=pwr , sig.level = siglev) #todo make n and n1 selectable in browser
    } else if (input$test_type == "F2 test") {
      #currently does not work
      p.out <- pwr.f2.test(u = 100, f2 = es,  power=pwr , sig.level = siglev) #todo make u and v selectable in browser
    } else if (input$test_type == "Same 2 proportions test") {
      p.out <- pwr.2p.test(h = es,  power=pwr , sig.level = siglev)
    } else if (input$test_type == "Unequal 2 proportions test") {
      p.out <- pwr.2p2n.test(n = 50, h = es,  power=pwr , sig.level = siglev) #todo make n and n1 selectable in browser
    } else if (input$test_type == "Chi-square test") {
      p.out <- pwr.chisq.test(w = es, N = 100, power=pwr , sig.level = siglev) #todo make N and Df selectable ?)
    }
    plot(p.out)
  }

  output$rma <- DT::renderDataTable({
    get_rma_output(get_meta_analysis())
  })
  
  output$metaforSummary <- renderPrint({
    get_metaforSummary_output(get_meta_analysis())
  })
  
       ## Data table ####
  metaTableData <- reactive({
    metaAnalysisSelection() %>%
      mutate(effectSize = round(effectSize, 2),
             CI = paste0("[", round(effectSizeLowerLimit, 2), ", ", round(effectSizeUpperLimit, 2), "]")) %>%
      select(citation, paperTitle, country, effectSizeSampleSize, effectSize, variance, CI, get_all_mods()) %>%
      rename(Citation = citation,
             Title = paperTitle,
             Country = country,
             `Sample Size` = effectSizeSampleSize,
             `Effect Size` = effectSize,
             Variance = variance,
             `95% CI` = CI)
  })
  output$metatable <- DT::renderDataTable({
    get_metatable_output(metaTableData())
  })
  
  output$downloadMetaData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(metaTableData(), con, col.names = TRUE, row.names = FALSE)
    }
  )

       ## Visualise results tab (forest plot and density plot) #####

  output$get_forests <- renderPlot({

    # shiny::validate(
    #   need(nrow(metaAnalysisSelection()) != 0, "Your selection did not result in a valid output, please change your selection.")
    # )
    
    # shiny::validate(
    #   need(!is.numeric(as.data.frame(metaAnalysisSelection())[,input$vis_moderator_variables[[1]]]), 
    #        "Visualisation is not available for models with continuous moderators.")
    # )
    get_forest_output(get_meta_analysis())
  })
  output$get_forests_ui <- renderUI({
      plotOutput("get_forests", height = paste0(toString(metaAnalysisForestHeightCount*10 + 400),'px'))
  })

  # violins plot
  output$get_violin <- renderPlotly({

    # shiny::validate(
    #   need(nrow(metaAnalysisSelection()) != 0, "Your selection did not result in a valid output, please change your selection")
    # )
    # 
    # shiny::validate(
    #   need(!is.numeric(as.data.frame(metaAnalysisSelection())[,input$vis_moderator_variables[[1]]]), 
    #        "Visualisation is not available for models with continuous moderators.")
    # )
    get_violin_output(metaAnalysisSelection())
  })


       ## Publication bias #####

  run_publication_bias <- function (result_rma) {
    changeRoundingOutput <- function(numberToRound){
      result <- as.numeric(round(numberToRound,3))
    }

    # Run publication bias analyses
    trimfill <- trimfill(result_rma)
    ranktest <- ranktest(result_rma)
    regtest <- regtest(result_rma)
    hc <- hc(result_rma)

    # Publication bias test interpretation
    resultInterpretation <- paste0("The rank correlation test for funnel plot asymmetry was statistically ",
                                   ifelse(!is.null(ranktest),
                                          paste0(ifelse(ranktest$pval < .05, "significant", "non-significant"),
                                                 ", Kendall's tau = ", changeRoundingOutput(ranktest$tau),
                                                 ", <i>p</i> = ", changeRoundingOutput(ranktest$pval), ". "), ""),
                                   ifelse(!is.null(regtest),
                                          paste0("Egger's regression test for funnel plot asymmetry was statistically ",
                                                 ifelse(regtest$pval < .05, "significant", "non-significant"),
                                                 ", <i>Z</i> = ", changeRoundingOutput(regtest$zval),
                                                 ", <i>p</i> = ", changeRoundingOutput(regtest$pval), ". "), ""),
                                   "The trim-and-fill method estimated <i>k</i> = ", trimfill$k0,
                                   " missing studies on the ", trimfill$side, " side of the funnel plot.")

    # Table
    resultsTable <- data.frame(Method = c(input$ma_model, "Trim-and-Fill", "Henmi-Copas"),
                               k = c(result_rma$k, trimfill$k, result_rma$k),
                               Estimate = c(changeRoundingOutput(result_rma$b),
                                            changeRoundingOutput(trimfill$b),
                                            changeRoundingOutput(hc$beta)),
                               CI = c(paste0("[", changeRoundingOutput(result_rma$ci.lb),
                                             ", ",
                                             changeRoundingOutput(result_rma$ci.ub),
                                             "]"),
                                      paste0("[", changeRoundingOutput(trimfill$ci.lb),
                                             ", ",
                                             changeRoundingOutput(trimfill$ci.ub),
                                             "]"),
                                      paste0("[", changeRoundingOutput(hc$ci.lb),
                                             ", ",
                                             changeRoundingOutput(hc$ci.ub),
                                             "]")),
                               Z = c(changeRoundingOutput(result_rma$zval),
                                     changeRoundingOutput(trimfill$zval),
                                     ""),
                               p = c(ifelse(changeRoundingOutput(result_rma$pval) == 0,
                                            "<.001",
                                            changeRoundingOutput(result_rma$pval)),
                                     ifelse(changeRoundingOutput(trimfill$pval) == 0,
                                            "<.001",
                                            changeRoundingOutput(trimfill$pval)),
                                     ""),
                               PI = ifelse(input$ma_model == "FE",
                                           c("", "", ""),
                                           c(paste0("[", changeRoundingOutput(predict(result_rma)$cr.lb), ", ",
                                                    changeRoundingOutput(predict(result_rma)$cr.ub), "]"),
                                             paste0("[", changeRoundingOutput(predict(trimfill)$cr.lb), ", ",
                                                    changeRoundingOutput(predict(trimfill)$cr.ub), "]"),
                                             "")),
                               T2 = c(changeRoundingOutput(result_rma$tau2),
                                      changeRoundingOutput(trimfill$tau2),
                                      changeRoundingOutput(hc$tau2)),
                               I2 = c(changeRoundingOutput(result_rma$I2),
                                      changeRoundingOutput(trimfill$I2),
                                      "")
    )

    result <- list()
    result$resultInterpretation <- resultInterpretation
    result$resultsTable <- resultsTable

    return(result)
  }

  # Publication bias analyses
  get_publication_bias <- reactive({
    
    # If the meta-analysis uses moderators or a multilevel model, refit as
    # rma.uni() without moderators
    result_rma <- get_meta_analysis()$rma
    if(("rma.mv" %in% class(result_rma)) | !is.null(result_rma$formula.mods)){
      dataSelection <- metaAnalysisSelection()
      result_rma <- rma.uni(yi=effectSize,
                            vi=variance,
                            data=dataSelection,
                            method = input$ma_model,
                            slab = reference)
    }
    result <- run_publication_bias(result_rma)
    return(result)
  })

  # Publication bias analyses with user input data
  get_updated_publication_bias <- reactive({
    # If the meta-analysis uses moderators or a multilevel model, refit as
    # rma.uni() without moderators
    result_rma <- get_updated_meta_analysis()$rma
    if(("rma.mv" %in% class(result_rma)) | !is.null(result_rma$formula.mods)){
      dataSelection <- get_updated_data_for_meta_analysis()
      result_rma <- rma.uni(yi=effectSize,
                            vi=variance,
                            data=dataSelection,
                            method = input$ma_model,
                            slab = reference)
    }
    result <- run_publication_bias(result_rma)
    return(result)
  })
  
  output$publicationbias_interpretation <- renderUI({
    tagList(
      HTML(get_publication_bias()$resultInterpretation)
      )
  })
  
  output$publicationbias_table <- DT::renderDataTable({
    get_publication_bias_table(get_publication_bias())
  })
    
  output$get_funnels <- renderPlot ({
    get_funnels_output(metaAnalysisSelection(), get_meta_analysis())
  }, height = 'auto')
  
       ## Statistical power analysis tab ####
  # select test type
  observe({
    tests <- tests %>% filter(tests$Family == input$test_fam)
    updateSelectInput(session, "test_type", choices=tests$Test, selected = "two sample t-test")
  })


  # statistical power analysis parameters
  output$get_es_val <- renderUI({
    get_es_val_output(get_meta_analysis())
  })

  # statistical power analysis plot
  output$poweranalysis <- renderPlot({
    get_power_analysis_output(get_meta_analysis())
  })

  # Update data for metaAnalysis and Reload all relevant outputs.
  observeEvent(input$reMetaAnalysis, {

    # Reload rma output
    output$rma <- DT::renderDataTable({
      get_rma_output(get_updated_meta_analysis())
    })

    # Reload metaforSummary ouput
    output$metaforSummary <- renderPrint({
      get_metaforSummary_output(get_updated_meta_analysis())
    })

    #### Data table ####
    # TODO: remove duplicate code for datatable
    updatedMetaTableData <- reactive({
    get_updated_data_for_meta_analysis() %>%
      mutate(effectSize = round(effectSize, 2),
             CI = paste0("[", round(effectSizeLowerLimit, 2), ", ", round(effectSizeUpperLimit, 2), "]")) %>%
      select(citation, paperTitle, country, effectSizeSampleSize, effectSize, variance, CI, get_all_mods()) %>%
      rename(Citation = citation,
             Title = paperTitle,
             Country = country,
             `Sample Size` = effectSizeSampleSize,
             `Effect Size` = effectSize,
             Variance = variance,
             `95% CI` = CI)
  })

    # Reload metatable output
    output$metatable <- DT::renderDataTable({
      get_metatable_output(updatedMetaTableData())
    })

    # Reload downloader
    output$downloadMetaData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(metaTableData(), con, col.names = TRUE, row.names = FALSE)
      }
    )

    # Reload forests graph
    output$get_forests <- renderPlot({
      get_forest_output(get_updated_meta_analysis())
    })
    output$get_forests_ui <- renderUI({
      plotOutput("get_forests", height = paste0(toString(metaAnalysisForestHeightCount*10 + 400),'px'))
    })

    # Reload volin graph
    output$get_violin <- renderPlotly({
      get_violin_output(get_updated_data_for_meta_analysis())
    })

    # Reload publicationbias_interpretation
    output$publicationbias_interpretation <- renderUI({
      tagList(
        HTML(get_updated_publication_bias()$resultInterpretation)
        )
    })

    # Reload publication bias table
    output$publicationbias_table <- DT::renderDataTable({
      get_publication_bias_table(get_updated_publication_bias())
    })

    # Reload funnels output
    output$get_funnels <- renderPlot ({
      get_funnels_output(metaAnalysisSelection(), get_meta_analysis())
    }, height = 'auto')

    # Reload statistical power analysis parameters
    output$get_es_val <- renderUI({
      get_es_val_output(get_updated_meta_analysis())
    })

    # Reload statistical power analysis parameters
    output$get_es_val <- renderUI({
      get_es_val_output(get_updated_meta_analysis())
    })

    # Reload statistical power analysis plot
    output$poweranalysis <- renderPlot({
      get_power_analysis_output(get_updated_meta_analysis())
    })

    # TODO：Remove unnecessary reload

  })


  ####### Meta-regression tab #####

  # Short meta-regression input form: Store form data from a single form
  regressionFormData <- reactive({
    formData <- sapply(metaRegressionFields, function(x) input[[x]])
    formData <- c(formData)
    formData <- t(formData)
    formData <- data.frame(formData, stringsAsFactors=F)
    return(formData)
  })
  # Short meta-regression input form: Init a data frame to store multiple records
  countColumnsRegression <- length(metaRegressionFields)
  regressionForms <- data.frame(matrix(ncol = countColumnsRegression, nrow = 0))
  colnames(regressionForms) <- names(metaRegressionFields)

  formDataRegression <- eventReactive(input$addRegressionPaper,{
    shinyjs::show(id="addedPaperDataRegression")
    shinyjs::hide(id="regressionForm")
    regressionForms <<- rbind(regressionForms, regressionFormData())

    output$addedPaperRegression <- DT::renderDataTable({
      datatable(regressionForms,
                colnames = c('Title', 'Authors', 'Year', 'Logit-transformed cooperation', 'Variance', 'Sample size'),
                options = list(columnDefs = list(list(className = 'dt-right')),
                               pageLength = 10,
                               scrollX = TRUE,
                               dom = 'lftp'),
                class="stripe" ,
                escape = FALSE
      )
    })
    return(regressionForms)
  })
  observe(formDataRegression())

  # Record self-added paper reference for coloring the element in forest plot
  addedPaperReferenceReg <- c()

  # Update metaAnalysisSelection() with user's input
  get_updated_data_for_meta_regression <- reactive({
    dataSelection <- metaRegressionSelection()
    dataSelectionNames <- names(dataSelection)
    new_rows <- c()
    for (i in dataSelectionNames){
      new_rows[[i]] <- NA
    }
    formData <- formDataRegression()

    new_rows$effectSize<-as.numeric(formData$regressionEffectSize)
    new_rows$yearOfDataCollection<-as.numeric(formData$regressionYear)
    new_rows$variance<-as.numeric(formData$regressionVariance)
    new_rows$effectSizeSampleSize<-as.numeric(formData$regressionSampleSize)

    new_rows$reference <- paste0(formData$regressionAuthor, ' (', formData$regressionYear, ')')
    new_rows$logPropContributed <- as.numeric(formData$regressionEffectSize)
    new_rows$coefficientOfVariation<-as.numeric(formData$regressionVariance)
    new_rows$sampleSize <- as.numeric(formData$regressionSampleSize)
    addedPaperReferenceReg <<- append(addedPaperReferenceReg, new_rows$reference)

    new_rows$paperTitle <- formData$regressionTitle
    new_rows$citation <- new_rows$reference

    if (dim(regressionForms)[1] != 0) {
      selfAddedPaper <- data.frame(new_rows)
      dataSelection <- rbind(dataSelection, selfAddedPaper) %>%
        arrange(effectSize)  %>%
        arrange(logPropContributed)
    }

    # Count rows for later forest plot height computing
    metaRegressionForestHeightCount <<- nrow(dataSelection)

    return(dataSelection)
  })

       ## Meta-regression interpretation ####
  get_interpretation_reg <- reactive({
    
    # Function to generate strings describing treatments from inputs
    # Each string has the form treatmentSubprop1: valueOption1 AND/OR ...
    treatmentString <- function(treatment){
      treatmentSubprops <- list()
      valueOptions <- list()
      numSubprop <- ifelse(is.null(input[[paste0("extraCriteria", treatment)]]), 1, 
                           as.numeric(input[[paste0("extraCriteria", treatment)]]) + 1)
      for (i in 1:numSubprop){
        treatmentSubprops[[i]] <- input[[paste0("treatmentSubpropSelection", treatment, letters[[i]])]]
        valueOptions[[i]] <- input[[paste0("valueOptionsSelection", treatment, letters[[i]])]]
      }  
      treatmentSubprops <- unlist(treatmentSubprops)
      valueOptions <- unlist(valueOptions)
      
      output <- paste(treatmentSubprops, valueOptions, sep = ": ", collapse = paste0(" ", input[[paste0("combinator", treatment)]], " "))
      return(output)
    }
    
    metaRegInterpretation <- case_when(
      # If there is at least one treatment 1
      !(input$treatmentSubpropSelection1a == "") ~ paste0(
        "Meta-regression of selected treatments (", treatmentString(1), "). The estimate indicates the rate of cooperation in these treatments."),
      # All else
      TRUE ~ "Unfortunately, no interpretation is available for this analysis."
    )
    
    return(metaRegInterpretation)
  })
 
  get_moderator_baseline_reg <- reactive({
    
    # Select factors, converting chr to fct
    data <- metaRegressionSelection() %>%
      select(get_all_mods_reg()) %>%
      mutate_if(is_character, as_factor) %>%
      select_if(is.factor) 
    
    if(ncol(data) > 0){
      
      baselines <- list()
      
      # Find reference levels
      referenceLevels <- lapply(data, function(x) sort(levels(x))[1])
      
      for(i in 1:ncol(data)){
        
        baselines[[i]] <- paste0("Reference level for ", names(data)[i], ": ", referenceLevels[[i]], "\n")
      }
      
      output <- do.call(paste, c(baselines, sep = "<br/>"))
    } else {
      output <- ""
    }
  
  return(output)
})
 
  output$interpretation_reg <- renderUI({
    tagList(
      p("The following interpretation is derived from the selections that you have made:"),
      p(paste0(get_interpretation_reg())),
      p(
        HTML(get_moderator_baseline_reg())
      ),
      br()
    )
  })
  
  output$moderator_description_reg <- renderUI({
    mods <- get_all_mods_reg()
    modsno <- length(mods)
    
    
    if(!is.null(modsno) & modsno > 0){
      lapply(1:modsno, function(val) {
        column(12,
               
               uiOutput(paste0("line_reg_", val)))
      })
    }
    
  })
  
  observe({
    mods <- get_all_mods_reg()
    modsno <- length(mods)
    
    lapply(1:modsno, function(val) {
      output[[paste0("line_reg_", val)]] <- renderUI(p(strong(mods[val]), 
                                                   em(selections[which(selections$treatmentSubproperty == mods[val] ) , ]$treatmentDesc), 
                                                   em(studyMods[which(studyMods$moderator == mods[val]),]$desc),
                                                   em(external_country_moderators[which(external_country_moderators$pLabel == mods[val] ) , ]$desc) ) )
    }) 
    
  })
  
       ## Meta-regression models ####
  call_meta_regression <- function(regressionData){
    dataSelection <- regressionData
    changeRoundingOutput <- function(numberToRound){
      result <- as.numeric(round(numberToRound,3))
    }

        # One vector containing all selected or created moderators
    all_mods <- get_all_mods_reg()

    # Moderator formula
    if(length(all_mods) == 0){  # Or use a different condition
      mods <- NULL
    }
    if(length(all_mods) > 0){
      mods <- as.formula(paste("~", paste0("`",all_mods,"`", collapse = " + ")))
    }

    # Multilevel formula
    # Note that this is hardcoded to only allow study, paper and country,
    # with studies nested within papers and otherwise crossed factors.
    if(length(input$multilevel_variables_reg) == 0){
      random = NULL
    } else if(length(input$multilevel_variables_reg) == 1){
      random = as.formula(paste0("~ 1 | ", input$multilevel_variables_reg))
    } else if(length(input$multilevel_variables_reg) == 3){
      random = list(as.formula("~ 1 | paperName/studyNameGeneral"), as.formula("~ 1 | country"))
    } else if(all(c("studyNameGeneral", "paperName") %in% input$multilevel_variables_reg)){
      random = as.formula("~ 1 | paperName/studyNameGeneral")
    } else if(length(input$multilevel_variables_reg) == 2){
      random = list(as.formula(paste0("~ 1 | ", input$multilevel_variables_reg[[1]])),
                    as.formula(paste0("~ 1 | ", input$multilevel_variables_reg[[2]])))
    } else random = NULL

    # Meta-regression
    if(is.null(random)){
      result_rma <- rma.uni(yi=logPropContributed,
                            vi=coefficientOfVariation,
                            data=dataSelection,
                            method = input$ma_model_reg,
                            mods = mods,
                            slab = reference)
    }
    if(!is.null(random)){
      result_rma <- rma.mv(yi=logPropContributed,
                           V=coefficientOfVariation,
                           data=dataSelection,
                           method = input$ma_model_reg,
                           random = random,
                           mods = mods,
                           slab = reference)
    }

    if(is.null(mods)){
      resultsTable <- data.frame(Effect = paste0(input$treatmentSubpropSelection1a, ": ", input$valueOptionsSelection1a),
                                 k = result_rma$k,
                                 Est = changeRoundingOutput(transf.ilogit(result_rma$b)),
                                 CI = paste0("[", changeRoundingOutput(transf.ilogit(result_rma$ci.lb)),
                                             ", ",
                                             changeRoundingOutput(transf.ilogit(result_rma$ci.ub)),
                                             "]"),
                                 Z = changeRoundingOutput(result_rma$zval),
                                 p = ifelse(changeRoundingOutput(result_rma$pval) == 0,
                                            "<.001",
                                            changeRoundingOutput(result_rma$pval)),
                                 PI = ifelse(input$ma_model == "FE",
                                             "",
                                             paste0("[", changeRoundingOutput(transf.ilogit(predict(result_rma)$cr.lb)), ", ",
                                             changeRoundingOutput(transf.ilogit(predict(result_rma)$cr.ub)), "]")),
                                 T2 = changeRoundingOutput(result_rma$tau2),
                                 I2 = ifelse(is.null(random), changeRoundingOutput(result_rma$I2), "")
      )
    }
    if(!is.null(mods)){
      resultsTable <- data.frame(Effect = rownames(coef(summary(result_rma))),
                                 Est = changeRoundingOutput(result_rma$b),
                                 CI = paste0("[", changeRoundingOutput(result_rma$ci.lb),
                                             ", ",
                                             changeRoundingOutput(result_rma$ci.ub),
                                             "]"),
                                 Z = changeRoundingOutput(result_rma$zval),
                                 p = ifelse(changeRoundingOutput(result_rma$pval) == 0,
                                            "<.001",
                                            changeRoundingOutput(result_rma$pval)))
    }
    result <- list()
    result$rma <- result_rma
    result$table <- resultsTable
    result$mods <- mods

    return(result)

  }

  get_meta_regression <- reactive({
    call_meta_regression(metaRegressionSelection())
  })

  get_reg_output <- function (regResult) {
    analysis <- regResult
    mods <- analysis$mods
    result_rma <- analysis$rma
    resultsTable <- analysis$table

    # Container for datatable

    metaoutput = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 1, ''),
          th(colspan = 5, 'Overall effect size'),
          th(colspan = 1, 'Pred. Int.'),
          th(colspan = 2, 'Heterogeneity estimates')
        ),
        tr(
          lapply(c("Effect", "k", "b", "95% CI", "Z", "p", "95% PI", "T²", "I²"), th)
        )
      )
    ))

    if(is.null(mods)){
      DT::datatable(resultsTable, rownames = FALSE, container = metaoutput,
                    options = list(columnDefs = list(list(className = 'dt-right', targets = 1:7)),
                                   pageLength = 10,
                                   scrollX = TRUE,
                                   dom = 'lftp'),  # dom : control (see https://datatables.net/reference/option/dom)
                    class="stripe" , #selection = 'none' ,

                    escape = FALSE )
    } else {
      DT::datatable(resultsTable,
                    options = list(columnDefs = list(list(className = 'dt-right', targets = 2:5)),
                                   pageLength = 10,
                                   scrollX = TRUE,
                                   dom = 'lftp'),  # dom : control (see https://datatables.net/reference/option/dom)
                    class="stripe" , #selection = 'none' ,

                    escape = FALSE )
    }
  }

  get_reg_summary <- function(regResult) {
    analysis <- regResult
    result_rma <- analysis$rma
    summ <- summary(result_rma)
    return(summ)
  }

  get_forests_reg_output <- function (regSelectionData, regResult) {
    shiny::validate(
      need(nrow(regSelectionData) != 0, "Your selection did not result in a valid output, please change your selection")
    )

    result_rma <- regResult$rma

    match_list <- c()
    for (i in result_rma[['slab']]) {
      if (i %in% addedPaperReferenceReg || str_sub(i,1,-3) %in% addedPaperReferenceReg) {
        match_list <- append(match_list, 1)
      } else {
        match_list <- append(match_list, 2)
      }
    }

    fplot <- viz_forest(x = result_rma,
                        col = c("red","#87C211")[match_list],
                        study_labels = result_rma$slab,
                        summary_label = "Meta-analytic estimate",
                        xlab = "Logit-transformed cooperation",
                        variant = "classic",
                        summary_col = c("steelblue4"),
                        annotate_CI = TRUE,
                        text_size = 4,
    )

    #ggplotly(fplot, height = result_rma$k * 9 + 100, tooltip = c("x"))
    fplot
  }

  get_violin_reg_output <- function(regSelectionData) {
    shiny::validate(
      need(nrow(regSelectionData) != 0, "Your selection did not result in a valid output, please change your selection")
    )

    if (is.null(input$vis_moderator_variables_reg) | input$vis_moderator_variables_reg == ""){
      all_mods <- input$moderator_variables_reg
    } else {
      all_mods <- input$vis_moderator_variables_reg
    }

    if(length(all_mods) == 0){  # Or use a different condition
      facetFormula <- NULL
    }

    # Uses first moderator only
    if(length(all_mods > 0)){
      facetFormula <- as.formula(paste0("`", all_mods[[1]], "`", " ~ ", "."))
    }

    # NB needed for Violin plot
    data_summary <- function(x) {
      m <- mean(x)
      ymin <- m-sd(x)
      ymax <- m+sd(x)
      return(c(y=m,ymin=ymin,ymax=ymax))
    }

    d <- regSelectionData %>%
      mutate(xaxis = "")

    violin_p <- d %>%
      ggplot(., aes(x = xaxis, y = logPropContributed, fill = xaxis, text = logPropContributed)) +
      geom_violin(trim=FALSE, na.rm=TRUE, scale = "count") +
      # add mean point
      stat_summary(na.rm=TRUE, fun.data=data_summary, geom="pointrange",  size=1, color="black") +
      geom_jitter(shape=4, alpha=0.5,  fill="#000000", position=position_jitter(0.05)) +
      # add custom color
      scale_fill_manual(values=c( "#87C211", "#FBAC0C",  "#026EA4", "#D8D8D8", "#CEE3E8")) +
      # Theme
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      # #Specify the limits of the x-axis and relabel it to something more meaningful
      scale_y_continuous(name='Logit-transformed Cooperation') +
      # #Give y-axis a meaningful label
      xlab('') +
      # #Add a vertical dashed line indicating an effect size of zero, for reference
      geom_hline(yintercept=0, color='black', linetype='dashed', size=0.2)

    if(length(all_mods > 0)){
      violin_p <- violin_p + facet_grid(facetFormula, space = "free_y")
    }

    res <- ggplotly(violin_p, height = 500, tooltip=c("text") )
    res$x$data[[1]]$hoverinfo <- "none"

    return(res)

  }

  get_metatable_reg_output <- function(metaTableDataReg) {
    DT::datatable(metaTableDataReg,
              options = list(pageLength = 10,
                             scrollX = TRUE,
                             dom = 'lftp'),  # dom : control (see https://datatables.net/reference/option/dom)
              class="stripe" , #selection = 'none' ,
              escape = FALSE )
  }
  
  output$reg <- DT::renderDataTable({
    get_reg_output(get_meta_regression())
  })
  
  output$regSummary <- renderPrint({
    get_reg_summary(get_meta_regression())
  })
  
       ## Visualise results tab (forest plot and density plot) #####
  
  output$get_forests_reg <- renderPlot({
    metaRegData <- metaRegressionSelection()
    get_forests_reg_output(
      metaRegData,
      call_meta_regression(metaRegData)
    )
  })

  output$forests_reg_ui <- renderUI({
    plotOutput("get_forests_reg", height = paste0(toString(metaRegressionForestHeightCount*10 + 400),'px'))
  })
  
  # violins plot
  output$get_violin_reg <- renderPlotly({
    
    # validate(
    #   need(input$treatmentSubpropSelection1a !="", "We have too little information to perform this analysis.  ")
    # )
    get_violin_reg_output(metaRegressionSelection())
  })
  
  
       ## Data table ####
  
  metaTableData_reg <- reactive({
    metaRegressionSelection() %>%
      mutate(yi = round(logPropContributed, 2),
             vi = round(coefficientOfVariation, 2)) %>%
      select(citation, paperTitle, country, sampleSize, yi, vi, get_all_mods_reg()) %>%
      rename(Citation = citation,
             Title = paperTitle,
             Country = country, 
             `Sample Size` = sampleSize, 
             `Logit-transformed Cooperation` = yi,
             `Variance` = vi)
  })
  output$metatable_reg <- DT::renderDataTable({
    get_metatable_reg_output(metaTableData_reg())
  })
  
  
  output$downloadMetaData_reg <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(metaTableData_reg(), con, col.names = TRUE, row.names = FALSE)
    }
  )
  output$download_compute_logit <- downloadHandler(
    filename="compute_logit_and_variance.xlsx",
    content=function(con) {
      file.copy("www/data/Computing_logit_and_variance.xlsx", con)
    }
  )

  #TODO: LOWER SHOULD LESS THAN UPPER
  observeEvent(input$reMetaRegression, {
    # Reload output
    output$reg <- DT::renderDataTable({
      get_reg_output(call_meta_regression(get_updated_data_for_meta_regression()))
    })
    output$regSummary <- renderPrint({
      get_reg_summary(call_meta_regression(get_updated_data_for_meta_regression()))
    })
    output$get_forests_reg <- renderPlot({
      get_forests_reg_output(
        get_updated_data_for_meta_regression(),
        call_meta_regression(get_updated_data_for_meta_regression())
      )
    })
    output$forests_reg_ui <- renderUI({
      plotOutput("get_forests_reg", height = paste0(toString(metaRegressionForestHeightCount*10 + 400),'px'))
    })

    output$get_violin_reg <- renderPlotly({
      get_violin_reg_output(get_updated_data_for_meta_regression())
    })
    updatedMetaTableData_reg <- reactive({
      get_updated_data_for_meta_regression() %>%
        mutate(yi = round(logPropContributed, 2),
               vi = round(coefficientOfVariation, 2)) %>%
        select(citation, paperTitle, country, sampleSize, yi, vi, get_all_mods_reg()) %>%
        rename(Citation = citation,
               Title = paperTitle,
               Country = country,
               `Sample Size` = sampleSize,
               `Logit-transformed Cooperation` = yi,
               `Variance` = vi)
    })
    output$metatable_reg <- DT::renderDataTable({
      get_metatable_reg_output(updatedMetaTableData_reg())
    })
    output$downloadMetaData_reg <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(get_updated_data_for_meta_regression(), con, col.names = TRUE, row.names = FALSE)
      }
    )

  })
  
  ####### Taxonomy tab #####
  output$selected <- renderText({
    bar_number <- as.numeric(req(input$bar_clicked))
    if (bar_number > 0) cos(bar_number)
  })
  
  output$d3 <- renderD3({
    
    json_data <- jsonlite::read_json("www/data/new_ontology.json")
    r2d3(data = json_data, d3_version = 4, script = "www/js/circlepacking.js", css = "www/css/circlepacking.css")
    
    #flare =  read.csv("www/data/ivdata.csv")
    #r2d3(data =flare, d3_version = 4, script = "www/js/bubbles.js")
    
  })
  

  output$infotable <- DT::renderDataTable({
    DT::datatable(result <- selectionsValues %>%
                    group_by(treatmentSubproperty, treatmentSubpropertyDescription, ivname) %>%
                    summarise(PossibleValues = paste(valueName, collapse = ', ')) %>%
                    rename(`Generic Independent Variable` = ivname, `Independent Variable` = treatmentSubproperty, `Description` = treatmentSubpropertyDescription, Values=PossibleValues) %>%
                    select(`Generic Independent Variable`, `Independent Variable`, `Description`,  Values)
    )
  })

  ####### Analyse Citations #####
  output$citations_viz <- renderVisNetwork({
      
    #### data wrangling first
     n <- filteredCitations()  %>%  #citation_nodes 
      rename(
        id = Id, 
        label = Label,
        group = input$citation_color_selector,
        value = indegree
      )
     
    # fill empty labels
    n$label = ifelse(n$label!='', paste0(n$label," (",n$year,")"), paste0("Unknown reference (",n$year,")"))
    
    #limit to nodes with *at least* 5 citations. Anything below does not add much, and affects performance
    #n <- n[n$value >=2,]
    n$title <- paste0("<p><b>",n$label,"</b><br><a href='http://doi.org/",n$id,"'>http://doi.org/",n$id,"</a><br><small><a href='",n$paper,"'>",n$paper,"</a></small><br><small><i>IV</i>: ", n$iv ,"<br><i>Game</i>: ", n$game ,"<br><i>Country</i>: ", n$country ,"</small></p>")
    # sort
    n <- n[order(n$label),]
    
    #edges
    e <- citation_edges

    # filter out edges targeting missing nodes (or visNet will throw an error)
    e <- e[e$from %in% n$id ,]
    e <- e[e$to %in% n$id ,]

    ### end of data wrangling
    visNetwork(background= "#EFF8FB",nodes=n, edges=e,  width = "50%") %>%
      visNodes( scaling=list("min"=5, "max" = 80) ) %>%
      visOptions( selectedBy= list(variable=as.character(input$citation_color_selector) ) ,
                  highlightNearest = list(enabled=T, algorithm="hierarchical"),
                  nodesIdSelection = list(useLabels= T ,enabled=T,
                                          style = 'width: 300px; height: 34px; background: #f8f8f8; margin: 15px; padding: 6px 12px;  background-color: #ffffff; border: 1px solid #cccccc;'),
      )  %>%
      #visPhysics(solver = "forceAtlas2Based", forceAtlas2Based=list(gravitationalConstant = -100000), stabilization = T) %>%
      visIgraphLayout(layout = "layout_with_graphopt",randomSeed = 123) %>%
      visInteraction(navigationButtons = T, hideEdgesOnDrag = T) %>%
      visLegend(width = 0.2, position = "right", main = list(text=input$color_selector,style = "font-size:20px")) %>%
      visEdges(hidden=input$remove_edges, arrows = list(from = list(enabled = TRUE, scaleFactor = 0.2)), smooth = FALSE)
  })
  
  filteredCitations <- reactive({
    

    if (!is.null(input$citation_year[1]) && input$citation_year[1] != "" &&
        !is.null(input$citation_year[2]) && input$citation_year[2] != "" &&
        !is.null(input$citation_number[1]) && input$citation_number[1] != "" &&
        !is.null(input$citation_number[2]) && input$citation_number[2] != ""
       ) {
       nodes <-  citation_nodes %>%
        filter(# 
          ( year >= input$citation_year[1]) &
            ( year <= input$citation_year[2]) &
              (indegree >= input$citation_number[1]) &
                (indegree <= input$citation_number[2])
        )
    }
     print(nrow(nodes))
    
    nodes
    
 
  })
  
 
  observe({
    updateSelectInput(session, "citation_color_selector", choices =  c("Community" = "community", "Type of Social Dilemma"="game", "Country of Study"="country", "Variable of Observartion"="iv"))
  })

  observeEvent(input$citations_info_message, {
    showModal(modalDialog(
      title = "How to use the citation network",
      p("An edge has to be interpreted as a 'cites' relationship, i.e. 'paper A cites paper B'.",  class="text-muted"),
      p("Hover on the nodes to see the details of each paper.",  class="text-muted"),
      p("Click on a DOI to redirect to the original paper.",  class="text-muted"),
      p("You can zoom in and out both on the legend and the network, just by pointing your mouse in the area of interest.",  class="text-muted"),
      p("When selecting a node, its 1-hop neighbors will also be highlighted.",  class="text-muted"),
      p("You can color the network according to research community, type of social dilemma, country/region of the study, and variable of the observations.",  class="text-muted"),
      p("Communities are calculated using the ", a(href="https://en.wikipedia.org/wiki/Modularity_%28networks%29","Modularity Algorithm"), ". They are not labelled, only numbered.", class="text-muted")
    ))
  })

  ######### Long InputForm ##########
  # Input form for submitting papers

  # Disable the submit button if not all the mandatory fields are filled
  whether_all_mandatory_filled <- function (fields) {
    mandatoryFilled <-
      vapply(fields,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    if (is.na(mandatoryFilled)) {
      mandatoryFilled <- FALSE
    }
    return(mandatoryFilled)
  }
  observe({
    # must agree privacy terms and conditions
    agree_privacy <- input$privacy
    mandatoryFilled <- whether_all_mandatory_filled(fieldsMandatory)
    enableSubmit <- all(mandatoryFilled, agree_privacy)
    shinyjs::toggleState(id = "submit", condition = enableSubmit)
  })
  observe({
    mandatoryFilled <- whether_all_mandatory_filled(metaAnalysisFields)
    shinyjs::toggleState(id = "addAnalysisPaper", condition = mandatoryFilled)
  })
  observe({
    mandatoryFilled <- whether_all_mandatory_filled(metaRegressionFields)
    shinyjs::toggleState(id = "addRegressionPaper", condition = mandatoryFilled)
  })

  ## Add time at file name to indicate the submission time
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
  # Define the data to be saved
  longFormData <- reactive({
    data <- sapply(fieldsAll, function(x) toString(input[[x]]))

    numVar <- as.integer(input$numberOfVariables)
    variables <- c()
    for (k in 1:numVar) {
      numTreatments <- as.integer(input[[paste0("treatmentNum", k)]])
      treatments <- c()
      for (i in 1:numTreatments) {
        treatment <- c()
        numGeneric <- as.integer(input[[paste0("GIVNum", k, '_', i)]])

        treatment[[paste0('quantitativeMethod', k, '_', i)]] <- input[[paste0('quantitativeMethod', k, '_', i)]]
        treatment[[paste0('addCorrelation', k, '_', i)]] <- input[[paste0('addCorrelation', k, '_', i)]]
        treatment[[paste0('addSampleSizeA', k, '_', i)]] <- input[[paste0('addSampleSizeA', k, '_', i)]]
        treatment[[paste0('DVBehavior', k, '_', i)]] <- input[[paste0('DVBehavior', k, '_', i)]]
        treatment[[paste0('addMean', k, '_', i)]] <- input[[paste0('addMean', k, '_', i)]]
        treatment[[paste0('addStandardDeviation', k, '_', i)]] <- input[[paste0('addStandardDeviation', k, '_', i)]]
        treatment[[paste0('addSampleSizeB', k, '_', i)]] <- input[[paste0('addSampleSizeB', k, '_', i)]]
        treatment[[paste0('addProportionOfCooperation', k, '_', i)]] <- input[[paste0('addProportionOfCooperation', k, '_', i)]]
        treatment[[paste0('addLowestChoice', k, '_', i)]] <- input[[paste0('addLowestChoice', k, '_', i)]]
        treatment[[paste0('addHighestChoice', k, '_', i)]] <- input[[paste0('addHighestChoice', k, '_', i)]]
        treatment[[paste0('betweenOrWithinSubjects', k, '_', i)]] <- input[[paste0('betweenOrWithinSubjects', k, '_', i)]]
        generic_ivs <- c()
        for (m in 1:numGeneric) {
          generic_iv <- c()
          numSpecific <- as.integer(input[[paste0("SIVNumber",k, '_', i, '_', m)]])
          treatment[[paste0("addGenIV", k, '_', i, '_', m)]] <- input[[paste0("addGenIV", k, '_', i, '_', m)]]
          treatment[[paste0("addDescriptionGenericIV", k, '_', i, '_', m)]] <- input[[paste0("addDescriptionGenericIV", k, '_', i, '_', m)]]
          treatment[['specificVariables']] <- c()
          for (n in 1:numSpecific) {

            treatment[['specificVariables']][[paste0("SubpropSelection", k, '_', i, '_', m, '_', n)]] <- input[[paste0("SubpropSelection", k, '_', i, '_', m, '_', n)]]
            treatment[['DescriptionSpecificIV']][[paste0("DescriptionSpecificIV", k, '_', i, '_', m, '_', n)]] <- input[[paste0("DescriptionSpecificIV", k, '_', i, '_', m, '_', n)]]
            treatment[['ValueOptionsSelection']][[paste0("ValueOptionsSelection", k, '_', i, '_', m, '_', n)]] <- input[[paste0("ValueOptionsSelection", k, '_', i, '_', m, '_', n)]]

          }

        }
        treatment_with_name <- c()
        treatment_with_name[[paste0('treatment_', i)]] <- treatment
        treatments <- c(treatments, treatment_with_name)
      }

      variables[[paste0('variable', k)]] <- treatments

    }


    treatmentsJSON <- toJSON(variables)
    data <- c(data, treatmentsValue = treatmentsJSON)

    data <- c(data, timestamp = humanTime())
    # note: data.frame(t(data) won't be accepted by google api, use the following line as a workaround. so weird...
    data <- data.frame(t(data.frame(t(data.frame(t(data))))))
    # fill NULL with NA to avoid misorder when add to sheet
    data[data == 'NA'] <- ""
    data
  })

  # Save submission in google sheets
  saveData <- function(longFormData) {
    SHEET_ID <- "1DxFQYx1SfzREuX4j9eZYWONRZkQDqmSzagwgb68sSSA"
    # we are using googlesheet4 to access the google sheet, and for doing this, a google service account is needed.
    # If you need to create a new google service account, follow this instruction:
    # https://gspread.readthedocs.io/en/latest/oauth2.html#enable-api-access
    gs4_auth(path = "unified-welder-284622-df7c91d6ba7f.json")
    sheet_append(SHEET_ID, longFormData, sheet="LongInputForm")
  }

  # action to take when submit button is pressed
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")

    tryCatch({
      saveData(longFormData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
      shinyjs::hide("removeVariable")
      shinyjs::hide("removeTreatment")

      # remove UI for treatment variables
      #for (i in c(addedTreatments, addedVariables)) {
      # removeUI(
      #    selector = paste0('#', i)
      #  )
      #}
      #
      #addedTreatments <<- c('Treatment1')
      #addedVariables <<- c()

    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })

  # Allow user to submit another paper after a successful submission
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })

  #### Meta-analysis Input Form ####
  observeEvent(input$expand_analysisForm, {
    show(id = "analysisForm")
    shinyjs::show(id="analysisForm")
    reset('analysisForm')
    output$analysisTreatment1 <- renderUI({
      em(class = "text-muted",
         generateCategoryString(input$genIVselection1a), br(),
         generateSubpropString(input$treatmentSubpropSelection1a))
    })

    output$analysisTreatment2 <- renderUI({
      if (input$genIVselection2a==""){
       em(class = 'text-muted', "N/A")
      } else {
      em(class = "text-muted",
         generateCategoryString(input$genIVselection2a), br(),
         generateSubpropString(input$treatmentSubpropSelection2a))
      }
    })

    output$analysisModerators <- renderUI({
      moderator_text1 <- ''
      moderator_text2 <- ''

      if (is.null(input$moderator_variables) & is.null(input$moderator_characteristics)){
        return(em(class = "text-muted", "No moderator provided"))
      }
      if (!is.null(input$moderator_variables)){
        if (length(input$moderator_variables)>1){
          text <- toString(input$moderator_variables)
        } else {
          text <- input$moderator_variables
        }
        moderator_text1 <- paste("Variable Specific Moderators: ",
                                text)
      }
      if (!is.null(input$moderator_characteristics)) {
        if (length(input$moderator_characteristics)>1){
          text <- toString(input$moderator_characteristics)
        } else {
          text <- input$moderator_characteristics
        }
        moderator_text2 <- paste("Study Specific Moderators: ",
                                text)
      }
      em(class = "text-muted", moderator_text1, br(), moderator_text2)
    })

    output$analysisEffectMeasure <- renderUI({
      # es_measure
      if (is.null(input$es_measure)){
        return(em(class = "text-muted", "No Measure provided"))
      } else {
        em(class = "text-muted", "Effect measure: ", input$es_measure)
      }
    })

  })

  # observe click on action link - add another paper
  observeEvent(input$addAnotherMetaAnalysisPaper, {
    shinyjs::show(id='analysisForm')
    reset("analysisForm")
  })

  #### Meta-regression input form ####
  observeEvent(input$expand_regressionForm, {
    shinyjs::show(id="regressionForm")
    reset('regressionForm')
    output$regressionTreatment1 <- renderUI({
      em(class = "text-muted",
         generateCategoryString(input$genIVselection1a), br(),
         generateSubpropString(input$treatmentSubpropSelection1a))
    })

    output$regressionModerators <- renderUI({
      moderator_text1 <- ''
      moderator_text2 <- ''

      if (is.null(input$moderator_variables) & is.null(input$moderator_characteristics)){
        return(em(class = "text-muted", "No moderator provided"))
      }
      if (!is.null(input$moderator_variables)){
        if (length(input$moderator_variables)>1){
          text <- toString(input$moderator_variables)
        } else {
          text <- input$moderator_variables
        }
        moderator_text1 <- paste("Variable Specific Moderators: ",
                                text)
      }
      if (!is.null(input$moderator_characteristics)) {
        if (length(input$moderator_characteristics)>1){
          text <- toString(input$moderator_characteristics)
        } else {
          text <- input$moderator_characteristics
        }
        moderator_text2 <- paste("Study Specific Moderators: ",
                                text)
      }
      em(class = "text-muted", moderator_text1, br(), moderator_text2)
    })

  })
  # observe click on action link - add another paper
  observeEvent(input$addAnotherMetaRegressionPaper, {
    shinyjs::show(id='regressionForm')
    reset("regressionForm")
  })


  observeEvent(input$showTerms, {
      showModal(modalDialog(
        title = "Privacy statement: Cooperation Databank",
        p("Date: [1/26/2021]", br(),
          "The Vrije Universiteit Amsterdam (hereinafter: VU) attaches great importance to the protection of your privacy and the security of your personal data. In this privacy statement we describe how we handle your personal data within the Cooperation Databank (CoDa). We process your personal data in accordance with applicable privacy legislation, including the General Data Protection Act (GDPR) and the General Data Protection Implementation Act.",
          h4('1. Who is responsible for the processing of my personal data?'),
          "Stichting VU is responsible for the data processing operations described in this privacy statement. Stichting VU maintain the Vrije Universiteit Amsterdam as a privately run university in accordance with the Higher Education and Research Act of The Netherlands (‘Wet op het hoger onderwijs en wetenschappelijk onderzoek’). Stichting VU has its registered office at De Boelelaan 1105 in (1081 HV) Amsterdam and is registered with the Chamber of Commerce under number 53815211.",
          h4('2. What (categories of) personal data will be processed?'),
          'We will process the following personal data:',br(),
          'a. Name;',br(),
          'b. Email address', br(),
          h4('3. For which purposes are my personal data processed, and on the basis of which legal grounds?'),
          'The personal data will only be used for the following purposes:',br(),
          '- To be used by the CoDa editorial team to contact you with any questions regarding the study you submit to the databank;', br(),
          'We process your personal data on the basis of the following legal grounds:',br(),
          '- You have given consent to the processing of your personal data. In that case you have the right to withdraw your consent at any time by contacting us via the contact details below.',
          h4('4. Who has access to my personal data?'),
          'The personal data will only be accessed by employees of the VU who by reason of their function have a role in the processing of your personal data for the abovementioned purposes and for whom it is necessary that they have access to the personal data.',br(),
          h4('5. Will my personal data be shared with third parties?'),
          "When processing your personal data, we may use service providers (processors) who process your personal data on behalf of and under the responsibility of the VU. The VU concludes processing agreements with these service providers to ensure that your personal data is processed carefully, securely and in accordance with the General Data Protection Regulation (GDPR). We remain solely responsible for these processing activities.",br(),
          "The following categories of personal data shall be shared publicly on the CoDa website:", br(),
          "a. Name (as Author of a study);",
          "b. Your name could be associated with any information that you have provided about the study which you annotated and submitted to be included in the cooperation databank. This will happen if your name is listed as an author of the study;", br(),
          "We process your personal data on the basis of the following legal grounds:",
          "- You have given consent to the processing of your personal data. In that case you have the right to withdraw your consent at any time by contacting us via the contact details below.", br(),
          "When processing your personal data, we may make use of service providers (processors) who process your personal data on behalf of and under the responsibility of the VU.  The VU concludes processing agreements with these service providers to ensure that your personal data is processed carefully, securely and in accordance with the General Data Protection Regulation (GDPR). We remain solely responsible for these processing activities.",
          h4('6. Will my personal data be transferred to countries outside of the European Economic Area?'),
          "Your name (as an author of the study) and information you share about your study can be made public on the cooperation databank – which is an open access databank of study on human cooperation. We will not make your email address public on the cooperation databank. Your email will only be accessible to the editorial team of the cooperation databank. ",
          h4('7. For how long will my personal data be retained?'),
          "We will not retain your personal data for longer than is necessary to achieve the predetermined purposes or as long as required by law. ", br(),

          tags$table(
            style = "border: 1px solid black; width: 100%; border-collapse: collapse;",
                    tags$tr(
                            tags$th('Categories of personal data', style = 'border: 1px solid black; padding: 2px; '),
                            tags$th('Retention period', style = "width: 65%; border: 1px solid black; padding: 2px;"),
                    ),
                    tags$tr(
                            tags$td('Email',style = 'border: 1px solid black;padding: 2px;'),
                            tags$td('10 years after making the study public',style = 'border: 1px solid black;padding: 2px;'),
                   ),
                   tags$tr(
                            tags$td('Name',style = 'border: 1px solid black;padding: 2px;'),
                            tags$td('This information will be publicly available in the cooperation databank if you are listed as an author of the study, and will remain public until you exercise your right for the information to be deleted from the databank.  '
                              ,style = 'border: 1px solid black;padding: 2px;'),
                   )
            ),

          h4("8. How will my personal data be secured?"),
          "The VU takes appropriate technical and organizational measures to protect your personal data against loss and any form of unlawful processing.", br(),
          "Personal information will be contained in password protected spreadsheets, to which only members of the CoDa editorial team will be provided with access rights. ",
          h4("9. Who can I contact with questions about the processing of my personal data?"),
          "You can ask questions about how we process your personal data via [d.p.balliet@vu.nl] or [+31681459096].",
          h4("10. How can I exercise my privacy rights?"),
          "On the basis of the GDPR you have the right – under certain conditions – to access your personal data that we process, to correct your personal data if it contains factual inaccuracies, to delete your personal data, to limit the processing of your personal data, to portability of your personal data and to object to the processing of your personal data.", br(),
          "If you wish to exercise any of these privacy rights, you can contact the Data Protection Officer of VU Amsterdam via:",br(),
          HTML('&emsp;'),"Data Protection Officer",br(),
          HTML('&emsp;'),"De Boelelaan 1105", br(),
          HTML('&emsp;'),"1081 HV AMSTERDAM ", br(),
          HTML('&emsp;'),"functionarisgegevensbescherming@vu.nl ", br(),
          "To be able to deal with your request, you will be asked to provide proof of identity. In this way it will be verified that the request has been made by the right person. If you are not satisfied with the way in which we deal with your personal data, you have the right to submit a complaint with a supervisory authority."

        ),

        footer = modalButton("Agree")
      ))
  })

  output$variables <- renderUI({
    numVar <- as.integer(input$numberOfVariables)
    lapply(1:numVar, function(k) {
      tags$div(
        h4(paste0('Variable', k, ' :')),
        h4('Specify the number of Treatments for Variable', k),
         fluidRow(
          column(6, numericInput(inputId = paste0("treatmentNum",k),
                    label = p(labelMandatory("Number of Treatments")
                    ), 1
                    ),),
        ),

        output[[paste0("TreatmentsOutput", k)]] <- renderUI({
          numTreatments <- as.integer(input[[paste0("treatmentNum", k)]])
          lapply(1:numTreatments, function(i) {
            tags$div(

              h5('Specify the number of Generic Independent Variable for Variable', k, 'Treatment', i),
              fluidRow(
                column(6, numericInput(inputId = paste0("GIVNum", k, '_', i),
                 label = p(labelMandatory("Number of Generic Independent Variable")), value = 1
                ),),
              ),
              output[[paste0("GIVoutput", k, '_', i)]] <- renderUI({
                numGeneric <- as.integer(input[[paste0("GIVNum", k, '_', i)]])
                lapply(1:numGeneric, function(m) {
                tags$div(
                  fluidRow(
                    column(6, selectInput(inputId = paste0("addGenIV", k, '_', i, '_', m),
                    label = p(labelMandatory("Generic Independent Variable"), br(),
                    helpText('Example: Punishment', br(),
                    em('Check our codebook for the list of all the Generic Independent Variables and their definitions')),
                    ),
                    choices = c("",sort(selections$ivname))),),
                  ),

                  fluidRow(column(6,
                      checkboxInput(paste0("addDescriptionGenericIV", k, '_', i, '_', m),
                                    "Add description for Generic Independent Variable")
                      )
                  ),

                  conditionalPanel(
                    condition = paste0('input.addDescriptionGenericIV', k, '_', i, '_', m, ' == 1'),
                    fluidRow(column(8, textAreaInput(paste0("addDescriptionGenericIVText", k, '_', i, '_', m),
                                  "Description for Generic Independent Variable"))),
                  ),

                  conditionalPanel(condition = paste0('input.addGenIV', k, '_', i, '_', m,' != ""'),
                    h5('Specify the number of Specific Independent Variable for Variable', k , 'Treatment', i, 'Generic IV', m),
                    fluidRow(
                      column(6, numericInput(inputId = paste0("SIVNumber",k, '_', i, '_', m),
                       label = p(labelMandatory("Number of Specific Independent Variable")), 1
                      ),),
                    ),

                    output[[paste0("SIVNumber",k, '_', i)]] <- renderUI({
                      numSpecific <- as.integer(input[[paste0("SIVNumber",k, '_', i, '_', m)]])
                      lapply(1:numSpecific, function(n) {
                        tags$div(
                          fluidRow(
                            column(6, selectInput(inputId = paste0("SubpropSelection", k, '_', i, '_', m, '_', n), #name of input used to be "gen_iv", also removed selectInput for current_iv
                                 label = p("Specific Independent Variable",
                                           helpText("Example: Punishment treatment", br(),
                                                    em("Check our codebook for the list of all the Specific Independent Variables and their definitions"))
                                 ),
                                 choices = c("", ivLabelsGen(input[[paste0("addGenIV", k, '_', i, '_', m)]]))),),
                          ),
                          fluidRow(column(6,
                                  checkboxInput( paste0("DescriptionSpecificIV", k, '_', i, '_', m, '_', n),
                                                "Add description for Specific Independent Variable")
                                  )
                          ),
                          conditionalPanel(
                                    condition = paste0('input.','DescriptionSpecificIV', k, '_', i, '_', m, '_', n, ' == 1'),
                                    fluidRow(column(8, textAreaInput(paste0("DescriptionSpecificIVText", k, '_', i, '_', m, '_', n),
                                                            "Description for Specific Independent Variable"))),
                                     ),
                          fluidRow(
                            column(6, selectizeInput(inputId = paste0("ValueOptionsSelection",  k, '_', i, '_', m, '_', n),
                                 label = p("Specific Independent Variable values", br(),
                                 helpText("The possible values of a treatment.")
                                 ), #label displayed in ui
                                 choices = c("", valueOptionUpdateGen(isolate(input[[paste0("SubpropSelection",  k, '_', i, '_', m, '_', n)]]))
                                             ), options = list(create = TRUE)

                             ),),
                          ),

                        )# div
                      })

                    })

                  ),

                )
              })
              }),

                  # quantitative variables for treatment
                  fluidRow(column(8,
                  radioButtons(paste0('quantitativeMethod', k, '_', i),
                               "Please provide quantitative variables (either a or b) ",
                                choices = c("a: Provide correlation and sample size" = "a", "b: Provide Mean, Standard Deviation, and sample size" = "b"),
                               selected = character(0)
                  )
                  )),

                  conditionalPanel(
                                    condition = paste0('input.', 'quantitativeMethod', k, '_', i,' == "a"'),
                  #fluidRow(column(
                  #4, selectInput('addEsMeasure0', 'Choose an effect measure:',
                  #                 choices =c('',"Standardised Mean Difference" = "d", "Raw correlation coefficient" = "r"))
                  #)),
                  fluidRow(column(
                  4, numericInput(paste0('addCorrelation', k, '_', i), labelMandatory('Correlation'), NULL)
                  )),
                                    fluidRow(column(
                  4, numericInput(paste0('addSampleSizeA', k, '_', i), labelMandatory('Sample size'), NULL)
                  )),

                  ),

                  conditionalPanel(
                                    condition = paste0('input.', 'quantitativeMethod', k, '_', i,' == "b"'),

                      fluidRow(column(
                      4, selectInput(paste0('DVBehavior', k, '_', i), labelMandatory('Behavior (dependent variable)'),
                        choices = c('', 'Contributions', 'Cooperation', 'Withdrawals')
                        )
                      )),
                      conditionalPanel(
                                    condition = paste0('input.', 'DVBehavior', k, '_', i,' == "Contributions" || input.', 'DVBehavior', k, '_', i,' =="Withdrawals"'),

                      fluidRow(column(
                      4, numericInput(paste0('addMean', k, '_', i), labelMandatory('Mean'), NULL)
                      )),
                      fluidRow(column(
                      4, numericInput(paste0('addStandardDeviation', k, '_', i), labelMandatory('Standard Deviation'), NULL)
                      )),
                      ),

                      conditionalPanel(
                                    condition = paste0('input.', 'DVBehavior', k, '_', i,' == "Cooperation" '),
                                          fluidRow(column(
                      4, numericInput(paste0('addProportionOfCooperation', k, '_', i), labelMandatory('P(C) (proportion of cooperation'), NULL)
                      )),

                      ),

                      fluidRow(column(
                      4, numericInput(paste0('addSampleSizeB', k, '_', i), labelMandatory('Sample size'), NULL)
                      )),
                  ),

                   fluidRow(column(
                    4, radioButtons(paste0('betweenOrWithinSubjects',k, '_', i ), 'Between or Within Subjects',
                                     choices = c("Between Subjects" = "Between", "Within Subjects" = "Within"),
                                    selected = character(0)
                      )
                    )),
              HTML('<hr style="color: orange;">')

            )
          })
        }),
           HTML('<hr style="color: purple;">')
      )
    })

  })


})
