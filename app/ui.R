#### Coda Dashboard #####
### For further description see server.R.

# File with all the libraries.
source('loadLibraries.R');

# The header includes a picture that must be in the app/www.
header <- dashboardHeader(
  title = "Cooperation Databank",
  #color="orange",
  titleWidth = 500,
  tags$li(a(href = 'https://cooperationdatabank.org',
            target = "_blank", # Open in new window
            img(src ='data/coda-logo-white.svg', height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
)

body <- dashboardBody(
  shinyjs::useShinyjs(), id = "whole_dashboard",
  useSweetAlert(),
  #### Body ####
  tags$body(
    tags$style(HTML("
                    .shiny-output-error-validation {
                        text-align : center;
                        background-color: #f2dede;
                        border-color: #ebcccc;
                        color: #a94442;
                        width : 100%; height : 100%;
                    }
                    
                    .skin-blue .main-header .navbar, .logo {
                              background-color: #002B40!important;
                              
                    }
                    
                     .main-header {
                       border-bottom : #FBAC0C solid 1px!important;
                     }
                     .main-header .logo {
                      font-family : 'Roboto Bold',Verdana,sans-serif;
                      line-height : 48px!important;
                      font-size : 24px!important;
                      
                    }
                    
                    .skin-blue .left-side, .skin-blue .main-sidebar {
                          background-color: #F7FbFD!important;
                          border: 1px solid #d2d6de;
                          color: #e7f0f2!important;
                          left: 0px;
                          top: -1px;
                    }
                    .sidebar-menu a {
                        color : #002B40!important
                    }
                    
                    .wrapper, .content-wrapper {
                        background-color: #e7f0f2!important;
                    }
                    
                    .nav-tabs-custom>.nav-tabs>li.active {
                          border-top-color: #fbac0c!important;
                    }
                    
                    
                    .box-info>.box-header {
                        background-color: #002B40!important;
                    }
                    .box-info { 
                        border-color : #002B40!important; 
                    }
                    .box.box-success { 
                        border-color : #fbac0c!important; 
                    }
                    .box-success>.box-header {
                        background-color : #fbac0c!important ;
                        color : #fff!important;
                        border-color : #fbac0c!important; 
                    }
                    
                    .btn-myinfo {
                        background-color: #002B40!important ;
                        color : #fff!important;
                    }
                    .box-info {
                        border: 1px solid #002B40!important ;
                    }
                    .box-success {
                        border: 1px solid #002B40!important ;
                    }
                    
                    .sw-dropdown {
                    display : flex!important;
                    }
                    .sw-dropdown > button {
                    flex  : 100!important;
                    }
                    .sw-dropdown > .sw-dropdown-content {
                    margin-top: 46px;
                    }
                    .customtitle {
                      color : #002B40!important;
                       font-family : 'Roboto Bold',Verdana,sans-serif;
                       font-size : 22px!important;
                      font-weight: bold!important;
                    } 
                    
                    .sidebar-toggle {
                      color : #fbac0c!important;
                    }

                    .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
                          background-color: #e4e4e3;
                          color : #002B40!important;
                          border-left-color: #fbac0c;
                    }
                    
                    .bg-teal, .bg-teal a  { 
                        color :  #002B40!important;
                        background-color : #87c211!important;
                    }
                    
                    .bg-purple { 
                        background-color : #ACB5C3!important ;
                        color : #fff!important;
                    }
                    
                    .bg-red { 
                        background-color : #e7f0f2!important ;
                        color : #002B40!important;
                    }
                    
                    .bg-aqua { 
                        background-color : #fbac0c!important ;
                        color : #fff!important;
                    }
                    
                    .bg-light-blue { 
                        background-color : #82c211!important ;
                        color : #fff!important;
                    }
                    
                    .bg-lime {
                        background-color : #fff!important ;
                        border : #002B40 1px solid!important;
                        color : #002B40!important;
                        font-weight : bold!important;
                    }
                    .bg-lime a  {
                      color : #82c211!important;
                    }
                    
                    .small-box { 
                          font-family : Roboto Bold,Verdana,sans-serif!important;
                    }

                    table.dataTable tr.selected td,
                    table.dataTable td.selected {
                      background-color: #fbac0c !important;
                    }
                    
                    .tippy-popper {
                        font-size : 15px!important;
                    }
                    
                  
                    .datatables {
                      border-left: #87C211 3px solid!important;
                    }
                    
                    .dataTables_wrapper { 
                      margin-left : 5px;
                    }

                    .help-block {
                      font-weight: 500;
                    }
                    

                    ")
    )
  ),
  
  #### Items ####
  tabItems(
    #### Overview ####
    tabItem("overview",
            box(width=12, title = p(class="customtitle","Overview of the studies"), solidHeader = F, status = "warning",
                
                uiOutput("summary") ,
                
                div(id= "showSummary",
                    div(radioButtons("moderator", "Show studies by", choices =
                                       c("Country/Region" = "country", "Year of data collection" = "yearOfDataCollection",
                                         "Sample size" = "overallN")
                                     , selected = "country", inline = TRUE), align="center"),
                    conditionalPanel("input.moderator == 'country'",
                                     plotlyOutput("countries") %>% withSpinner()
                    ),
                    conditionalPanel("input.moderator == 'yearOfDataCollection' || input.moderator == 'overallN' ",
                                     plotlyOutput("show_studies") %>% withSpinner()
                    )
                ) # end div
                , 
                column(width=8),column(width=4,
                  materialSwitch(
                  'hidePlots',
                  value = F, status= 'warning',
                  label = "Hide Figures"
                )
                )

           ),
           box(width=12, status = "warning", title=p( class="customtitle","Select studies"), solidHeader = F,
               column(width =12,
                p(class="text-muted", "Choose inclusion criteria to select studies."),
                br(),
                h4(strong("Use one of our selection examples:")),
                p(class="text-muted", "*Please allow some time for the data to update."),
                selectInput(inputId = "selectionExample", label=NULL,
                            choices = c("Select..." = 0,
                                        "Analyze the 	relationship between Honesty-Humility and cooperation." = 1,
                                        "Analyze the effect of punishment on cooperation, comparing a punishment treatment vs. any other treatment." = 2,
                                        "Compare the effects of punishment vs. reward on cooperation." = 3
                                        ))
                ),
                div( id= "All Selections",
                     column(12,
                            h4(strong("Or make your own selection:")),
                            br()
                     ),
                     column(2,actionBttn(
                       inputId = "helpSelection",
                       label = "Help me",
                       style='jelly',
                       size = 'sm',
                       color = 'success',
                       icon = icon("info-circle") 
                     )
                     ),column(8),
                     column(2,
                            actionBttn("clearFilters",
                                       label = "Reset filters", 
                                       style='jelly', size = "sm",  
                                       icon = icon("times-circle") ,
                                       color = "danger", no_outline=F)
                     ),
                     column(12,br()),
                     column(4,
                            # h5(strong("Treatment 1"), style='display :inline-block!important;'),
                            dropdown(label='Treatment 1  ',size='lg', 
                                     status = "myinfo", icon = NULL, width = "500px",
                                     #tooltip = tooltipOptions(title = "Click to see options for Treatment 1"),
                                     
                                     selectInput(inputId = "genIVselection1a",
                                                 label = p("Generic Independent Variable",
                                                           a(tippy(
                                                             paste("",icon("question-circle")),allowHTML = TRUE,"<h5>Generic Independent Variables of an experimental session (treatment)</h5>"))), #label displayed in ui
                                                 choices = ""),
                                     uiOutput("descriptionCategory1a"),
                                     p(),
                                     selectInput(inputId = "treatmentSubpropSelection1a", #name of input used to be "gen_iv", also removed selectInput for current_iv
                                                 label = p("Specific Independent Variable",a( tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Independent Variables of an experimental session. You need to specify a generic IV first.</h5>"))), #label displayed in ui
                                                 choices = "", selected = ""),
                                     uiOutput("descriptionSubprop1a"),
                                     selectInput(inputId = "valueOptionsSelection1a",
                                                 label = p("Specific Independent Variable values", a(allowHTML = TRUE, tippy(paste("",icon("question-circle")),"<h5>The possible values of a treatment.</h5>"))), #label displayed in ui
                                                 choices = "", selected = ""),
                                     uiOutput("descriptionValue1a"),
                                     # actionButton("reset_input", "Reset all inputs", align="middle"),
                                     # br(),
                                     hr(),
                                     selectInput(inputId = "extraCriteria1", label = "Would you like to select more criteria (how many)?", choices = 0:5, selected = 0),
                                     radioButtons(inputId = "combinator1", label = "Would you like to combine with AND or with OR?",  choices = c("AND" = "AND", "OR" = "OR")
                                                  , selected = "AND", inline = TRUE),
                                     uiOutput('selectionCriteria1optional')
                            ),
                            br(),
                            # h5(strong("Treatment 2"), style='display :inline-block!important;'),
                            dropdown(
                              # tags$h3("Treatment 2"),
                              label='Treatment 2  ',size='lg', right = FALSE,
                              status = "myinfo", icon = NULL, width = "500px",
                              # tooltip = tooltipOptions(title = "Click to see options for Treatment 2"),
                              selectInput(inputId = "genIVselection2a",
                                          label = p("Generic Independent Variable",
                                                    a(tippy(
                                                      paste("",icon("question-circle")),allowHTML = TRUE,"<h5>Generic Independent Variables of an experimental session (treatment)</h5>"))), #label displayed in ui
                                          choices = ""),
                              uiOutput("descriptionCategory2a"),
                              selectInput(inputId = "treatmentSubpropSelection2a", #name of input used to be "gen_iv", also removed selectInput for current_iv
                                          label = p("Specific Independent Variable",a( tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Independent Variables of an experimental session. You need to specify a generic IV first.</h5>"))), #label displayed in ui
                                          choices = "", selected = ""),
                              uiOutput("descriptionSubprop2a"),
                              selectInput(inputId = "valueOptionsSelection2a",
                                          label = p("Specific Independent Variable values", a(allowHTML = TRUE, tippy(paste("",icon("question-circle")),"<h5>The possible values of a treatment.</h5>"))), #label displayed in ui
                                          choices = "", selected = ""),
                              uiOutput("descriptionValue2a"),
                              br(),
                              hr(),
                              selectInput(inputId = "extraCriteria2", label = "Would you like to select more criteria?", choices = 0:5, selected = 0),
                              radioButtons(inputId = "combinator2", label = "Would you like to combine with AND or with OR?",  choices = c("AND" = "AND", "OR" = "OR")
                                           , selected = "AND", inline = TRUE),
                              uiOutput('selectionCriteria2optional')
                            )),
                     column(4,
                            # h5(strong("Sample Characteristics"), style='display :inline-block!important;'),
                            dropdown(
                              label = "Sample Characteristics", size = 'lg',
                              status = "warning", icon = icon("users"), width = "500px",
                              sliderInput("yeardatacollection", "Year of data collection", dragRange= T,
                                          1950, 2020,  value = c(1950, 2020), sep = ""),
                              
                              selectInput("yearSource", label = p("Source of year of data collection", a(tippy(allowHTML = TRUE, paste("",icon("question-circle")),"<h5>Source of information about what year the data was collected.</h5>"))),
                                          choices = c("Received/Submitted", "Conducted","Published", "Accepted","Presented", "Working paper published", "Available online"), multiple= TRUE),
                              selectInput(inputId = "filter_countries",
                                          label = p("Country/Region", a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Country or region where the data collection took place.</h5>"))),
                                          choices = NULL, multiple= TRUE),
                              selectInput("countrySource", p("Source of country/region", a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Source of the country where the data collection took place.</h5>"))), multiple = TRUE,
                                          choices = c("Specified country", "Most authors","All authors","Multiple countries") ),
                              sliderInput("overallN", "Sample size",dragRange= T,
                                          0, 3000, value=c(0,3000),step = 10),
                              sliderInput("maleProportion", "Proportion of males in study",dragRange= T,
                                          0, 1.0, value=c(0,1.0),step = 0.1),
                              sliderInput("meanAge", "Mean age", dragRange= T,
                                          0, 100, value=c(0,100),step = 5),
                              sliderInput("ageHigh", "Highest age",dragRange= T,
                                          0, 100, value=c(0,100),step = 1),
                              sliderInput("ageLow", "Lowest age",dragRange= T,
                                          0, 100, value=c(0,100),step = 1),
                              radioButtons("studyStudentSample", label = p("Student sample only", a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether participants were recruited from a student population. TRUE = Participants were recruited from a student population, FALSE = Participants were not recruited from a student population.</h5>"))), 
                                           choices = c("Yes" , "No", "All" )
                                           , selected = 'All', inline = TRUE)
                              ,
                              
                              selectInput("studyAcademicDiscipline",  p("Academic discipline", a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Participant's field of study if the participant is a student. Multiple choices are allowed.</h5>"))),multiple=TRUE,
                                          choices = c("Economics" = "1", "Psychology" = "2", "Sociology" = "3", "Mixed" = "4", "Other" = "5") ),
                              selectInput("recruitmentMethod", p("Recruitment method", a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>The way participants were recruited to take part in the study.</h5>"))), multiple=TRUE,
                                          choices = c("Participant pool" = "1", "Mechanical Turk" = "2", "Advertisement" = "3", "Other" = "4", "ORSEE" = "5") )
                              
                            ),
                            br(),
                            # h5(strong("Study Characteristics"), style='display :inline-block!important;'),
                            dropdown(
                              label = "Study Characteristics", size = 'lg',
                              status = "warning", icon = icon("book"), width = "500px",
                              # tooltip = tooltipOptions(title = "Click to choose your study characteristics"),
                              
                              selectInput("studyExperimentalSetting",p("Experimental setting", a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>The setting in which the experiment was conducted. Multiple choices are allowed.</h5>"))), multiple = TRUE,
                                          choices = c("Online" = "Online", "Lab" = "Lab", "Classroom" = "Classroom", "Field" = "Field", "Lab in the field" = "Lab in the field", "Natural experiment" = "Natural experiment", "Other" = "Other") ),
                              selectInput("studyDilemmaType",p("Game type", a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Economic game that participants play during the experimental session.</h5>"))),
                                          choices = c("Public Goods Game","Prisoner's Dilemma Game", "Resource Dilemma","Other" )
                                          , multiple= TRUE),
                              radioButtons("studyContinuousPGG", p("Continuous vs. step-level public goods", a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether there was a provision point for contributions to establish a public good (i.e., step-level public goods) versus each contribution provided an incremental benefit to the public good (i.e., continuous public good).</h5>"))),
                                           choices = c("Continuous", "Step-level", "All" )
                                           , selected = 'All', inline = TRUE),
                              radioButtons("studySymmetric", label = p("Symmetric",a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether specific aspects of the game that were different (i.e., asymmetric) for participants. TRUE = Symmetric, FALSE = Asymmetric.</h5>"))),
                                           choices = c("Yes" = TRUE, "No" = FALSE, "All" = "All"),  selected = "All", inline = T),
                              radioButtons("studyOneShot", p("One-shot vs. repeated",a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether participants played the game with the same person only once (this also applies if participants switch partners after each trial) versus played the game repeatedly with the same person.</h5>"))),
                                           choices = c("One-shot" , "Repeated", "All" )
                                           , selected = 'All', inline = TRUE),
                              radioButtons("studyMatchingProtocol", label  = p("Matching",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>The way participants are paired with others during interactions.</h5>"))),
                                           choices = c("Stranger", "Partner" , "All"), selected = "All", inline = T ),
                              radioButtons("studyOneShotRepeated", p("Repeated one-shot game" ,a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether participants are paired with different partner(s) after each trial across many trials. TRUE = repeated one-shot, FALSE = one-shot.</h5>"))),
                                           choices = c("Yes" = TRUE, "No" = FALSE, "All" = 'All')
                                           , selected = 'All', inline = TRUE),
                              radioButtons("studyKnownEndgame", label = p("Known endgame",a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether participants know the exact number of trials at the beginning of the experiment. Endgame is considered known also when participants played a one-shot game. TRUE = Known endgame, FALSE = Endgame not known.</h5>"))),
                                           choices = c("Yes" = TRUE, "No" = FALSE, "All" = "All"),  selected = "All", inline = T),
                              selectInput("studyShowUpFee", label = p("Show-up fee",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>How participants were compensated for participation.</h5>"))),
                                          choices = c( "Paid","Course credit", "Non-monetary","Absent" )
                                          , multiple= TRUE),
                              selectInput("studyGameIncentive", label = p("Game incentive",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>Whether participants' decisions in the game determine their payoffs in different forms.</h5>"))),
                                          choices = c("Hypothetical", "Monetary","Non-monetary","Monetary lottery", "Non-monetary lottery" )
                                          , multiple= TRUE),
                              sliderInput("studyGroupSize", p("Group size",a(tippy(allowHTML = TRUE,paste0( "",icon("question-circle")),"<h5>Overall number of people affected by the choices in the game.</h5>" ))),dragRange= T,
                                          0, 400, value=c(0,400),step = 1),
                              sliderInput("studyKindex",  p("K index",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>For 2-persons PDG, it is calculated as (R-P)/(T-S), and 0 < K < 1. For N-persons PDG, it is calculated as (A(n)-B(0))/(0(max)-0(min)), and 0 < K < 1.</h5>" ))), dragRange= T,
                                          0, 1, value=c(0,1),step = 0.2 )   ,
                              sliderInput("studyMPCR",p("MPCR",a(tippy(allowHTML = TRUE,paste0("" ,icon("question-circle")), "<h5>The ratio of benefits to costs for a member to contribute one monetary unit to the group account. Calculated as multiplier divided by group size.</h5>"))), dragRange= T,
                                          0, 1, value=c(0,1),step = 0.2)   ,
                              sliderInput("studyPGDThreshold",p("Threshold",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")), "<h5>The minimum threshold of total contribution by all group members when the public good will be provided. This also defines the criticality of contributions, as contributions are more critical if other players cannot reach the threshold on their own (e.g. in sequential games where others’ prior contributions are known).</h5>"))), dragRange= T,
                                          0, 25, value=c(0,25),step = 1)   ,
                              sliderInput("replenishmentRate",p("Replenishment rate",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")), "<h5>The replenishment rate in the resource dilemma. The remaining resource in the common pool is multiplied by the replenishment rate after each trial.</h5>"))), dragRange= T,
                                          0, 25, value=c(0,25),step = 1)   ,
                              selectInput("discussion", p("Discussion",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>Whether communication was allowed between participants in the game.</h5>" ))),multiple = TRUE,
                                          choices = c("Uni-directional","Absent","Bi-directional") ),
                              selectInput("participantDecision", label =  p("Sequentiality decision",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")), "<h5>Whether group members make their decisions simultaneously or sequentially, i.e., whether participants take decisions one after another while receiving some form of feedback about preceding decisions.</h5>"))),
                                          choices = c("Simultaneous","Sequential turn-taking","Sequential leadership-by-example" )
                                          , multiple= TRUE),
                              sliderInput("choiceLow", p("Lowest choice option",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")), "<h5>Lowest choice option allowed to participants. The value of the lowest choice option. Is '0' in a binary choice between non-numeric options (such as 'cooperate' vs. 'defect'; 'C' vs. 'D').</h5>"))),dragRange= T,
                                          0, 3, value=c(0,3),step = 1),
                              sliderInput("choiceHigh", p("Highest choice option",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>Highest choice option allowed to participants. The value of the highest choice option. Is '1' in a binary choice between non-numeric options (such as 'cooperate' vs. 'defect'; 'C' vs. 'D').</h5>"))),dragRange= T,
                                          0, 15, value=c(0,15),step = 1),
                              sliderInput("studyNumberOfChoices",p("Number of choice options",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>The number of choice options players have when making the contribution. Is '2' when making a binary choice (such as cooperate vs. defect); (n+1) for continuous measure of cooperation; 3 for 3-choice Prisoner's Dilemma, etc.</h5>" ))), dragRange= T,
                                          0, 20, value=c(0,20),step = 1)  ,
                              radioButtons("deception", label = p("Deception",a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether participants believed that they were playing with real others.</h5>"))),
                                           choices = c("Yes" = TRUE, "No" = FALSE, "All" = "All"),  selected = "All", inline = T),
                              selectInput("studyRealPartner", label =p("Real Partner",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>Whether participants interact with real or imagined participants in the game.</h5>" ))),
                                          choices = c( "Real","Hypothetical","Deception")
                                          , multiple= TRUE),
                              selectInput(inputId = "studyAcquaintance", label = p("Acquaintance",a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether participants were interacting (and were aware of it) with people that they are not acquainted with or with people that they knew outside of the laboratory (e.g., friends, relatives, parents, romantic partners).</h5>")) ) ,
                                          choices = NULL, multiple= TRUE),
                              radioButtons("sanction", label = p("Sanction",a(tippy(allowHTML = TRUE,paste("",icon("question-circle")),"<h5>Whether a sanction (punishment or reward) mechanism was in place in the game. TRUE = sanction was present, FALSE = sanction was absent.</h5>"))),
                                           choices = c("Yes" = T, "No" = F, "All" = "All"),  selected = "All", inline = T)
                            )),
                     
                     column(4,
                            # h5(strong("Quantitave Study Results"), style='display :inline-block!important;'),
                            
                            dropdown(
                              label ="Quantitave Study Results", size='lg', status = "warning", icon = icon("tasks"), width = "500px",
                              # tooltip = tooltipOptions(title = "Click to see options"), 
                              right=T,
                              selectInput("studyTrialOfCooperation", label = p("Trial of cooperation",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>Trial on which the Dependent Variable was assessed.</h5>"))) ,
                                          choices = c("All trials" , "First trial", "First and last trials", "Last trial", "Other trials"), multiple= TRUE),
                              sliderInput("overallProportionCooperation",  p("Overall proportion of cooperation",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")), "<h5>Cooperative behavior for games (e.g. prisoner's dilemma and public goods game) with two choice options. Higher numbers equal higher cooperation.</h5>"))),dragRange= T,
                                          0, 10, value=c(0,10),step = 1),
                              sliderInput("overallMeanContributions", p("Overall mean of contributions",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")),"<h5>Cooperative behavior when the game (e.g., prisoner's dilemma and public goods dilemma) had a maximum number of choices greater than two. Higher numbers equate to higher cooperation.</h5>" ))),dragRange= T,
                                          0, 15, value=c(0,15),step = 0.1),
                              sliderInput("overallMeanWithdrawal", p("Overall mean of withdrawals",a(tippy(paste0("",icon("question-circle")), "<h5>Withdrawal behavior when the game type is a resource dilemma and the maximum choice range is about two. Higher numbers equate to lower cooperation.</h5>"))),dragRange= T,
                                          0, 10, value=c(0,10),step = 1),
                              sliderInput("overallStandardDeviation", p("Overall standard deviation of contributions or withdrawals",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")), "<h5>Standard deviation of contributions or withdrawals, only applicable when maximum number of choices is greater than two.</h5>"))),dragRange= T,
                                          0, 10, value=c(0,10),step = 1),
                              sliderInput("overallPercentageEndowmentContributed",p("Percentage of endowment contributed",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")), "<h5>Percentage of endowment contributed, calculated as (M - Lower choice option) / (Upper choice option – Lower choice option).</h5>"))), dragRange= T,
                                          0, 1, value=c(0,1),step = 0.1)   ,
                              sliderInput("numberOfObservations",p("Number of observations",a(tippy(allowHTML = TRUE,paste0("",icon("question-circle")), "<h5>Use this variable as sample size to compute variance for meta-regressions predicting cooperation. It corresponds to the total sample size in a single study after exclusion of participants (N), but it uses the number of observations for studies that uses both individual and groups as unit of analysis (e.g., interindividual-intergroup discontinuity studies).</h5>"))), dragRange= T,
                                          0, 2500, value=c(0,2500),step = 1) 
                              
                            ),
                            br(),
                            
                            dropdown(
                              label ="Paper Metadata", size = 'lg', status = "warning", icon = icon("cubes"), width = "500px",
                              right=T, 
                              # tooltip = tooltipOptions( title = "Click to see options"),
                              selectizeInput(inputId = "lang",
                                             label = "Language",# by which to filter observations from their papers:", #label displayed in ui
                                             choices = NULL, multiple= TRUE,
                                             options = list(placeholder = 'Select language')),
                              selectizeInput(inputId = "author",
                                             label = "Authors",# by which to filter observations from their papers:", #label displayed in ui
                                             choices = NULL, multiple= TRUE,
                                             options = list(placeholder = 'Select authors')),
                              
                              selectInput("publicationStatus", label = p("Publication status"),
                                          choices = c("Published Article","Doctoral Dissertation","Working paper","Master’s thesis","Raw data"), multiple= TRUE)
                            )
                     )
                     
                ) #end of div
            ), # end box of criteria selection
            box(width=12, status="warning" , title = p(class="customtitle","Explore your selection"), solidHeader = F,
                h5(
                  icon("info-circle"),
                  "Click on the rows in the table to ", strong(style= 'color:red', "de-select"), "effect sizes"
                ),
                
                fluidRow(
                  column(12,
                         br(),
                         DT::dataTableOutput('render_data')
                  )
                ),
                p(),
                valueBox(width = 4,value = downloadLink('downloadRefsAPA',icon("download")), subtitle= "APA references (.rtf)" , icon = icon("file-word"),color='lime'),
                valueBox(width = 4,value =   downloadLink('downloadRefs',  icon("download")), subtitle="Bibtex references (.bib)" , icon = icon("file-pdf"),color='lime'),
                valueBox(width = 4,value =   downloadLink('downloadData', icon("download")), subtitle= "Raw dataset (.csv)" , icon = icon("table"), color="lime")
            )
            
    ),
    #### Meta-Analyses ####
    tabItem("analyses",
            #### validationBox ####
            div(class= "validationBox",
                box(title="Meta-analytic models", solidheader=F, width =  12, status = 'danger',
                    
                    h2(icon("exclamation-triangle")),
                    p("In order to do this, you need to select some data first. "),
                    p("Go back to data overview and select at least one specific IV Treatment.")
                )
            ),
            #### modelBox ####
            div(id="modelBox",
                box(width=12, title = p(class="customtitle","Choose your model"), solidHeader = F, status = "warning",
                    fluidRow(
                      column(4, selectInput(inputId = "es_measure",
                                            label = "Choose an effect measure:", selected = "d",
                                            choices =c("Standardised Mean Difference" = "d", "Raw correlation coefficient" = "r") #"Risk Difference" = "RD", "Odd Ratio" = "OR",
                      )
                      ),
                      column(4, selectInput(inputId = "ma_model",
                                            label = "Specify a model", selected = "REML",
                                            choices = c("Fixed effect" = "FE",
                                                        "Hunter-Schmidt estimator" = "HS",
                                                        "Hedges estimator" = "HE",
                                                        "DerSimonian-Laird estimator" = "DL",
                                                        "Sidik-Jonkman estimator" = "SJ" ,
                                                        "Maximum-likelihood estimator" = "ML",
                                                        "Restricted ML estimator" = "REML",
                                                        "Empirical Bayes estimator" = "EB")
                      )
                      )
                    ),
                    
                    fluidRow(
                      column(12,
                             h4(strong("Moderators")),
                             p(class="text-muted","Choose additional moderators (mods.) for your analysis")
                      ),
                      column(3, selectInput(inputId = "moderator_variables",
                                            label = "Variable mods.", choices=NULL, multiple = TRUE )
                      ),
                      column(3, selectInput(inputId = "moderator_characteristics",
                                            label = "Study mods.", choices=NULL, multiple = TRUE )
                      ),
                      column(3, selectInput(inputId = "multilevel_variables",
                                            label = "Multilevel clusters", choices=c("Study" = "studyNameGeneral", "Paper" = "paperName", "Country/region" = "country"), multiple = T )
                      ),
                      column(3, selectInput(inputId = "country_moderators",
                                            label = "Country/Region mods.", choices=NULL, multiple = T ) 
                      )
                    ),
                    fluidRow(
                      uiOutput("moderator_description")
                    ),

                    actionLink("expand_analysisForm", "Add your study for meta-analysis")
                )
            ),
            # Add table that shows all previous added paper
            shinyjs::hidden(
              div(id='addedPaperData',
                  box(title = "Studies will be added to meta-analysis", width = 12, side = "right",
                      DT::dataTableOutput("addedPaper", width = '100%'),
                      br(),
                      actionLink("addAnotherMetaAnalysisPaper",
                                 "Click to add another study for meta-analysis"),
                      br(),
                      br(),
                      actionButton("reMetaAnalysis", "Add study to meta-analysis", class="btn-success"),
                      # TODO: reset button;
                      #actionLink("resetMetaAnalysisPaper", "Reset these papers"),
                      br(),
                  ),
              )
            ),
            shinyjs::hidden(
                div(
                  id = "analysisForm",
                  box(title = "Add your study for Meta-analysis", width =  12, side = "right",
                      fluidRow(
                        # Paper Metadata
                        column(9, h4('Study Metadata')),
                      ),

                      fluidRow(column(8,textInput("analysisTitle", labelMandatory(label="Title")))),

                      fluidRow(
                        column(6,textInput(inputId ="analysisAuthor", labelMandatory(label="Authors")
                        )),
                      ),
                      fluidRow(column(6,
                               selectizeInput("analysisPublicationYear",
                                    labelMandatory("Year the study was conducted"),
                                    choices = c('', 2021:1900),
                                    options = list(
                                      placeholder = 'Please select a year below',
                                      onInitialize = I('function() { this.setValue(""); }')
                            )
                          )
                        ),),

                      # Effect Size
                      br(),
                      h4('Effect Size'),
                      uiOutput("analysisEffectMeasure"),
                      br(),
                      fluidRow(
                        column(4, numericInput("effectSizeEstimate", labelMandatory(label="Effect Size Estimate"), value=NULL)),
                        column(4, numericInput("effectSizeVariance", labelMandatory(label="Effect Size Variance"), value=NULL)),
                        column(4, numericInput("effectSizeSampleSize", labelMandatory(label="Effect Size Sample Size"), value=NULL))
                      ),
                      tags$a(href="https://campbellcollaboration.org/escalc/html/EffectSizeCalculator-Home.php",
                             target="_blank", "Don't know your effect size? Here's a guide."),

                      # Treatment1
                      br(),
                      h4("Treatment 1"),
                      br(),
                      uiOutput("analysisTreatment1"),

                      # Treatment2
                      br(),
                      h4("Treatment 2"),
                      br(),
                      uiOutput("analysisTreatment2"),

                      # Moderators
                      br(),
                      h4("Moderators"),
                      br(),
                      uiOutput("analysisModerators"),

                      br(),
                      br(),
                      #actionButton("analyse_form", "Start analyse" , class = "btn-primary")
                      actionButton("addAnalysisPaper", "Add Study", class="btn-primary"),
                      br(),

                  )

                )
            ),

            #### resultBox ####
            div(id="resultBox",
                tabBox(title="Meta-Analysis Results", width =  12, side = "right", # with side=right , tab order is reverted
                       selected = 1,
                       #### Power Analysis ####
                       tabPanel("Power Analysis",
                                p(class = "text-muted","Calculate sample size based on effect size, significance and power."),
                                p("Statistical power analysis can estimate the minimum required sample size for the next study, 
                             given a set of assumptions about the effect size, significance level, and power."),
                                fluidRow(
                                  column(3, selectInput(inputId = "test_fam",
                                                        label = "Choose a test family:", selected = NULL,
                                                        choices =c("t-tests", "ANOVA","Correlations","Tests of Proportions", "Linear Models",
                                                                   "Chi tests"))
                                  ),
                                  column(3, selectInput(inputId = "test_type",
                                                        label = "test type:",
                                                        choices = NULL))
                                ) ,
                                div( htmlOutput("get_es_val")  ),
                                br(),
                                plotOutput("poweranalysis", width = '100%' )
                       ),
                       #### Publication Bias ####
                       tabPanel("Publication bias",
                                column(12,
                                       htmlOutput("publicationbias_interpretation")
                                ),
                                DT::dataTableOutput("publicationbias_table"),
                                div(
                                  checkboxInput("trimfill", label = "Use Trim-and-fill in funnel plot", value = FALSE)
                                ),
                                plotOutput("get_funnels", width='100%', hover = hoverOpts( delayType = "throttle", id="plot_hover")),
                                p(class = "text-muted",
                                  paste("This is a funnel plot which plots effect sizes according to their standard errors. 
                                   Studies with low standard errors are most often plotted near the average effect size, 
                                   and studies with higher standard errors will be distributed evenly on both sides of 
                                   the average, creating a funnel-shaped distribution. Asymmetry in the funnel-shape may 
                                   indicate publication bias.")),
                                p("See:", a("https://en.wikipedia.org/wiki/Funnel_plot"))
                       ),
                       #### Data Table ####
                       tabPanel("Data Table",
                                p("This table displays the studies, effect sizes, and moderator values included in the meta-analysis. 
                             The information on this table differs from the table in the “data overview” section, because that 
                             table includes studies coded for examining the same topic, but for which effect sizes were not computed."),
                                DT::dataTableOutput("metatable")
                       ),
                       #### Visualisation ####
                       tabPanel("Visualization",
                                selectInput( inputId = "vis_moderator_variables",
                                             label = "Choose one moderator:", choices = NULL, multiple = FALSE
                                ),
                                div(radioButtons("visualmoderator", "Show studies by", choices =
                                                   c("Forest Plot" = "forest","Violin Plot" = "violin")
                                                 , selected = "forest", inline = TRUE), align="center"),
                                conditionalPanel("input.visualmoderator == 'violin'",
                                                 p(class = "text-muted",  "This is a violin plot which plots the effect sizes, 
                                              including the probability density of the effect sizes at different values, 
                                              which is smoothed by a kernel density estimator. The violin plot includes 
                                              a marker for the median and a marker for the interquartile range."),
                                                 p("See:", a("https://en.wikipedia.org/wiki/Violin_plot")),
                                                 plotlyOutput( "get_violin" )
                                ),
                                conditionalPanel("input.visualmoderator == 'forest'",
                                                 p(class = "text-muted",
                                                   "This is a forest plot which displays effect sizes from the selected studies, 
                                              including the overall result of the meta-analysis."
                                                 ),
                                                 p("source:", a("https://en.wikipedia.org/wiki/Forest_plot")),
                                                 uiOutput("get_forests_ui"),
                                )
                       ), #end Visualisation tab
                       #### Meta-Analytic Models ####
                       tabPanel("Meta-analytic models", value =1,
                                
                                column(12,
                                       uiOutput("interpretation")
                                ),
                                withSpinner(DT::dataTableOutput("rma")),
                                br(),
                                p("Note: K = the number of effect sizes, CI = Confidence Interval; PI = Prediction Interval."),
                                br(),
                                p(strong("Raw output")),
                                verbatimTextOutput("metaforSummary")
                       )
                ),
                box(width=12, status="warning" , title = p(class="customtitle","Download data"), solidHeader = F,
                    valueBox(width = 4,value =   downloadLink('downloadMetaData', icon("download")), subtitle= "Selected dataset (.csv)" , icon = icon("table"), color="lime")
                )
            ) # end div
    ),
    #### Meta-Regression ####
    tabItem("metaregression",
            #### validationBox ####
            div(class= "validationBox",
                box(title="Meta-regression models", solidheader=F, width =  12, status = 'danger',
                    
                    h2(icon("exclamation-triangle")),
                    p("In order to do this, you need to select some data first. "),
                    p("Go back to data overview and select at least one specific IV Treatment.")
                    
                )
            ),
            #### modelBox ####
            div(id="modelBoxReg",
                box( width=12, title = p(class="customtitle","Choose your model"), solidHeader = F, status = "warning",
                     
                     fluidRow(
                       column(4, selectInput(inputId = "ma_model_reg",
                                             label = "Specify a model", selected = "REML",
                                             choices = c("Fixed effect" = "FE",
                                                         "Hunter-Schmidt estimator" = "HS",
                                                         "Hedges estimator" = "HE",
                                                         "DerSimonian-Laird estimator" = "DL",
                                                         "Sidik-Jonkman estimator" = "SJ" ,
                                                         "Maximum-likelihood estimator" = "ML",
                                                         "Restricted ML estimator" = "REML",
                                                         "Empirical Bayes estimator" = "EB" )
                       )
                       )
                     ),
                     
                     fluidRow(
                       column(12,
                              h4(strong("Moderators")),
                              p(class="text-muted","Choose additional moderators (mods.) for your analysis")
                       ),
                       column(3, selectInput(inputId = "moderator_variables_reg",
                                             label = "Variable mods.", choices=NULL, multiple = TRUE )
                       ),
                       column(3, selectInput(inputId = "moderator_characteristics_reg",
                                             label = "Study mods.", choices=NULL, multiple = TRUE )
                       ),
                       column(3, selectInput(inputId = "multilevel_variables_reg",
                                             label = "Multilevel clusters", choices=c("Study" = "studyNameGeneral", "Paper" = "paperName", "Country/region" = "country"), multiple = T )
                       ),
                       column(3, selectInput(inputId = "country_moderators_reg",
                                             label = "Country/Region mods.", choices=NULL, multiple = T ) 
                       )
                     ),
                     fluidRow(
                       uiOutput("moderator_description_reg")
                     ),
                     actionLink("expand_regressionForm", "Add your study for meta-regression")
                )
            ),

            #### Short Form Box for meta-regression####
            shinyjs::hidden(
              div(id='addedPaperDataRegression',
                  box(title = "Studies will be added to meta-regression", width = 12, side = "right",
                      DT::dataTableOutput("addedPaperRegression", width = '100%'),
                      br(),
                      actionLink("addAnotherMetaRegressionPaper",
                                 "Click to add another study for meta-regression"),
                      br(),
                      br(),
                      actionButton("reMetaRegression", "Add study to meta-regression", class="btn-success"),
                      # TODO: reset/deselect button;
                      #actionLink("resetMetaAnalysisPaper", "Reset these papers"),
                      br(),
                  ),
              )
            ),
            shinyjs::hidden(
                div(
                  id = "regressionForm",
                  box(title = "Add your study for Meta-regression", width =  12, side = "right",
                      fluidRow(
                        # Paper Metadata
                        column(9, h4('Study Metadata')),
                      ),

                      fluidRow(column(8,textInput("regressionTitle", labelMandatory(label="Title")
                      ))),

                      fluidRow(
                        column(6,textInput(inputId ="regressionAuthor", labelMandatory(label="Authors")
                        )),
                      ),
                      fluidRow(column(6,
                               selectizeInput("regressionYear",
                                    labelMandatory("Year the study was conducted"),
                                    choices = c('', 2021:1900),
                                    options = list(
                                      placeholder = 'Please select a year below',
                                      onInitialize = I('function() { this.setValue(""); }')
                            )
                          )
                        ),),

                      # Effect Size

                      fluidRow(
                        column(4, numericInput("regressionEffectSize", labelMandatory(label="Logit-transformed cooperation"), value=NULL)),
                        column(4, numericInput("regressionVariance", labelMandatory(label="Variance"), value=NULL)),
                        column(4, numericInput("regressionSampleSize", labelMandatory(label="Sample size"), value=NULL)),
                      ),

                      downloadLink('download_compute_logit', 'Use our calculation sheet to compute logit-transformed cooperation and variance.'),

                      # Treatment1
                      br(),
                      h4("Treatment 1"),
                      br(),
                      uiOutput("regressionTreatment1"),


                      # Moderators
                      br(),
                      h4("Moderators"),
                      br(),
                      uiOutput("regressionModerators"),

                      br(),
                      br(),
                      actionButton("addRegressionPaper", "Add study", class="btn-primary"),
                      br(),

                  )
                )
            ),

            #### resultsBox ####
            div( id="resultBoxReg",
                 tabBox(title="Meta-Regression Results", width =  12, side = "right", # with side=right , tab order is reverted
                        selected = 1,
                        #### Data Table ####
                        tabPanel("Data Table",
                                 p("This table displays the studies, effect sizes, and moderator values included in the meta-analysis. 
                             The information on this table differs from the table in the “data overview” section, because that 
                             table includes studies coded for examining the same topic, but for which effect sizes were not computed."),
                                 DT::dataTableOutput("metatable_reg")
                        ),
                        #### Visualisation ####
                        tabPanel("Visualization",
                                 selectInput( inputId = "vis_moderator_variables_reg",
                                              label = "Choose one moderator:", choices = NULL, multiple = FALSE
                                 ),
                                 div(radioButtons("visualmoderator_reg", "Show studies by", choices =
                                                    c("Forest Plot" = "forest","Violin Plot" = "violin")
                                                  , selected = "forest", inline = TRUE), align="center"),
                                 conditionalPanel("input.visualmoderator_reg == 'violin'",
                                                  p(class = "text-muted",  "This is a violin plot which plots the log-transformed proportion of cooperation, 
                                              including the probability density of the effect sizes at different values, 
                                              which is smoothed by a kernel density estimator. The violin plot includes 
                                              a marker for the median and a marker for the interquartile range."),
                                                  p(class = "text-muted", "See:", a("https://en.wikipedia.org/wiki/Violin_plot")),
                                                  plotlyOutput( "get_violin_reg" )
                                 ),
                                 conditionalPanel("input.visualmoderator_reg == 'forest'",
                                                  p(class = "text-muted",
                                                    "This is a forest plot that displays the log-transformed proportion 
                                                    of cooperation in the selected treatments, including the overall result of the meta-regression."
                                                  ),
                                                  p("See:", a("https://en.wikipedia.org/wiki/Forest_plot")),
                                                  uiOutput("forests_reg_ui")
                                 )
                        ),
                        #### Meta-Regression ####
                        tabPanel("Meta-regression", value = 1,
                                 
                                 column(12,
                                        uiOutput("interpretation_reg")
                                 ),
                                 withSpinner(DT::dataTableOutput("reg")),
                                 br(),
                                 h3("Raw output"),
                                 verbatimTextOutput("regSummary")
                                 
                        ) # endtab
                 ),
                 box(width=12, status="warning" , title = p(class="customtitle","Download data"), solidHeader = F,
                     valueBox(width = 4,value =   downloadLink('downloadMetaData_reg', icon("download")), subtitle= "Selected dataset (.csv)" , icon = icon("table"), color="lime")
                 )
            ) # end div
    ),
    #### Citation Analysis ####
    tabItem("citations",
            h4(strong("Analyze citations")),
            p( "The network below shows papers (nodes) and their citations (links). A link between nodes represents a citation. Larger nodes are more highly cited papers."),
            p( "Communities in the network are identified based on their modularity, i.e., the density of the links intra- and inter-communities, using the Louvain  community detection algorithm (Blondel et al., 2008)."),
            p( "Users can choose to display the citation network for specific range of years, papers with a specified number of citations, and color the nodes according to several criteria (e.g., community, game type).",),
            p("The citation network can take a few seconds to load."),
            p(class = "text-muted", "Blondel, V. D., Guillaume, J.-L., Lambiotte, R., & Lefebvre, E. (2008). Fast unfolding of communities in large networks. Journal of Statistical Mechanics: Theory and Experiment, 10, P10008."),

            actionButton("citations_info_message", "Further info & tips"),
            br(),br(),
            
            selectInput(inputId = "citation_color_selector",   label = "Color network by:", choices =""),
            sliderInput("citation_year", "Paper Year", dragRange= T, width = '80%',
                        1950, 2021,  value = c(1950, 2021), sep = ""),
            sliderInput("citation_number", "Citation number (cited by how many papers)", dragRange= T, width = '80%',
                        0, 300,  value = c(0, 300), sep = ""),
            checkboxInput("remove_edges", label = "remove edges", value = F),
            
            br(),
            visNetworkOutput("citations_viz", height = "1000px")
    ), # end tab citations
    tabItem("ontology",
            
            box ( 
              width=12, status = "warning", title=p( class="customtitle","Ontology overview"), solidHeader = F,
              h4(strong("Explore the ontology of Independent Variables")),
              p("The image  shows all the Independent Variables (IV) that were used to annotate studies and their treatments. The size of the bubbles indicate how many treatment were annotated with the IV. Click on a chosen circle to zoom in and see the subclasses, and click on the external circles to zoom out. For a more specific description of the IVs, you can refer to the table below."),
              d3Output("d3" , width = "100%", height = "800px")
            ),
            box(
              width=12, status = "warning", title=p( class="customtitle","Independent Variables details"), solidHeader = F,
              
              p(class="text-muted","Below a description of the independent variables, their moderators, and their possible values. For further details, see also ", a("https://data.cooperationdatabank.org/"), "."),
              DT::dataTableOutput("infotable")
            )  
    ),

    # Long InputForm: Allow users to add studies
    # Todo: When imputing a study, annotators can have access to  a FAQ sheet for responses to common questions
    # Todo: publication status and language
    tabItem("addPaper",
            box(width=12, status = "warning", title=p( class="customtitle","Add Study"), solidHeader = F,
              p("You can use this form to manually add studies to the Cooperation Databank,
              including their effect sizes and sample/study characteristics.
              These studies can be published or unpublished.
              At the moment, the Cooperation Databank only includes studies that observe cooperation
              in a Prisoner’s Dilemma, Public Goods Dilemma, and Resource Dilemma.
              The submitted information about each study will be reviewed by an Editorial Board and
              subsequently added to the Databank."),
            ),
              shinyjs::inlineCSS(appCSS),
              div(
                id = "form",
                box(width=12,
                    status = "primary",
                    title="Add Study Metadata", solidHeader = F, collapsible = TRUE,
                # Meta data of paper
                #br(),
                #h4('Add Study Metadata'),
                #br(),
                fluidRow(column(8,textInput("addTitle",
                                            label = p(labelMandatory("Title"), br(),
                                                      helpText('Example: Altruistic punishment in humans'))
                                            ))),

                fluidRow(

                  column(6,textInput(inputId ="addAuthors",
                  label = p(labelMandatory("Authors"), br(),
                            helpText('Example: Fehr, Gächter'))
                  )),
                ),
                fluidRow(
                  column(6,
                         selectizeInput("addYearConducted",
                                    labelMandatory("Year the study was conducted"),
                            choices = c('',2021:1900),
                            options = list(
                              placeholder = 'Please select a year below',
                              onInitialize = I('function() { this.setValue(""); }')
                            )
                          )
                  ),
                ),
                fluidRow(
                  column(4,
                         selectizeInput(inputId = "addLanguage",
                         label = labelMandatory("Language"),# by which to filter observations from their papers:", #label displayed in ui
                         choices = c('', ISO_639_2$Name),
                         options = list(placeholder = 'Select language')),
                  ),
                ),

                fluidRow(
                  column(4,
                            selectInput("addPublicationStatus", label = p("Publication status"),
                            choices = c('',"Published Article","Doctoral Dissertation","Working paper","Master’s thesis","Raw data")),

                  )
                ),
                fluidRow(
                  column(8,
                    textAreaInput("addAbstract", "Abstract", rows = 10,
                                  placeholder = "Abstract about the sample and method of the study (250 words max).")
                  ),
                ),
                ),


                box(width=12,
                    status = "primary",
                    title="Add Treatments", solidHeader = F, collapsible = TRUE,
                  # Treatment
                #br(),
                #h4('Treatments'),
                #br(),

                fluidRow(column(11,
                p(strong("Defining Treatments:"), br(), br(),
"Each study can include one or more effect sizes. To allow flexibility to compute effect sizes, we request that you specify treatments in your study (i.e., the manipulated or measured variables used to predict cooperation).",
br(), br(), strong("A treatment"), "can be characterized by:", br(),
HTML('&emsp;'), "(a) one or more variables that takes different levels (or values) within a study (e.g., gender: male; group size: 4).", br(),
HTML('&emsp;'), "(b) a single variable correlated with cooperation (e.g., age, personality trait, or expectations of partner cooperation)", br(),br(),
"Below, we request that you specify the different treatments in your study that can be used as input to create effect sizes. We recommend watching this tutorial for further instructions and relevant examples.",
br(),br(),"For example, a study could manipulate both (1) the presence and absence of punishment and (2) the presence and absence of communication (e.g., a 2 x 2 between subjects design). This would create four treatments that can be specified below. If the study also included a measure of a personality variable that was related to cooperation, then this would be defined as an additional treatment.",
br(),br(),"Below, you can add and define treatments by selecting one or more variables that specify the treatment, e.g., a punishment treatment and communication treatment. When you select a specific", em("Generic Independent Variable"), "that specifies the treatment, you will be asked to assign values to several more", em("Specific Independent Variables."),
br(),br(),"For each treatment you can report the requested statistics, including a sample size, mean, standard deviation, Proportion of Cooperation, the Lowest choice, the Highest choice, Between Subjects vs. Within Subjects design, Pearson’s correlation.",
br(),br(),)
                )),
                helpText(strong('Note:'), 'If you do not find a general or specific IV among the listed ones,
                you can still annotate the treatments by selecting “Add description” in the list and describe
                what variables were manipulated or measured.', br(),
                'The CoDa editorial board will review this content and either re-annotate it according to the
                 current codebook or add this variable to the list of the new ones to be implemented in the future.'),
                    h4('Add Treatment1'),
                fluidRow(
                  column(6, selectInput(inputId = "addGenIVselectionTreatment1",
                            label = p(labelMandatory("Generic Independent Variable"), br(),
                            helpText('Example: Punishment', br(),
                            em('Check our codebook for the list of all the Generic Independent Variables and their definitions')),
                            ),

                             choices = ""),),
                ),
                fluidRow(column(6,
                                checkboxInput("addDescriptionGenericIVTreatment1",
                                              "Add description for Generic Independent Variable")
                                )
                ),
                conditionalPanel(
                                  condition = 'input.addDescriptionGenericIVTreatment1 == 1',
                                  fluidRow(column(8, textAreaInput("addDescriptionGenericIVTextTreatment1",
                                                          "Description for Generic Independent Variable"))),
                                   ),
                conditionalPanel(
                                  condition = 'input.addGenIVselectionTreatment1 != ""',
                  fluidRow(
                    column(6, selectInput(inputId = "addTreatmentSubpropSelectionTreatment1", #name of input used to be "gen_iv", also removed selectInput for current_iv
                               label = p("Specific Independent Variable",
                                         helpText("Example: Punishment treatment", br(),
                                                  em("Check our codebook for the list of all the Specific Independent Variables and their definitions"))
                               ),
                               choices = "", selected = ""),),
                    ),
                  fluidRow(column(6,
                                checkboxInput("addDescriptionSpecificIVTreatment1",
                                              "Add description for Specific Independent Variable")
                                )
                  ),
                  conditionalPanel(
                                  condition = 'input.addDescriptionSpecificIVTreatment1 == 1',
                                  fluidRow(column(8, textAreaInput("addDescriptionSpecificIVTextTreatment1",
                                                          "Description for Specific Independent Variable"))),
                                   ),
                     fluidRow(
                       column(6, selectInput(inputId = "addValueOptionsSelectionTreatment1",
                               label = p("Specific Independent Variable values", br(),
                               helpText("The possible values of a treatment.")
                               ), #label displayed in ui
                               choices = "", selected = ""),),
                    ),
                ),

                fluidRow(column(8,
                radioButtons('quantitativeMethodTreatment1',
                             "Please provide quantitative variables (either a or b) ",
                              choices = c("a: Provide correlation and sample size" = "a", "b: Provide Mean, Standard Deviation, and sample size" = "b"),
                             selected = character(0)
                )
                )),

                conditionalPanel(
                                  condition = 'input.quantitativeMethodTreatment1 == "a"',
                #fluidRow(column(
                #4, selectInput('addEsMeasure0', 'Choose an effect measure:',
                #                 choices =c('',"Standardised Mean Difference" = "d", "Raw correlation coefficient" = "r"))
                #)),
                fluidRow(column(
                4, numericInput('addCorrelationTreatment1', labelMandatory('Correlation'), NULL)
                )),
                                  fluidRow(column(
                4, numericInput('addSampleSizeATreatment1', labelMandatory('Sample size'), NULL)
                )),

                ),

                conditionalPanel(
                                  condition = 'input.quantitativeMethodTreatment1 == "b"',

                    fluidRow(column(
                    4, selectInput('DVBehaviorTreatment1', labelMandatory('Behavior (dependent variable)'),
                      choices = c('', 'Contributions', 'Cooperation', 'Withdrawals')
                      )
                    )),
                    conditionalPanel(
                                  condition = 'input.DVBehaviorTreatment1 == "Contributions" || input.DVBehaviorTreatment1 =="Withdrawals"',

                    fluidRow(column(
                    4, numericInput('addMeanTreatment1', labelMandatory('Mean'), NULL)
                    )),
                    fluidRow(column(
                    4, numericInput('addStandardDeviationTreatment1', labelMandatory('Standard Deviation'), NULL)
                    )),
                    ),

                    conditionalPanel(
                                  condition = 'input.DVBehaviorTreatment1 == "Cooperation" ',
                                        fluidRow(column(
                    4, numericInput('addProportionOfCooperationTreatment1', labelMandatory('P(C) (proportion of cooperation'), NULL)
                    )),

                    ),

                    fluidRow(column(
                    4, numericInput('addSampleSizeBTreatment1', labelMandatory('Sample size'), NULL)
                    )),

                    #fluidRow(column(
                    #4, numericInput('addLowestChoiceTreatment1', 'Lowest Choice', NULL)
                    #)),
                    #fluidRow(column(
                    #4, numericInput('addHighestChoiceTreatment1', 'Highest Choice', NULL)
                    #)),
                    fluidRow(column(
                    4, selectInput('betweenOrWithinSubjectsTreatment1', labelMandatory('Between or Within Subjects'),
                                     choices = c('', "Between", "Within", "Mixed"),
                      )
                    )),
                ),



                      actionLink('addAnotherVariable', "Add another treatment for this variable"), br(),
                      shinyjs::hidden(
                      div(id = 'removeVariable',
                                      actionLink('removeVariableLink', "Remove this treatment", style="color: red"), br(),

                      )),


                      actionLink('addAnotherTreatment', "Add another variable"), br(),
                      shinyjs::hidden(
                      div(id = 'removeTreatment',
                                      actionLink('removeTreatmentLink', "Remove this variable", style="color: red"), br(),

                      ))


                ),

            box(width=12,
                status = "primary",
                title="Add Sample Characteristics", solidHeader = F, collapsible = TRUE,

                fluidRow(column(11, strong(
                "Please report other relevant characteristics of the study (e.g., related to the sample or the game structure). This information will assist the editors in evaluating your study, and can be used for search criteria and as variables in meta-analyses (e.g., moderator variables)."
                ))),
                # Sample Character
                br(),

                helpText(em("Check our codebook for the list of all the Sample Characteristics, their values, and their definitions")),
                #br(),

                fluidRow(column(4, selectizeInput("addYearOfDataCollection", "Year of data collection ",
                                        choices = c('',2021:1900),
                                        options = list(
                                        placeholder = 'Please select a year below',
                                        onInitialize = I('function() { this.setValue(""); }')
                                )
                        )),
                ),
                fluidRow(
                  column(4,
                             conditionalPanel(
                                  condition = 'input.addYearOfDataCollection != ""',
                                  selectInput("addYearSource", label = p("Source of year of data collection", br(),
                                                                       helpText('Source of information about what year the data was collected.')),
                                            choices = c('', "Received/Submitted", "Conducted","Published",
                                                        "Accepted","Presented", "Working paper published", "Available online"),

                                  ),
                             ),
                  )
                ),

                fluidRow(column(4, selectizeInput("addCountries", "Country/region",
                                        label = p("Country/Region", br(),
                                                  helpText("Country or region where the data collection took place.")),
                            choices = (codelist[["iso3c"]]), multiple= TRUE,
                                        options = list(
                                        placeholder = 'Please select a country/region below',
                                        onInitialize = I('function() { this.setValue(""); }')
                                )
                        )),
                ),
                fluidRow(
                  column(4,
                             conditionalPanel(
                                  condition = 'input.addCountries != ""',
                                  selectInput("addCountrySource", p("Source of country/region", br(),
                                                                    helpText("Source of the country where the data collection took place.")),
                            choices = c("Specified country", "Most authors","All authors","Multiple countries"),
                                                 selected = "All authors"),

                                  ),
                  )
                ),

                # todo: https://github.com/rstudio/shiny/issues/927
                fluidRow(
                  column(6, numericInput("addSampleSize", "Sample size",
                                                   NULL, 0, 3000
                  ),),
                ),

                fluidRow(
                  column(6, numericInput("addMaleProportion", "Proportion of males in study",
                                                    NULL, 0, 1.0
                  ),),
                ),

                fluidRow(
                  column(4, numericInput("addMeanAge", "Mean age",
                            NULL,0, 100),),
                  column(4, numericInput("addAgeHigh", "Highest age",
                            NULL,0, 100),),
                  column(4, numericInput("addAgeLow", "Lowest age",
                            NULL,0, 100),),

                ),

                fluidRow(column(4, radioButtons("addStudyStudentSample",
                           label = p("Student sample only", br(),
                           helpText("Whether participants were recruited from a student population. Yes = Participants were recruited from a student population, No = Participants were not recruited from a student population.")

                           ),
                             choices = c("Yes" = TRUE , "No" = FALSE),
                              selected = character(0), inline = TRUE))),


                fluidRow(column(4,
                                conditionalPanel(
                                  condition = 'input.addStudyStudentSample == "TRUE"',
                                  selectInput("addStudyAcademicDiscipline",  p("Academic discipline", br(),
                                  helpText("Participant's field of study if the participant is a student. Multiple choices are allowed.")
                                  ),
                                      choices = c('',"Economics" = "1", "Psychology" = "2", "Sociology" = "3", "Mixed" = "4", "Other" = "5") ),
                                ),

                )),
                fluidRow(column(4,
                  selectInput("addRecruitmentMethod", p("Recruitment method", br(),
                  helpText("The way participants were recruited to take part in the study.")
                  ),
                            choices = c('',"Participant pool" = "1", "Mechanical Turk" = "2", "Other" = "3") ),

                )),),

                box(width=12,
                    status = "primary",
                    title="Add Study Characteristics", solidHeader = F, collapsible = TRUE,
                  # Study Characters
                #br(),
                #h4("Study Characteristics"),
                helpText(em("Check our codebook for the list of all the Study Characteristics variables, their values, and their definitions")),
                br(),
                fluidRow(column(4,
                  selectInput("addStudyExperimentalSetting",p("Experimental setting", br(),
                  helpText("The setting in which the experiment was conducted. Multiple choices are allowed.")
                  ), multiple = TRUE,
                            choices = c("Online" = "Online", "Lab" = "Lab", "Classroom" = "Class", "Field" = "Field", "Lab in the field" = "Lab in the field", "Natural experiment" = "Natural experiment", "Other" = "Other") ),

                ),
                ),

                fluidRow(
                  column(4,
                  selectInput("addStudyDilemmaType",p(labelMandatory("Game type"), br(),
                  helpText("Economic game that participants play during the experimental session.")
                  ),
                            choices = c('',"Public Goods Game","Prisoner's Dilemma Game", "Resource Dilemma","Other" )
                            ),
                )
                ),


                fluidRow(column(6,
                  conditionalPanel(
                    condition = 'input.addStudyDilemmaType == "Public Goods Game"',
                    radioButtons("addStudyContinuousPGG", p("Continuous vs. step-level public goods", br(),
                    helpText("Whether there was a provision point for contributions to establish a public good (i.e., step-level public goods) versus each contribution provided an incremental benefit to the public good (i.e., continuous public good).")
                    ),
                             choices = c("Continuous", "Step-level", 'All')
                                 , selected = character(0), inline = TRUE),
                  ),

                )),

                fluidRow(column(4,
                            radioButtons("addStudySymmetric", label = p("Symmetric", br(),
                            helpText("Whether specific aspects of the game that were different (i.e., asymmetric) for participants. Yes = Symmetric, No = Asymmetric.")
                            ),
                             choices = c("Yes" = TRUE, "No" = FALSE, "All" = "All"),  selected = character(0), inline = T),
                          ),
                ),

                fluidRow(
                  column(4, radioButtons("addStudyOneShot", p("One-shot vs. repeated", br(),
                  helpText("Whether participants played the game with the same person only once (this also applies if participants switch partners after each trial) versus played the game repeatedly with the same person.")
                  ),
                             choices = c("One-shot" , "Repeated" ,"All" = "All")
                             , selected = character(0), inline = TRUE),),
                ),
                fluidRow(
                  column(4,
                            conditionalPanel(
                    condition = 'input.addStudyOneShot == "One-shot"',radioButtons("addStudyOneShotRepeated", p("Repeated one-shot game" , br(),
                              helpText("Whether participants are paired with different partner(s) after each trial across many trials. TRUE = repeated one-shot, FALSE = one-shot.")
                              ),
                             choices = c("Yes" = TRUE, "No" = FALSE, "All" = "All")
                             , selected = character(0), inline = TRUE),))
                ),

                fluidRow(column(4, radioButtons("addStudyMatchingProtocol", label  = p("Matching", br(),
                helpText("The way participants are paired with others during interactions.")
                ),
                             choices = c("Stranger", "Partner", "All"), selected = character(0), inline = T ),
                                ),
                ),

                fluidRow(
                  column(4, radioButtons("addStudyKnownEndgame", label = p("Known endgame", br(),
                                        helpText("Whether participants know the exact number of trials at the beginning of the experiment. Endgame is considered known also when participants played a one-shot game. Yes = Known endgame, No = Endgame not known.")
                                        ),
                             choices = c("Yes" = TRUE, "No" = FALSE, "All" = "All"),  selected = character(0), inline = T),
                  ),
                ),

                fluidRow(
                  column(4, selectInput("addStudyShowUpFee", label = p("Show-up fee", br(),
                                                                       helpText("How participants were compensated for participation.")),
                            choices = c('', "Paid","Course credit", "Non-monetary","Absent" ), multiple = TRUE,
                            ),),

                ),
                fluidRow(
                  column(4, selectInput("addStudyGameIncentive",
                                        label = p("Game incentive", br(),
                                                helpText("Whether participants' decisions in the game determine their payoffs in different forms.")),
                            choices = c('',"Hypothetical", "Monetary","Non-monetary","Monetary lottery", "Non-monetary lottery" ), multiple = TRUE
                            ),
                        )
                ),

                fluidRow(
                  column(4,
                      textInput("addStudyGroupSize",
                                   p("Group size", br(),
                                     helpText("Overall number of people affected by the choices in the game.
                                     If you want to input multiple values, please enter values separated by a comma.")),
                                ),
                  )
                ),

                fluidRow(
                 column(6, conditionalPanel(
                                  condition = 'input.addStudyDilemmaType == "Prisoner\'s Dilemma Game"',
                                  textInput("addStudyKindex",
                                                p("K index", br(),
                                                helpText("For 2-persons PDG, it is calculated as (R-P)/(T-S), and 0 < K < 1.
                                                For N-persons PDG, it is calculated as (A(n)-B(0))/(0(max)-0(min)), and 0 < K < 1.
                                                If you want to input multiple values, enter values separated by a comma."
                                                )
                                                ),),
                                  )
                 ),
                ),
                fluidRow(
                     column(6, conditionalPanel(
                      condition = 'input.addStudyDilemmaType == "Public Goods Game"',
                      textInput("addStudyMPCR",
                                   p("MPCR",br(),
                                   helpText("The ratio of benefits to costs for a member to contribute one monetary unit to the group account.
                                   Calculated as multiplier divided by group size.
                                   If you want to input multiple values, enter values separated by a comma.")
                                   ),
                      ),
                      )
                 ),
                ),

                fluidRow(
                  column(6, conditionalPanel(
                      condition = 'input.addStudyContinuousPGG=="Step-level"',
                         textInput("addStudyPGDThreshold",
                                      p("Threshold", br(),
                                      helpText("The minimum threshold of total contribution by all group
                                      members when the public good will be provided.
                                      This also defines the criticality of contributions, as contributions
                                      are more critical if other players cannot reach the threshold on their own
                                      (e.g. in sequential games where others’ prior contributions are known).
                                      If you want to input multiple values, enter values separated by a comma.")
                                      ),
                            ), )
                  )
                ),

                fluidRow(
                 column(6, conditionalPanel(
                      condition = 'input.addStudyDilemmaType == "Resource Dilemma"',
                      textInput("addReplenishmentRate",
                                   p("Replenishment rate", br(),
                                     helpText("The replenishment rate in the resource dilemma.
                                     The remaining resource in the common pool is multiplied by the replenishment
                                     rate after each trial. If you want to input multiple values, enter values separated by a comma.")),
                      ),
                                            )
                 )
                ),

                fluidRow(
                  column(4,
                            selectInput("addDiscussion",
                                        p("Discussion", br(),
                                          helpText("Whether communication was allowed between participants in the game.")
                                        ),
                            choices = c('',"Uni-directional","Absent","Bi-directional"), multiple = TRUE ),
                  ),
                ),

                fluidRow(
                  column(4,
                            selectInput("addParticipantDecision",
                                        label =  p("Sequentiality decision", br(),
                                                   helpText("Whether group members make their decisions simultaneously or sequentially, i.e., whether participants take decisions one after another while receiving some form of feedback about preceding decisions.")),
                            choices = c('',"Simultaneous","Sequential turn-taking","Sequential leadership-by-example" ), multiple = TRUE
                            ),
                  )
                ),

                fluidRow(
                  column(4,
                            textInput("addChoiceLow",
                                         p(labelMandatory("Lowest choice option"),
                                          br(), helpText("Lowest choice option allowed to participants.
                                          The value of the lowest choice option.
                                          Is '0' in a binary choice between non-numeric options
                                          (such as 'cooperate' vs. 'defect'; 'C' vs. 'D').
                                          If you want to input multiple values, enter values separated by a comma.")),
                            ),
                  ),

                  column(4,
                            textInput("addChoiceHigh",
                                         p(labelMandatory("Highest choice option"),br(),
                                           helpText("Highest choice option allowed to participants.
                                           The value of the highest choice option.
                                           Is '1' in a binary choice between non-numeric options
                                           (such as 'cooperate' vs. 'defect'; 'C' vs. 'D').
                                           If you want to input multiple values, enter values separated by a comma.")),
                            ),
                  ),
                ),

                fluidRow(
                  column(4,
                            textInput("addStudyNumberOfChoices",
                                         p(labelMandatory("Number of choice options"), br(),
                                           helpText("The number of choice options players have when making the contribution.
                                           Is '2' when making a binary choice (such as cooperate vs. defect);
                                           (n+1) for continuous measure of cooperation; 3 for 3-choice Prisoner's Dilemma, etc.
                                           If you want to input multiple values, enter values separated by a comma.")
                                         ),
                            ),
                  )
                ),

                fluidRow(
                  column(4,
                             radioButtons("addDeception",
                                          label = p("Deception", br(),
                                          helpText("Whether participants believed that they were playing with real others.")
                                          ),
                             choices = c("Yes" = TRUE, "No" = FALSE, 'All'='All'),  selected = character(0), inline = T),
                  ),
                ),

                fluidRow(
                  column(4,
                            selectInput("addStudyRealPartner",
                                        label =p("Real Partner", br(),
                                                 helpText("Whether participants interact with real or imagined participants in the game.")),
                            choices = c('', "Real","Hypothetical","Deception"), multiple = TRUE
                            ),
                  ),
                ),

                fluidRow(
                  column(4,
                            selectInput(inputId = "addStudyAcquaintance",
                                        label = p("Acquaintance", br(),
                                                 helpText("Whether participants were interacting (and were aware of it) with people that they are not acquainted with or with people that they knew outside of the laboratory (e.g., friends, relatives, parents, romantic partners).") ) ,
                            choices = NULL, multiple= TRUE),
                  ),
                ),

                fluidRow(
                  column(4,
                             radioButtons("addSanction",
                                          label = p("Sanction", br(),
                                                    helpText("Whether a sanction (punishment or reward) mechanism was in place in the game. Yes = sanction was present, No = sanction was absent.")),
                             choices = c("Yes" = TRUE, "No" = FALSE, 'All'='All'),  selected = character(0), inline = T),
                  ),
                ),
                    ),

                box(width=12,
                    status = "primary",
                    title="Add Overall cooperation", solidHeader = F, collapsible = TRUE,
                #br(),
                #h4('Overall cooperation'),
                #br(),
                # Quantitave study results

                fluidRow(
                  column(4, conditionalPanel(
                      condition = 'input.addStudyNumberOfChoices==2',
                            numericInput("addOverallProportionCooperation",
                                         p("Overall proportion of cooperation", br(),
                                           helpText("Cooperative behavior for games (e.g. prisoner's dilemma and public goods game) with two choice options. Higher numbers equal higher cooperation.")),
                            NULL,0, 10),)
                  ),
                ),

                conditionalPanel(
                      condition = 'input.addStudyNumberOfChoices > 2',
                      fluidRow(
                        column(4,
                                  numericInput("addOverallMeanContributions",
                                               p("Overall mean of contributions", br(),
                                                 helpText("Cooperative behavior when the game (e.g., prisoner's dilemma and public goods dilemma) had a maximum number of choices greater than two. Higher numbers equate to higher cooperation.")),
                                  NULL,0, 15),
                        ),
                      ),
                        fluidRow(
                          column(4,
                                  numericInput("addOverallMeanWithdrawal",
                                               p("Overall mean of withdrawals",br(),
                                                 helpText("Withdrawal behavior when the game type is a resource dilemma and the maximum choice range is about two. Higher numbers equate to lower cooperation.")),
                                  NULL,0, 10),
                          ),
                        ),

                      fluidRow(
                        column(4,
                                  numericInput("addOverallStandardDeviation",
                                               p("Overall standard deviation of contributions or withdrawals", br(),
                                                 helpText("Standard deviation of contributions or withdrawals, only applicable when maximum number of choices is greater than two.")),
                                  NULL, 0, 10),
                        ),
                      ),
                      fluidRow(
                        column(4,
                                  numericInput("addOverallPercentageEndowmentContributed",
                                               p("Percentage of endowment contributed", br(),
                                                 helpText("Percentage of endowment contributed, calculated as (M - Lower choice option) / (Upper choice option – Lower choice option).")),
                                  NULL, 0, 1)   ,
                        )
                      ),
                ),

                conditionalPanel(
                      condition = '!!input.addOverallProportionCooperation || !!input.addOverallMeanContributions || !!input.addOverallMeanWithdrawal || !!input.addOverallPercentageEndowmentContributed',
                      fluidRow(
                        column(4,
                                  selectInput("addStudyTrialOfCooperation",
                                              label = p("Trial of cooperation", br(),
                                                        helpText("Trial on which the Dependent Variable was assessed.")) ,
                                  choices = c('',"All trials" , "First trial", "First and last trials", "Last trial", "Other trials")),
                        )
                      ),
                ),

                fluidRow(
                  column(6,
                            numericInput("addNumberOfObservations",p("Number of observations", br(),
                            helpText("Use this variable as sample size to compute variance for meta-regressions predicting cooperation.
                            It corresponds to the total sample size in a single study after exclusion of participants (N),
                            but it uses the number of observations for studies that uses both individual and groups as unit of analysis
                            (e.g., interindividual-intergroup discontinuity studies).")
                            ),
                            NULL, 0, 2500),
                  )
                ), ),

                # todo: language selection choices, treatment choices

                box(width=12,
                    status = "warning",
                    title="Submit your study", solidHeader = F, collapsible = F,


                    #fluidRow(column(8, radioButtons(
                    #  'askUserEmail',
                    #  p('Can we have your name and email? Because we might get back for additional questions if something was not input correctly.'),
                    #  choices = c("Yes, let's keep in touch!" = 'y', "No, thanks!"='n'),
                    #  selected = character(0)
                    #))),
                    #conditionalPanel(
                    #              condition = 'input.askUserEmail == "y"',
                    fluidRow(column(6, textInput("userName",labelMandatory("Your name:")))),

                    fluidRow(column(6, textInput("userEmail",labelMandatory("Email Address:")))),
                    #),
                    fluidRow(column(8,
                      checkboxInput('privacy', div("I agree with the ",
                                                   actionLink('showTerms',
                                                              labelMandatory("terms and conditions"))))
                    )
                    ),
                    actionButton("submit", "Submit", class = "btn-primary"),
                # Show error message if there are errors
                shinyjs::hidden(
                  span(id = "submit_msg", "Submitting..."),
                  div(id = "error",
                      div(br(), tags$b("Error: "), span(id = "error_msg"))
                  )
                ) ),
              ),


            # Show thank you message after a successful submission.
              shinyjs::hidden(
                div(
                    id = "thankyou_msg",
                    box(width=12,
                      status = "success",
                      title="Thank you!", solidHeader = T, collapsible = F,
                      h3("Thanks, your study was submitted successfully!"),
                      actionLink("submit_another", "Submit another study")
                    )
                )
              )

    )
    
  )
)

side <- dashboardSidebar(
  #width = 350,
  sidebarMenu(
    menuItem("Data overview",tabName = "overview", icon = icon("bar-chart")),
    menuItem("Meta-analyses",tabName = "analyses", icon = icon("sliders")),
    menuItem("Meta-regression",tabName = "metaregression", icon = icon("line-chart")),
    menuItem("Citation explorer",tabName = "citations", icon = icon("share-alt")),
    menuItem("Ontology explorer",tabName = "ontology", icon = icon("spinner")),
#    menuItem("Add Study",tabName = "addPaper", icon = icon("plus")),
    menuItem("Tutorials", href = "https://cooperationdatabank.org/tutorials", newtab = TRUE, icon = icon("graduation-cap")),
    menuItem("Website", href = "https://cooperationdatabank.org", newtab = TRUE, icon = icon("home"))
  ),
  br(),
  tags$a(
    href = "https://cooperationdatabank.org/contact/", # Link to open
    target = "_blank", # Open in new window
    valueBox("Feedback?", subtitle = tags$p("Please let us know.", style = "font-size: 100%;"),
             icon = icon("comment-alt"), color = "yellow", width = NULL)
  )
)


dashboardPage(
  skin= "blue",
  header,
  side, #dashboardSidebar(disable = F),
  body
)
