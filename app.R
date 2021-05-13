#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# inspo for multitab layout: https://vnijs.shinyapps.io/radiant/?_ga=2.55818795.1255273247.1609787905-1921353160.1609208896&SSUID=000c2676af

# tufts heart failure risk ex: https://www.tuftsmedicalcenter.org/patient-care-services/departments-and-services/cardiovascular-center/heart-failure-calculator

library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(DT)
library(scales)
library(dplyr)
source("./model-functions.R")

# TODO get relevant images for footer / do the footer
ui <- fluidPage(
    theme = shinytheme("flatly"),
    shinyjs::useShinyjs(),
    # shinythemes::themeSelector(),
    
    tags$head(tags$style(
        HTML("
         .shiny-output-error-validation {
         color: red;
         }
         ")
    )),
    navbarPage(
        "Hepatocellular carcinoma recurrence multistate model",
        
        tabPanel(
            title = "Calculator",
            
            sidebarPanel(
                width = 4,
                h4(strong("Variable Selection")),
                br(),
                selectInput(
                    "modelType",
                    "Model Type",
                    c(
                        Preoperative = "preop",
                        Postoperative = "postop"
                    )
                ),
                radioButtons("sex", "Sex",
                             c(Male = "Male", Female = "Female"),
                             selected = "Female"),
                numericInput("age", "Age", min = 18, value = 18),
                
                #postoperative model covariates
                conditionalPanel(
                    condition = "input.modelType == 'postop'",
                    
                    selectInput(
                        "state",
                        "Disease State",
                        c(
                            'No recurrence' = "Surgery",
                            '1st Local Recurrence' = "First local recurrence",
                            '2nd Local Recurrence' = "Second local recurrence"
                        )
                    ),
                    h5(strong("Pathology")),
                    checkboxInput("multipleTumours", "Multiple tumours"),
                    checkboxInput("largestTumour", "Largest tumour size  \u22655cm"),
                    checkboxInput("satellite", "Satellite lesions"),
                    checkboxInput("microvascular", "Microvascular invasion")
                ),
                actionButton("makePlot", "Generate Plot")
            ),
            
            mainPanel(
                h1("Hepatocellular Carcinoma Risk Probabilities"),
                h5(uiOutput("plotExplanation")),
                br(),
                
                # titlePanel(textOutput("titlePanel")),
                mainPanel(
                    
                    tabsetPanel(
                        
                        tabPanel(title = "Monthly",
                                 br(),
                                 helpText("Hover over the curves in the figure for monthly risk predictions 
                              per disease state"),
                                 plotlyOutput("probPlot", width = "800px", height = "600px")        
                        ),
                        tabPanel(title = "Yearly",
                                 br(),
                                 helpText("Hover over the curves in the figure for yearly risk predictions 
                              per disease state"),
                                 plotlyOutput("probPlotYearly", width = "800px", height = "600px")
                        )
                        
                    ),
                    br(),
                )
            )
        ),
        
        tabPanel(
            title = "About the Model",
            h2("Statistical Analysis of Results"),
            br(),
            p(
                "Multistate modeling of HCC recurrence can be used to account for the
        various disease states a patient can exist in and transition between after
        curative-intent LR. In contrast to standard single time-to-event estimate,
        multistate modeling provides more realistic prognostication of outcomes after
        curative intent surgery for HCC by taking into account a multitude of postoperative
        disease states and transitions between them. Our multistate modeling calculator can 
        provide meaningful data to guide the management of patients undergoing postoperative surveillance and therapy"
            ),
           # br(),
            
            h4("Methods"),
            p(
                "Adult patients (\u226518 years) undergoing LR for HCC between Jan-2000 and Dec-2018 were retrospectively
        identified at a single academic institution (Toronto General Hospital). Multistate data analysis was 
        employed to model post-LR tumor progression by describing transitions between distinct disease states.
        In the selected model, the states included surgery, local recurrence (1st,2nd,3rd,4th,5th), 
        distant metastasis with or without local recurrence, and death. The diagnosis of HCC was established 
        as per the American Association for the Study of Liver Diseases (AASLD) guidelines. Patients with preoperative
        tumour rupture, prior HCC treatment (including liver resection, liver transplant, and locoregional therapies
        such as radiofrequency ablation, transarterial chemoembolization, and microwave ablation), missing pathology
        information or fibrolamellar subtype on pathology were excluded."), 
            
            p("To visualize and estimate the incidence of
        first, second, third, fourth, and fifth local recurrence a Kaplan-Meier method
        with a clock reset approach was applied. Patients without recurrence, distant metastasis,
        or death were censored. Multistate data analysis was used to model post-LR tumor
        progression by describing transitions between several well-defined, distinct
        states in which the Markov assumption was specified so that the event process’
        future evolution depends on the current state. In the selected model, the states included
        surgery, first local recurrence, second local recurrence, third local recurrence, fourth
        local recurrence, fifth local recurrence, a separate state encompassing any local
        and distant metastasis or distant metastasis alone, and death (absorbing state). 
        Patients who did not progress to another state remained in their most recent state,
        regardless of treatment received (e.g. transplant). In this progressive multistate model,
        transitions are only allowed in the forward direction. Based on these transitions,
        a matrix was constructed and formed the basis for estimating the transition intensity
        functions used to estimate risk (transition probability) functions. Moreover, fixed 
        covariate effects were evaluated through adjustments by age, sex, 
        and post-LR pathology variables – these are reported as hazard ratios (HR)."),
            HTML(
                "<p>Statistical analyses were performed using R (R Core Team (2019) <a href='http://www.R-project.org/'> R: A language and environment for statistical computing.
         R Foundation for Statistical Computing, Vienna, Austria.</a>
        Multistate modeling was performed using the ‘msm’ and ‘survival’ packages.</p>"
                
            ),
            #TODO: Add link to abstract/poster
            HTML("<p><a href='https://www.google.com'> Read the full paper here </a></p>"),
            #br(),
            HTML(
                "<p>This model was created and presented as a Capstone project for the Data Science in 
                Biomedical Engineering and Data Science for Public Health Courses at the 
                Johns Hopkins Bloomberg School of Public Health.</a></p>"
                
            )
        )
    )
)

server <- function(input, output, session) {
    
    values <- reactiveValues(
        model = "preop",
        state = "Surgery",
        stateBaseline = "Surgery",
        sex = "Female",
        age = 18,
        multipleTumours = 0,
        largestTumour = 0,
        satellite = "No",
        microvascular = 0,
        explanation = "The figure below represents the predicted 5-year probabilities
    of making a transition to various disease states after curative
    intent liver resection for a 18-year old female who has not yet undergone surgery."
    )
    
    output$plotExplanation <- renderText({
        return(values$explanation)
    })
    
    observeEvent(input$makePlotBaseline, {
        values$stateBaseline <- input$stateBaseline
        
    })
    
    observe({
        output$probPlotBaseline <- renderPlotly({
            
            preparePlot(input = "base", 
                        state = values$stateBaseline)
        })
    })
    observe({
        output$probPlotBaselineYearly <- renderPlotly({
            
            preparePlot(input = "base", 
                        state = values$stateBaseline,
                        by_year = TRUE)
        })
    })
    
    
    observeEvent(input$makePlot, {
        values$model <- input$modelType
        values$state <- input$state
        values$stateBaseline <- input$stateBaseline
        values$sex <- input$sex
        values$age <- input$age 
        values$multipleTumours <- input$multipleTumours
        values$largestTumour <- input$largestTumour
        values$microvascular <- input$microvascular
        
        if(input$satellite) {
            values$satellite <- "Yes"
        }
        else {
            values$satellite <- "No"
        }
        
        # Prepare plot exmplanation phrase:
        sum <- paste(sep = " ", "The figure below represents the predicted 5-year probabilities of making", 
                     "a transition to various disease states after curative intent liver resection for a",
                     values$age, "year old")
        
        if(values$model == "preop") {
            sum <- paste(sum, sep = " ", tolower(values$sex), 
                         "who has not yet undergone surgery")
        }
        
        else if(values$model == "postop") {
            sum <- paste(sum, sep = " ", tolower(values$sex),
                         "who has undergone surgery")
            
            if(values$state == "Surgery"){
                sum <- paste(sum, sep = " ", "without post-surgical recurrence.") 
            }
            
            else if(values$state != "Surgery"){
                sum <- paste(sum, sep = " ", "who has developed a", 
                             paste0(tolower(values$state), ".")) 
            }
            
            if(values$multipleTumours | values$largestTumour | 
               values$satellite == "No" | values$microvascular){
                sum <- paste(sum, sep = " ", 
                             "Surgical pathology demonstrated:")
                
                if(values$multipleTumours){
                    sum <- paste(sum, sep = " ", 
                                 "multiple tumours")
                    
                }
                else if(!values$multipleTumours){
                    sum <- paste(sum, sep = " ", 
                                 "one solid tumour")
                    
                }
                
                if(values$largestTumour){
                    sum <- paste(paste0(sum, ","), sep = " ", 
                                 "a greatest tumour size of \u22655cm")
                }
                
                if(values$satellite == "Yes"){
                    sum <- paste(paste0(sum, ","), sep = " ", 
                                 "the presence of satellite lesions")
                } 
                
                if(values$microvascular){
                    sum <- paste(paste0(sum, ","), sep = " ", 
                                 "microvascular invasion")
                } 
                
                sum <- paste0(sum, ".")
            }
            
        }
        
        # debugging in console
        print(sum)
        
        values$explanation <- sum
        
    })
    
    observe({
        output$probPlot <- renderPlotly({
            
            if(values$model == "preop"){
                preparePlot(input = values$model, 
                            state = "Surgery", 
                            sex = values$sex, 
                            age = values$age)
            }
            
            else{
                preparePlot(input = values$model, 
                            state = values$state, 
                            sex = values$sex, 
                            age = values$age, 
                            solitary = values$multipleTumours, 
                            satellite = values$satellite, 
                            microvascular = values$microvascular, 
                            size = values$largestTumour)
            }
            
        })
    })
    
    observe({
        output$probPlotYearly <- renderPlotly({
            
            if(values$model == "preop"){
                preparePlot(input = values$model, 
                            state = "Surgery", 
                            sex = values$sex, 
                            age = values$age,
                            by_year = TRUE)
            }
            
            else{
                preparePlot(input = values$model, 
                            state = values$state, 
                            sex = values$sex, 
                            age = values$age, 
                            solitary = values$multipleTumours, 
                            satellite = values$satellite, 
                            microvascular = values$microvascular, 
                            size = values$largestTumour,
                            by_year = TRUE)
            }
            
        })
    })
    
    output$yearlyData <- renderDT({
        ggp <- NULL
        if(values$model == "preop"){
            ggp <- preparePlot(input = values$model, 
                               state = "Surgery", 
                               sex = values$sex, 
                               age = values$age,
                               by_year = TRUE)
        }
        else{
            ggp <- preparePlot(input = values$model, 
                               state = values$state, 
                               sex = values$sex, 
                               age = values$age, 
                               solitary = values$multipleTumours, 
                               satellite = values$satellite, 
                               microvascular = values$microvascular, 
                               size = values$largestTumour,
                               by_year = TRUE)
        }
        ggp_data <- plotly_data(ggp)
        #TODO: error below resulting when the model type is "postop", since the first 9
        # rows becomes too many rows to get rid of 
        
      #  if(values$model == "preop" | (values$model == "postop" & values$state == "Surgery")){
      #      ggp_data <- ggp_data[(9:nrow(ggp_data)),c("Years", "State", "Probability")]
      #  }
        
      #  else if(values$model == "postop" & values$state == "First local recurrence"){
      #      ggp_data <- ggp_data[(8:nrow(ggp_data)),c("Years", "State", "Probability")]
      #  }
      #  
      #  else if(values$model == "postop" & values$state == "Second local recurrence"){
      #      ggp_data <- ggp_data[(7:nrow(ggp_data)),c("Years", "State", "Probability")]
      #      
      #  }
        
        ggp_data$Probability <- percent(ggp_data$Probability, accuracy = 0.1, digits = 2, trim = TRUE)
        ggp_data <- t(pivot_wider(ggp_data, names_from = State, values_from = Probability))
        colnames(ggp_data) <- sprintf("Year %s", seq(1:5))
        ggp_data <- ggp_data[2:nrow(ggp_data),]
        
        DT::datatable(ggp_data, 
                      options = list(searching = FALSE, 
                                     paging = FALSE, 
                                     autoWidth = TRUE))
        
    }
    
    
    )
    
}

# Run the application
shinyApp(ui = ui, server = server)
