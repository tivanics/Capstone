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


ui <- fluidPage(
    theme = shinytheme("yeti"),
    shinyjs::useShinyjs(),
     #shinythemes::themeSelector(),
    
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
            title = "Multistate model",
            
            sidebarPanel(
                width = 4,
                h4(strong("Clinical variables")),
                br(),
                selectInput(
                    "modelType",
                    "Model Type",
                    c(
                        'Preoperative' = "preop",
                        'Postoperative' = "postop"
                    )
                ),

                numericInput("age", "Age", min = 18, value = 18),
                radioButtons("sex", "Sex",
                             c(Female = "Female", Male = "Male"),
                             selected = "Female"),
                
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
                    br(),
                    h5(strong("Pathology specimen")),
                    checkboxInput("multipleTumours", "Multiple tumours"),
                    checkboxInput("largestTumour", "Largest tumour size  \u22655cm"),
                    checkboxInput("satellite", "Satellite lesions"),
                    checkboxInput("microvascular", "Microvascular invasion")
                ),
                actionButton("makePlot", "Create Plot")
            ),
            
            mainPanel(
                h4("Hepatocellular Carcinoma Risk Probabilities after curative intent liver resection"),
                h5(uiOutput("plotExplanation")),
                br(),
                
                # titlePanel(textOutput("titlePanel")),
                mainPanel(
                  helpText("Hover your mouse over the curves in the figure for monthly risk predictions 
                              per disease state"),
                                   plotlyOutput("probPlot", width = "800px", height = "600px")        
                          ),
                    
                #    tabsetPanel(
                #        
                #        tabPanel(title = "Monthly",
                #                 br(),
                #                 helpText("Hover your mouse over the curves in the figure for monthly risk predictions 
                #              per disease state"),
                #                 plotlyOutput("probPlot", width = "800px", height = "600px")        
                #        ),
                #        tabPanel(title = "Yearly",
                #                 br(),
                #                 helpText("Hover your mouse over the curves in the figure for yearly risk predictions 
                #              per disease state"),
                #                 plotlyOutput("probPlotYearly", width = "800px", height = "600px")
                #        )
                        
                   # )#,
                    br()#,
              #  )
            )
        ),
        
        tabPanel(
            title = "About the Model",
            h2("Statistical Analysis of Results"),
            br(),
            h4("Background"),
            p(
                "Multistate modeling of HCC recurrence can be used to account for the
        various disease states a patient can exist in and transition between after
        curative-intent liver resection (LR). In contrast to standard single time-to-event estimate,
        multistate modeling provides more realistic prognostication of outcomes after
        curative intent surgery for HCC by taking into account a multitude of postoperative
        disease states and transitions between them. Our multistate modeling calculator can 
        provide meaningful data to guide the management of patients undergoing postoperative surveillance and therapy"
            ),
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
           h4("Statistical software"), 
           HTML(
                "<p>Statistical analyses were performed using R (R Core Team (2019) <a href='http://www.R-project.org/'> R: A language and environment for statistical computing.
         R Foundation for Statistical Computing, Vienna, Austria.</a>
        Multistate modeling was performed using the ‘msm’ and ‘survival’ packages.</p>"
                
            ),
            #TODO: Add link to abstract/poster
           h4("Manuscript"), 
            HTML("<p><a href='https://easl.eu/wp-content/uploads/2021/01/Digital-Liver-Cancer-Summit-2021-Abstract-book.pdf'> Read an abstract of the project here </a></p>"),
            #br(),
           h4("Capstone project"), 
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
        age = 50,
        multipleTumours = 0,
        largestTumour = 0,
        satellite = "No",
        microvascular = 0,
        explanation = "The figure below represents the predicted 5-year probabilities
    of making a transition to various postsurgical oncologic disease states after curative
    intent liver resection for a 50-year old female who has not yet undergone liver resection."
    )
    
    output$plotExplanation <- renderText({
        return(values$explanation)
    })
    
    #observeEvent(input$makePlotBaseline, {
    #    values$stateBaseline <- input$stateBaseline
    #    
    #})
    
#    observe({
 #       output$probPlotBaseline <- renderPlotly({
 #           
  #          preparePlot(input = "base", 
   #                     state = values$stateBaseline)
     #   })
    #})
   # observe({
   #     output$probPlotBaselineYearly <- renderPlotly({
  #          
  #          preparePlot(input = "base", 
  #                      state = values$stateBaseline,
  #                      by_year = TRUE)
  #      })
  #  })
    
    
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
        
        # Explanation of the clinical scenario
        sum <- paste(sep = " ", "The figure below represents the predicted 5-year probabilities of making", 
                     "a transition to various postsurgical oncologic disease states after curative intent liver resection for a",
                     values$age, "year old")
        
        if(values$model == "preop") {
            sum <- paste(sum, sep = " ", tolower(values$sex), 
                         "who has not yet undergone liver resection")
        }
        
        else if(values$model == "postop") {
            sum <- paste(sum, sep = " ", tolower(values$sex),
                         "who has undergone liver resection")
            
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
                                 "a solitary tumour")
                    
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
    
    #observe({
    #    output$probPlotYearly <- renderPlotly({
    #        
    #        if(values$model == "preop"){
    #            preparePlot(input = values$model, 
    #                        state = "Surgery", 
    #                        sex = values$sex, 
    #                        age = values$age,
    #                        by_year = TRUE)
    #        }
    #        
    #        else{
    #            preparePlot(input = values$model, 
    #                        state = values$state, 
    #                        sex = values$sex, 
    #                        age = values$age, 
    #                        solitary = values$multipleTumours, 
    #                        satellite = values$satellite, 
    #                        microvascular = values$microvascular, 
    #                        size = values$largestTumour,
    #                        by_year = TRUE)
    #        }
    #        
    #    })
    #})
    
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

       # ggp_data$Probability <- percent(ggp_data$Probability, accuracy = 0.1, digits = 2, trim = TRUE)
      #  ggp_data <- t(pivot_wider(ggp_data, names_from = State, values_from = Probability))
      #  colnames(ggp_data) <- sprintf("Year %s", seq(1:5))
      #  ggp_data <- ggp_data[2:nrow(ggp_data),]
      #  
      #  DT::datatable(ggp_data, 
       #               options = list(searching = FALSE, 
      #                               paging = FALSE, 
        #                             autoWidth = TRUE))
        
    }
    
    
    )
    
}

# Run the application
shinyApp(ui = ui, server = server)
