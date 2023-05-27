###################################
#          Hepatofactor           #  
###################################

# By Andrea Vaca

# Load packages
library(shiny)
library(shinydashboard)
#library(shinyjs) # we use shinyjs:: in the code
library(shinyFeedback)
library(shinyalert)
library(caret)
library(recipes)
library(dplyr)
library(ggplot2)
library(plotly)
library(rlang)
library(waiter)
library(keras)
library(tensorflow)

#setwd("D:/DRIVE UNIVERSIDAD/UOC/Sem4/Revisión/HepatoFactor")

# Data and machine learning model
modelo_sfg <- keras::load_model_hdf5("data/modelo_sfg.h5")


#Datos
load("data/datos.RData")


# Load R scripts:
source("scripts/script_graficos_web.R")

#Function normalized
normalizar <- function(data, min_vals, max_vals) {
  normalized_data <- data  # Crear una copia del data frame original
  
  for (col in colnames(data)) {
    normalized_data[[col]] <- (data[[col]] - min_vals[[col]]) / (max_vals[[col]] - min_vals[[col]])
  }
  
  return(normalized_data)
}

# Required variables to make a prediction:
variables <- c("Id","Age","Gender","WBC", "RBC", "Plat", "AST.1", "ALT.1", "RNA.Base")


# Render report function:
render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}



# User Interface (UI)
ui <- dashboardPage(
  
  # General theme for the app
  skin = "blue",
  
  # Header
  dashboardHeader(
    
    # Title
    title = "Hepatic Fibrosis Degree Prediction", 
    titleWidth = 300,
    
    # Help button
    tags$li(class = "dropdown", actionLink("help", "Help")),
    
    # Download report button
    tags$li(class = "dropdown", shinyjs::hidden(downloadLink("downloadReport", "Generate report")))
    
  ), # dashboardHeader final
  
  
  
  # Side Bar
  dashboardSidebar(
    
    # Width of the sidebar in pixels
    width = 300,
    
    # Form
    div(
      # Link for app information and templates
      p(class="p-form", "Information and templates", 
        actionLink("info",  label = "", icon = icon("info-circle")))
    ),
    
    # STEP 1: Type of prediction
    radioButtons("radioPred", label = "1. Number of predictions:",
                 inline = TRUE,
                 choices = list("Unique" = "unique", 
                                "Multiple" = "multiple"), 
                 selected = "unique"),
    
    # STEP 2: Upload data
    conditionalPanel('input.radioPred == "unique"',
                     
                     radioButtons("radioData", label = "2. Upload data:",
                                  inline = TRUE,
                                  choices = list("CSV file" = "file",
                                                 "Manually" = "manual"), 
                                  selected = "file")
    ),
    
    
    div(id = "form",
        
        # Load a CSV file
        conditionalPanel('input.radioPred == "multiple"  || (input.radioPred == "unique"  && input.radioData == "file")',
                         
                         fileInput("DataFile", "Choose CSV File:",
                                   accept = ".csv")
        ),
        
        # Load data manually
        conditionalPanel('input.radioPred == "unique"  && input.radioData == "manual"',
                         
                         # Fullfill the form:
                         div(id="manual-form",
                             
                             textInput("Id", label = "ID:", placeholder = "For example: 1, sample1,..."),
                             numericInput("Age", label = "Age:", value = NULL, min = 1, max = 130),
                             
                             selectInput("Gender", label = "Gender:",
                                         choices = list("Select a gender" = "","Male"="Male", "Female"="Female"),
                                         selected = ""),
                             
                             numericInput("WBC", label = "White Blood Cells (units/μL):", value = NULL, min = 0, max = NA),
                             numericInput("RBC", label = "Red Blood Cells (units/μL):", value = NULL, min = 0, max = NA),
                             numericInput("Plat", label = "Platelet (units/μL):", value = NULL, min = 0, max = NA),
                             numericInput("AST.1", label = "Aspartate Aminotransferase Base (units/L):", value = NULL, min = 0, max = NA),
                             numericInput("ALT.1", label = "Alamine Aminotransferase Base (units/L):", value = NULL, min = 0, max = NA),
                             numericInput("RNA.Base", label = "Ribonucleic Acid Base (U):", value = NULL, min = 0, max = NA)
                                                      )
        )
    ),
    
    div(class="form-buttons",
        actionButton("clear", "Clear"),             # Clear form button
        actionButton("submit", "Make prediction")   # Submit button
    )
    
  ), # dashboardSidebar final
  
  
  
  
  # Body
  dashboardBody(
    
    # header HTML tag
    tags$head(
      tags$link(rel="icon", href="favicon.svg"), # favicon
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # CSS
    ),
    
    
    # This function must be called from a Shiny app's UI in order for all other shinyjs functions to work.
    shinyjs::useShinyjs(),
    
    # This function must be called from a Shiny app's UI in order for all other shinyalert functions to work.
    shinyalert::useShinyalert(),
    
    # This function must be called from a Shiny app's UI in order for all other waiter functions to work.
    waiter::useWaitress(),
    
    # This function must be called from a Shiny app's UI in order for all other shinyFeedback functions to work.
    shinyFeedback::useShinyFeedback(), 
    
    div (class="boxMain",
         
         # Predictions results
         fluidRow(
           box(title = "Prediction", status = "primary", solidHeader = TRUE,
               collapsible = TRUE, width=12,
               
               uiOutput("resultsUI")
           )
         ),
         
         
         # Data Tables
         fluidRow(
           box(title = "Data", status = "primary", solidHeader = TRUE,
               collapsible = TRUE, width=12,
               
               uiOutput("tablesUI") 
           )
         ),
         
         
         # Graphs of data
         fluidRow(
           box(title = "Distplots", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
               
               uiOutput("distplotUI")
           ),
           
           box(title = "Boxplots", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
               
               uiOutput("boxplotUI")
           )
         ),
         
         fluidRow(
           box(title = "Pie Plots", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
               
               uiOutput("piechartUI")
           )
         )
    ),
    
    tags$footer(p("Hepatic Fibrosis Degree Prediction by Andrea Vaca Tello"),
                div(class="footerLinks",
                    actionLink("linkedin", label= a(href="https://www.linkedin.com/in/andrea-vaca-040853116/", 
                                                    div(class="linkedinLink" ,icon("linkedin"), p("andreavacatello")), target="_blank")),
                    actionLink("github",   label= a(href="https://github.com/AndreaVacaUoc", 
                                                    div(icon("github"),   p("AndreaVacaUoc")), target="_blank"))
                ),
                HTML('<div class="CreativeCommons"><a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">
                     <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" />
                     </a>This work is licensed under a<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>
                     </div>'),
    )
    
  ) # dashboardBody final
  
  
) # UI final


# Define server
server <- function(input, output) {
  
  
  #################
  #  Manual form  #
  #  Validations  #
  #################
  
  # Check success and warmings
  
  ## Input ID
  observeEvent(input$Id, {    ## Input ID
    if (nchar(input$Id) > 0) {
      hideFeedback("Id")
      showFeedbackSuccess(
        inputId = "Id"
      )
    } else {
      hideFeedback("Id")
    }
  })
  
  ## Input Age
  observeEvent(input$Age, {
    if (!is.na(input$Age) & input$Age >= 1 & input$Age <= 130) {
      hideFeedback("Age")
      showFeedbackSuccess(
        inputId = "Age"
      )
    } else if (!is.na(input$Age) & (input$Age < 1 | input$Age > 130)) {
      hideFeedback("Age")
      showFeedbackWarning(
        inputId = "Age",
        text = "Must be between 1 and 130"
      )
    } else {
      hideFeedback("Age")
    }
  })
  
  ## Input Gender
  observeEvent(input$Gender, {
    if (input$Gender != "") {
      hideFeedback("Gender")
      showFeedbackSuccess(
        inputId = "Gender"
      )
    } else {
      hideFeedback("Gender")
    }
  })

  ## Input WBC
  observeEvent(input$WBC, {
    if (!is.na(input$WBC) & input$WBC >= 0) {
      hideFeedback("WBC")
      showFeedbackSuccess(
        inputId = "WBC"
      )
    } else if (!is.na(input$WBC) & input$WBC < 0) {
      hideFeedback("WBC")
      showFeedbackWarning(
        inputId = "WBC",
        text = "Must be positive"
      )
    } else {
      hideFeedback("WBC")
    }
  })
  
  ## Input RBC
  observeEvent(input$RBC, {
    if (!is.na(input$RBC) & input$RBC >= 0) {
      hideFeedback("RBC")
      showFeedbackSuccess(
        inputId = "RBC"
      )
    } else if (!is.na(input$RBC) & input$RBC < 0) {
      hideFeedback("RBC")
      showFeedbackWarning(
        inputId = "RBC",
        text = "Must be positive"
      )
    } else {
      hideFeedback("RBC")
    }
  })
 
  ## Input Plat
  observeEvent(input$Plat, {
    if (!is.na(input$Plat) & input$Plat >= 0) {
      hideFeedback("Plat")
      showFeedbackSuccess(
        inputId = "Plat"
      )
    } else if (!is.na(input$Plat) & input$Plat < 0) {
      hideFeedback("Plat")
      showFeedbackWarning(
        inputId = "Plat",
        text = "Must be positive"
      )
    } else {
      hideFeedback("Plat")
    }
  })
  
  ## Input AST.1
  observeEvent(input$AST.1, {
    if (!is.na(input$AST.1) & input$AST.1 >= 0) {
      hideFeedback("AST.1")
      showFeedbackSuccess(
        inputId = "AST.1"
      )
    } else if (!is.na(input$AST.1) & input$AST.1 < 0) {
      hideFeedback("AST.1")
      showFeedbackWarning(
        inputId = "AST.1",
        text = "Must be positive"
      )
    } else {
      hideFeedback("AST.1")
    }
  })
  
  ## Input ALT.1
  observeEvent(input$ALT.1, {
    if (!is.na(input$ALT.1) & input$ALT.1 >= 0) {
      hideFeedback("ALT.1")
      showFeedbackSuccess(
        inputId = "ALT.1"
      )
    } else if (!is.na(input$ALT.1) & input$ALT.1 < 0) {
      hideFeedback("ALT.1")
      showFeedbackWarning(
        inputId = "ALT.1",
        text = "Must be positive"
      )
    } else {
      hideFeedback("ALT.1")
    }
  })
  
   ## Input RNA.Base
  observeEvent(input$RNA.Base, {
    if (!is.na(input$RNA.Base) & input$RNA.Base >= 0) {
      hideFeedback("RNA.Base")
      showFeedbackSuccess(
        inputId = "RNA.Base"
      )
    } else if (!is.na(input$RNA.Base) & input$RNA.Base < 0) {
      hideFeedback("RNA.Base")
      showFeedbackWarning(
        inputId = "RNA.Base",
        text = "Must be positive"
      )
    } else {
      hideFeedback("RNA.Base")
    }
  })
  
 
  
  # Check required inputs
  observeEvent(input$submit, {
    
    if (input$radioPred == "unique" & input$radioData == "manual") {
      
      if (nchar(input$Id) == 0) { ## Input ID
        showFeedbackDanger(
          inputId = "Id",
          text = ""
        )
      }
      
      if (is.na(input$Age)) { ## Input Age
        showFeedbackDanger(
          inputId = "Age",
          text = ""
        )
      }
      
      if (input$Gender == "") { ## Input Gender
        showFeedbackDanger(
          inputId = "Gender",
          text = ""
        )
      }
      
      if (is.na(input$WBC)) { ## Input WBC
        showFeedbackDanger(
          inputId = "WBC",
          text = ""
        )
      }
      
      if (is.na(input$RBC)) { ## Input RBC
        showFeedbackDanger(
          inputId = "RBC",
          text = ""
        )
      }
      
      if (is.na(input$Plat)) { ## Input Plat
        showFeedbackDanger(
          inputId = "Plat",
          text = ""
        )
      }
      
      if (is.na(input$AST.1)) { ## Input AST.1
        showFeedbackDanger(
          inputId = "AST.1",
          text = ""
        )
      }
      
      if (is.na(input$ALT.1)) { ## Input ALT.1
        showFeedbackDanger(
          inputId = "ALT.1",
          text = ""
        )
      }
      
     
      if (is.na(input$RNA.Base)) { ## Input RNA.Base
        showFeedbackDanger(
          inputId = "RNA.Base",
          text = ""
        )
      }
      
    }
  })
  
  
  
  
  ##################
  #  Storage Data  #
  ##################
  
  # Reactivevaules for data storage
  storageData <- reactiveValues(data = NULL)
  
  # Save data
  observeEvent(input$submit, {
    
    if (input$radioPred == "multiple" | (input$radioPred == "unique" & input$radioData == "file")) {
      # From CSV file
      req(input$DataFile)
      
      storageData$data <- read.csv(input$DataFile$datapath)
      
    } else if (input$radioPred == "unique" & input$radioData == "manual") {
      # Load data manually
      req(input$Id,
          input$Age,
          input$Gender,
          input$WBC,
          input$RBC,
          input$Plat,
          input$AST.1,
          input$ALT.1,
          input$RNA.Base) 
     
      storageData$data <- data.frame(Id       = input$Id,
                                     Age      = input$Age,
                                     Gender   = input$Gender,
                                     WBC      = input$WBC,
                                     RBC      = input$RBC,
                                     Plat     = input$Plat,
                                     AST.1    = input$AST.1,
                                     ALT.1    = input$ALT.1,
                                     RNA.Base = input$RNA.Base)
    }
  })
  
  
  
  #################
  #    General    #
  #  Validations  #
  #################
  
  observeEvent(input$submit, {
    
    # 1) Check that all variables are present
    req(storageData$data)
    
    var_error <- NULL
    
    for (i in variables) {
      if (!(i %in% names(storageData$data))) {
        var_error <- c(var_error, i)
      }
    }
    
    if (!is.null(var_error)) {
      var_error <- paste0("Variable/s required: ", paste(var_error, collapse = ", "),".")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problems:",
                   br(),
                   tags$ul(
                     tags$li(var_error)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    }
    
    
    # 2) Check NAs
    req(storageData$data)
    
    nas <- apply(storageData$data, 2, function(x){sum(is.na(x))}) # NAs per vararible
    nas_var <- NULL
    
    for (i in variables) {
      if (nas[[i]] > 0) {
        nas_var <- c(nas_var, i)
      }
    }
    
    if (!is.null(nas_var)) {
      nas_var <- paste0("Missing values in Variable/s: ", paste(nas_var, collapse = ", "), ".")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(nas_var)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    }
    
    
    
    # 3) Check the variable type (numeric or character)
    #    At this point we don't have to check the Id variable 
    #    because it can be character or numeric.
    req(storageData$data)
    
    type_var1 <- NULL # Check numeric variables
    type_var2 <- NULL # Check character variables (only Gender)
    
    for (i in variables) {
      if (i != "Id" & i != "Gender" & !is.numeric(storageData$data[,i])) {
        type_var1 <- c(type_var1, i)
      }
      
      if (i == "Gender" & !is.character(storageData$data[,i])) {
        
        type_var2 <- c("Variable Gender must be character with labels Female/Male.")
        
      } 
      else if (i == "Gender" & is.character(storageData$data$Gender)) {
        
        gender_levels <- sort(unique(storageData$data$Gender))
        
        if (length(gender_levels) > 2) {
          type_var2 <- c("Variable Gender must be character with labels Female/Male.")
          
        } else if (length(gender_levels) == 2 & (gender_levels[1] != "Female" | gender_levels[2] != "Male")) {
          
          type_var2 <- c("Variable Gender must be character with labels Female/Male.")
          
        } else if (length(gender_levels) == 1 & !(gender_levels[1] %in% c("Female","Male"))) {
          
          type_var2 <- c("Variable Gender must be character with labels Female/Male.")
        }
      }
    }
    
    
    if (!is.null(type_var1)) {
      type_var1 <- paste0("Variable/s: ", paste(type_var1, collapse = ", "), " must be numeric.")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(type_var1)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    } else if (!is.null(type_var2)) {
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(type_var2)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
    }
    
    
    
    # 4) Check Id not repeated
    req(storageData$data)
    
    rep_id <- NULL
    
    if (length(unique(storageData$data$Id)) != length(storageData$data$Id)) {
      rep_id <- names(which(table(storageData$data$Id)>1))
    }
    
    if (!is.null(rep_id)) {
      rep_id <- paste0("Repeated IDs: ", paste(rep_id, collapse = ", "), ".")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(rep_id)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    }
    
    
    # 5) Check variable range
    req(storageData$data)
    
    range_age <- NULL # Check Variable Age Range
    range_var <- NULL # Check range of the rest of numeric variables
    
    for (i in variables) {
      if (i == "Age" & ((TRUE %in% unique(names(table(storageData$data[,i] < 1)))) | (TRUE %in% unique(names(table(storageData$data[,i] > 130)))))) {
        range_age <- "Age must be between 1 and 130 years"
      }
      
      if (i != "Age" & i != "Id" & i != "Gender" & (TRUE %in% unique(names(table(storageData$data[,i]< 0))))) {
        range_var <- c(range_var, i)
      }
    }
    
    
    if (!is.null(range_age)) {
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(range_age)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    } else if (!is.null(range_var)) {
      range_var <- paste0("Variable/s: ", paste(range_var, collapse = ", "), " must be greater than 0.")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(range_var)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    } else {
      # Success: Data is correct
      
      # Pop up alert
      shinyalert(
        title = "Prediction in progress",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   
                   "Possible results:",
                   br(),
                   tags$ul(
                     tags$li("Degree of fibrosis F1 (F1)"),
                     tags$li("Degree of fibrosis F2 (F2)"),
                     tags$li("Degree of fibrosis F3 (F3)"),
                     tags$li("Degree of fibrosis F4 (F4)")
                   ),
                   br(),
                   "Plots:",
                   br(),
                   tags$ol(
                     tags$li("They graph the information of the data used for the training of the predictive model."),
                     tags$li("They overlap the data used for the prediction making it easy to understand the results."),
                     tags$li("You can hover over the plots to get more information about each point.")
                   )
        ),
        size = "m",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
    }
    
  })
  
  
  
  
  ####################
  #  Data structure  #
  ####################
  
  # Transform variables correctly
  observeEvent(input$submit, {
    req(storageData$data)
    
    # Any patient whose age exceeded 89 is listed as being of age 90.
    storageData$data$Age    <- ifelse(storageData$data$Age<=89,storageData$data$Age,90)
    
    # Gender must be a factor with two levels (Female/Male)
    storageData$data$Gender <- factor(storageData$data$Gender, levels = c("Female","Male"))
  })
  
  
  
  
  ################
  #  Clear data  #
  ################
  
  # Reset form when changing  RadioButtons
  observeEvent(input$radioPred, {
    shinyjs::reset("form")
  })  
  
  observeEvent(input$radioData, {
    shinyjs::reset("form")
  })  
  
  # Clear button for all imputs
  observeEvent(input$clear, {
    shinyjs::reset("form")
  })
  
  
  
  
  ################
  # Prediction/s #
  ################
  
  # Make the prediction
  prediction <- eventReactive(input$submit, {
    req(storageData$data)
    
    # Data preprocesing
    datos_procesados <- storageData$data[, c("WBC", "RBC", "Plat", "AST.1", "ALT.1", "RNA.Base")]
    
    datos_normalizados <- normalizar(datos_procesados, min_val, max_val)
    
    x_test <- as.matrix(datos_normalizados[, 1:6])
    # Prediction/s
    predicciones_sfg_prob <- predict(modelo_sfg, x_test)
    
    predicciones_sfg <- max.col(predicciones_sfg_prob)
    
    # Convertir predicciones a valores originales de BH.staging
    predicciones_sfg <- factor(ifelse(predicciones_sfg == 1, "F1",
                                      ifelse(predicciones_sfg == 2, "F2",
                                             ifelse(predicciones_sfg == 3, "F3", "F4"))),
                               levels = c("F1", "F2", "F3", "F4"))
    predicciones_sfg

  })
  
  # Dataframe with data and predictions
  data_pred <-eventReactive(input$submit, {
    req(prediction())
    
    storageData$data %>%
      mutate(BH.staging = factor(prediction(), levels = c("F1","F2","F3","F4")))
  })
  
  
  
  
  ####################
  #    hide/show     #
  #  Report Button   #
  ####################
  
  # Show
  observeEvent(input$submit, {
    req(storageData$data, prediction(), data_pred())
    
    shinyjs::show("downloadReport") 
  })
  
  # Hide
  observeEvent(input$submit, {
    if (is.null(storageData$data)) {
      shinyjs::hide("downloadReport") 
    }
  })
  
  
  ###########
  # Outputs #
  ###########
  
  # Data and predictions:
  observeEvent(input$submit,{
    req(prediction(), data_pred())
    
    # Depending on input$radioPred we'll generate 
    # different UI components and outputs:
    
    if (input$radioPred == "unique") { # Unique prediction
      
      output$resultsUI <- renderUI({
        textOutput("uni_class_predict")
      })
      
      output$tablesUI <- renderUI({
        tableOutput("unitable")
      })
      
      # Result unique
      output$uni_class_predict <- renderText({paste("The result of the prediction is:", prediction()[1])})
      
      # Table unique
      output$unitable <- renderTable({storageData$data[1,]})
      
    } else { # Multiple prediction
      
      output$resultsUI <- renderUI({
        req(prediction(), data_pred()) # So that when giving an error the option to download data also disappears
        div(
          DT::dataTableOutput("multi_class_predict"),
          br(),
          downloadButton("downloadPred", "Download results")
        )
      })
      
      output$tablesUI <- renderUI({
        DT::dataTableOutput("multitable")
      })
      
      # Results multiple
      output$multi_class_predict <- DT::renderDataTable({data_pred()},
                                                        rownames= FALSE)
      
      # Table multiple
      output$multitable <- DT::renderDataTable({storageData$data},
                                               rownames= FALSE)
      
      # Download results multiple
      output$downloadPred <- downloadHandler(
        filename = function() {
          paste0(gsub(".csv", "", input$DataFile$name), "_Predictions.csv")
        },
        content = function(file) {
          write.csv(data_pred(), file, row.names = FALSE)
        }
      )
    }
  })
  
  
  
  
  # Plots UI
  
  ## Distplot UI (Histogram + Rug)
  output$distplotUI <- renderUI({
    req(data_pred())
    
    div(
      selectInput("var1", label = NULL,
                  choices = list("Select a variable" = "",
                                 "Age"="Age", 
                                 "WBC"="WBC",
                                 "RBC"="RBC",
                                 "Plat"="Plat",
                                 "AST.1"="AST.1",
                                 "ALT.1"="ALT.1",
                                 "RNA.Base"="RNA.Base"),
                  selected = "Age"),
      
      plotlyOutput("distplot")
      
    )
  })
  
  ## Boxplot UI
  output$boxplotUI <- renderUI({
    req(data_pred())
    
    div(
      selectInput("var2", label = NULL,
                  choices = list("Select a variable" = "",
                                 "Age"="Age", 
                                 "WBC"="WBC",
                                 "RBC"="RBC",
                                 "Plat"="Plat",
                                 "AST.1"="AST.1",
                                 "ALT.1"="ALT.1",
                                 "RNA.Base"="RNA.Base"),
                  selected = "Age"),
      
      plotlyOutput("boxplot")
      
    )
  })
  
  ## Pie chart UI
  output$piechartUI <- renderUI({
    req(data_pred())
    
    div(
      selectInput("var3", label = NULL,
                  choices = list("Select a variable" = "",
                                 "Gender" = "Gender", 
                                 "BH.staging"  = "BH.staging"),
                  selected = "Gender"),
      
      plotlyOutput("pie_chart")
      
    )
  })
  
  
  
  # Plots outputs
  
  ## Distplot output
  output$distplot <- renderPlotly({
    req(data_pred(), input$var1)
    
    # Arguments distplot
    args1 <- switch(input$var1,
                    "Age"     = list(data_pred(), sym("Age"), "Age"),
                    "WBC"     = list(data_pred(), sym("WBC"), "WBC"),
                    "RBC"     = list(data_pred(), sym("RBC"), "RBC"),
                    "Plat"    = list(data_pred(), sym("Plat"), "Plat"),
                    "AST.1"   = list(data_pred(), sym("AST.1"), "AST.1"),
                    "ALT.1"   = list(data_pred(), sym("ALT.1"), "ALT.1"),
                    "RNA.Base"= list(data_pred(), sym("RNA.Base"), "RNA.Base"))
    
    # Distplot
    do.call(plot_distplot, args1)
  })
  
  ## Boxplot output
  output$boxplot <- renderPlotly({
    req(data_pred(), input$var2)
    
    # Arguments Boxplot
    args2 <- switch(input$var2,
                    "Age"     = list(data_pred(), sym("Age"), "Age"),
                    "WBC"     = list(data_pred(), sym("WBC"), "WBC"),
                    "RBC"     = list(data_pred(), sym("RBC"), "RBC"),
                    "Plat"    = list(data_pred(), sym("Plat"), "Plat"),
                    "AST.1"   = list(data_pred(), sym("AST.1"), "AST.1"),
                    "ALT.1"   = list(data_pred(), sym("ALT.1"), "ALT.1"),
                    "RNA.Base"= list(data_pred(), sym("RNA.Base"), "RNA.Base"))
    
    # Boxplot
    do.call(plot_boxplot, args2)
  })
  
  ## Pie chart output
  output$pie_chart <- renderPlotly({
    req(data_pred(), input$var3)
    
    if (input$var3 == "Gender") {
      
      # Pie chart Gender
      plot_gender(data_pred()) 
      
    } else if (input$var3 == "BH.staging") {
      
      # Pie chart BH.staging
      plot_class(data_pred()) 
      
    }
  })
  
  
  
  
  ############
  #  Report  #
  ############
  
  # Rmarkdown Report
  output$downloadReport <- downloadHandler(
    
    filename = "Hepatic Fibrosis Degree Prediction.pdf",
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir())
      my_files <- list.files("reports")
      file.copy(paste0("reports/", my_files), tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      if (nrow(data_pred()) > 1) {
        markdown <- file.path(tempReport, "report_multiple.Rmd")
        
        params <- list(data_raw   = storageData$data,
                       data_pred  = data_pred())
        
      } else {
        markdown <- file.path(tempReport, "report_unique.Rmd")
        
        params <- list(prediction = prediction(),
                       data_raw   = storageData$data,
                       data_pred  = data_pred())
      }
      
      # Notification of rendering report
      notify <- showNotification(
        "Rendering report...",
        duration = NULL,
        closeButton = TRUE,
        type = "warning"
      )
      on.exit(removeNotification(notify), add = TRUE)
      
      
      # Render report and download progress bar
      withProgressWaitress({
        
        for (i in 1:10) {
          
          incProgressWaitress(2)
          
          if (i == 6) {
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            callr::r(render_report,
                     list(input = markdown, output = file, params = params))
          } else {
            Sys.sleep(0.25)
          }
        }
      }, selector = "#downloadReport", max = 15, theme = "overlay-percent")
    })
  
  
  
  
  
  ##########
  #  Info  #
  ##########
  
  # Pop up with info
  observeEvent(input$info, {
    shinyalert(
      title = "Information and templates",
      text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                 
                 "How works this app?",
                 br(),
                 tags$ul(
                   tags$li(a(href="info_files/Web_Aplication_Guide.pdf", "Web Aplication Guide", download=NA, target="_blank"))
                 ),
                 br(),
                 "Templates and examples of", tags$em("CSV"), "files:",
                 br(),
                 tags$ul(
                   tags$li(a(href="info_files/Example_unique_pred.csv", "Example for unique predictions", download=NA, target="_blank")),
                   tags$li(a(href="info_files/Example_multiple_pred.csv", "Example for multiple predictions", download=NA, target="_blank")),
                   tags$li(a(href="info_files/Template_pred.csv", "Template for predictions", download=NA, target="_blank"))
                 )
      ),
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Ok",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  
  
  
  ##########
  #  Help  #
  ##########
  
  # Pop up with info
  observeEvent(input$help, {
    shinyalert(
      title = "Information about the app",
      text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                 
                 "How works this app?",
                 br(),
                 tags$ul(
                   tags$li(a(href="info_files/Web_Aplication_Guide.pdf", "Web Aplication Guide", download=NA, target="_blank"))
                 ),
                 br(),
                 "Templates and examples of", tags$em("CSV"), "files:",
                 br(),
                 tags$ul(
                   tags$li(a(href="info_files/Example_unique_pred.csv", "Example for unique predictions", download=NA, target="_blank")),
                   tags$li(a(href="info_files/Example_multiple_pred.csv", "Example for multiple predictions", download=NA, target="_blank")),
                   tags$li(a(href="info_files/Template_pred.csv", "Template for predictions", download=NA, target="_blank"))
                 ),
                 br(),
                 "Possible results:",
                 br(),
                 tags$ul(
                   tags$li("Degree of fibrosis F1 (F1)"),
                   tags$li("Degree of fibrosis F2 (F2)"),
                   tags$li("Degree of fibrosis F3 (F3)"),
                   tags$li("Degree of fibrosis F4 (F4)")
                 ),
                 br(),
                 "Plots:",
                 br(),
                 tags$ol(
                   tags$li("They graph the information of the data used for the training of the predictive model."),
                   tags$li("They overlap the data used for the prediction making it easy to understand the results."),
                   tags$li("You can hover over the plots to get more information about each point.")
                 )
      ),
      size = "m", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Ok",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  
}  # server final




# Run the application 

## Opcion 1
shinyApp(ui = ui, server = server)

