#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD IN REQUIRED LIBRARIES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
#API
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(httr)
# DATA and VISUALISATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(forcats)
library(tidyverse)
library(data.table)
library(reshape2)
library(lubridate)
library(htmlTable)

# ADMIN: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(packrat)
library(rsconnect)
# SHINY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(shinydashboardPlus)
library(shinyjqui)
library(styler)
library(shinyAce)
library(shinyEffects)
library(htmlwidgets)
library(htmltools)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ IN DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #API
# req <- httr::GET(url = "https://vqweudps2j.execute-api.ap-southeast-2.amazonaws.com/default/predict_los")
# #PATIENT DEMOGRAPHICS
# pat_info <- read.csv("data/patient_demographic.csv")

#fake data 
#data<-read.csv("data/Fake_Data_Patient_LOS.csv")
data_sample_full <- read.csv("data/my_samp_demo_2.csv")
#data_sample <- read.csv("data/10_pat_sample.csv")
#check
#str(data_sample)
Current_Time <- today()
data_sample <- data_sample_full %>% 
  select( -GENDER_SEX,
          -ADMISSION_TYPE, -INSURANCE, -ETHNICITY,-MARITAL_STATUS,-DOB, -DOD,
          -EXPIRE_FLAG) %>% 
  #set data types
  mutate(Admit_Date = Sys.Date() - 1, 
         Current_Time = Sys.Date()) %>% 
  #create a variable which stores the actual LOS per patient
  group_by(SUBJECT_ID) %>% 
  mutate(Actual_LOS = Current_Time - Admit_Date,
         Latest_prediction_days = Latest_LOS_prediction - Initial_LOS_prediction,
         Latest_prediction_days = gsub("0", Initial_LOS_prediction, Latest_prediction_days)) %>% 
  ungroup() %>% 
  group_by(SUBJECT_ID) %>% 
  #LOGIC - to colour the info boxes
  #Green = Most recent LOS prediction is shorter (in time/ length) than the initial LOS prediction and the actual LOS time is shorter than the most recent prediction
  #Yellow = Most recent LOS prediction is longer (in time/ length) than the initial LOS prediction and the actual LOS time is shorter than the most recent prediction
  #Red = the actual LOS is greater (in time/ length) than the most recent prediction
  #Grey = Most recent LOS prediction is  equal (in time/length) to the initial LOS prediction and the actual LOS time is shorter than the most recent prediction
  mutate(LOS_PRED_STATUS = case_when(
                           #need to set the difftime objects to numeric as we need to compare them as numbers
                           as.numeric(Latest_prediction_days) <= as.numeric(Initial_LOS_prediction) & as.numeric(Actual_LOS) <= as.numeric(Latest_prediction_days)  ~ "Green",
                           as.numeric(Latest_prediction_days) <= as.numeric(Initial_LOS_prediction) & as.numeric(Actual_LOS) <= as.numeric(Latest_prediction_days) ~ "Grey",
                           as.numeric(Latest_prediction_days) >= as.numeric(Initial_LOS_prediction) & as.numeric(Actual_LOS)  <= as.numeric(Latest_prediction_days) ~ "Yellow",
                           as.numeric(Actual_LOS)  > as.numeric(Latest_prediction_days)  ~ "Red"
  ))


data_demo<-data_sample_full %>% 
  select(SUBJECT_ID,GENDER_SEX,
         ADMISSION_TYPE, INSURANCE, ETHNICITY, MARITAL_STATUS,DOB) %>% 
  mutate(DOB = as.Date(DOB, format = "%d/%m/%Y"))
str(data_demo)


#set columns to POSIXct date format for manipulation using lubridate
#*************************************************************************************************************
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   USER INTERFACE UI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#*************************************************************************************************************

ui <- fluidPage(
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Header image and title
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    titlePanel(div(img(src = 'header_proto.png', height = "auto", width = "100%"))
    ),# end title panel for header image
    useShinydashboard(),
    title = "Predicting patient LOS", #title of web brower tab
   
    tabPanel("WARD", fluid = TRUE,
             useShinyalert(), # for the how to quick guide button
             # we want to use custom colours for the info box backgrounds so we need some custom css styling
             tags$style(
                 type = 'text/css',
                 #green
                 '.bg-maroon-gradient {background: -webkit-gradient(linear, 0% 100%, 0% 0%, from(rgb(7, 110, 48)), to(rgb(130, 173, 55))) !important; }'
             ),
             #yellow/amber
             tags$style(
                 type = 'text/css',
                 '.bg-yellow-gradient {background: -webkit-gradient(linear, 0% 100%, 0% 0%, from(rgb(	255, 65, 1)), to(rgb(	255, 192, 1))) !important; }'
             ),
             #blue/no change
             tags$style(
                 type = 'text/css',
                 '.bg-purple-gradient {background: -webkit-gradient(linear, 0% 100%, 0% 0%, from(rgb(14, 147, 230)), to(rgb(175, 177, 179))) !important; }'
             ),
             #red
             tags$style(
                 type = 'text/css',
                 '.bg-red-gradient {background: -webkit-gradient(linear, 0% 100%, 0% 0%, from(rgb(93,14,34)), to(rgb(230, 14, 35))) !important; }'
             ), 
             #RIDL purple
             tags$style(
                 type = 'text/css', 
                 '.bg-maroon {background-color: #AA9ABA!important; }'
             ),#end of customising the colours for info boxes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #sidebar Panel with legend for LOS status
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
    sidebarPanel(width = 3, style = "background-color: white;", 
                 div(img(src = 'LOS_legend.png', height = "auto", width = "100%"))
    ),#end sidebar panel
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Main panel with 4x graphs and info boxes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~         
    mainPanel(width = 9, style = "background-color: white;", 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # DROP DOWN MENU AND FIRST ROW OF INFO BOXES One Per hospital bed/patient
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # fluidRow(
        #   column(12, align = "centre",
        #          selectInput(inputId = "PatientID", 
        #                      label = "Select a patient ID to access their latest LOS prediction",
        #                      choices = unique(data_sample$SUBJECT_ID)
        #                      )# end select input
        #          )#end column
        # ),# end fluidrow
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Ward view
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        fluidRow(
            column(12, align = "left",
                   h1("Ward summary: LOS Prediction & Current Status"),
                   h3("Each box below represents a patient/bed in the ward. Boxes coloured based on legend on far left."),
                   tags$hr()
            )#end Col
        ),# end fluidrow
        
        fluidRow(
            column(12, align = "centre",
            uiOutput("Bed_1"), 
            uiOutput("Bed_2"),
            uiOutput("Bed_3"),
            uiOutput("Bed_4"),
            uiOutput("Bed_5")
              )#end column
            ),#end fluidrow
            
        fluidRow(
            column(12, align = "centre",
            uiOutput("Bed_6"),
            uiOutput("Bed_7"),
            uiOutput("Bed_8"),
            uiOutput("Bed_9"),
            uiOutput("Bed_10")
              )#end column
            )#end fluidrow
   

    )# end mainpanel
    
))#end fluidPage

#*************************************************************************************************************
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   SERVER LOGIC
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#**************************************************************************************************************

server <- function(input, output, session) {
    
  
#Bed 1 patient 1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Pat_1 <- eventReactive(data_sample$SUBJECT_ID == "11929",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "11929")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    
    Pat_1B <- eventReactive(data_sample$SUBJECT_ID == "11929",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "11929")
      dat2 <- dat$Latest_prediction_days
      dat2
    })
    
    Pat_1C <- eventReactive(data_sample$SUBJECT_ID == "11929",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "11929")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    output$Bed_1 <- renderUI({  
      x<- Pat_1()
      if(x =="Green"){gradientBox("Patient ID:", Pat_1C(),  
                     gradientColor = "maroon",
                     width = 2,
                     collapsible = FALSE,
                     icon = "fa fa-bed",
                     title = tags$p(style = "font-size: 18px;","Bed #1:"),
                     footer = HTML(paste("Latest LOS prediction:", Pat_1B())))
                     }
      else if ( x == "Yellow"){
                   gradientBox("Patient ID:", Pat_1C(),  
                   gradientColor = "yellow",
                   width = 2,
                   collapsible = FALSE,
                   icon = "fa fa-bed",
                   title = tags$p(style = "font-size: 18px;","Bed #1:"),
                   footer = HTML(paste("Latest LOS prediction:", Pat_1B())))
                   }
      else if ( x == "Red"){
                   gradientBox("Patient ID:", Pat_1C(),  
                   gradientColor = "red",
                   width = 2,
                   collapsible = FALSE,
                   icon = "fa fa-bed",
                   title = tags$p(style = "font-size: 18px;","Bed #1:"),
                   footer = HTML(paste("Latest LOS prediction:", Pat_1B())))
                   }
      else {
                   gradientBox("Patient ID:", Pat_1C(), 
                              width = 2,
                              collapsible = FALSE,
                              icon = "fa fa-bed",
                              gradientColor = "purple",
                              title = tags$p(style = "font-size: 18px;","Bed #1:"),
                              footer = HTML(paste("Latest LOS prediction:", Pat_1B())))
                              }
    })
         
    
    #Bed 2 patient 2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_2 <- eventReactive(data_sample$SUBJECT_ID == "22318",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "22318")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_2B <- eventReactive(data_sample$SUBJECT_ID == "22318",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "22318")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_2C <- eventReactive(data_sample$SUBJECT_ID == "22318",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "22318")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    
    output$Bed_2 <- renderUI({  
      x<- Pat_2()
      if(x =="Green"){gradientBox("Patient ID:", Pat_2C(),   
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #2:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_2B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_2C(),  
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #2:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_2B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_2C(), 
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #2:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_2B())))
      }
      else {
        gradientBox("Patient ID:", Pat_2C(), 
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #2:"),
                                   footer = HTML(paste("Latest LOS prediction:", Pat_2B())))
      }
    })            
    
    #Bed 3 patient 3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_3 <- eventReactive(data_sample$SUBJECT_ID == "23008",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "23008")
      dat2 <- dat$LOS
      dat2
    })
    Pat_3B <- eventReactive(data_sample$SUBJECT_ID == "23008",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "23008")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_3C <- eventReactive(data_sample$SUBJECT_ID == "23008",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "23008")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    
    output$Bed_3 <- renderUI({  
      x<- Pat_3()
      if(x =="Green"){gradientBox("Patient ID:", Pat_3C(),  
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #3:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_3B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_3C(),
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #3:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_3B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_3C(),
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #3:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_3B())))
      }
      else {
        gradientBox("Patient ID:", Pat_3C(),
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #3:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_3B())))
      }
    })            

    
    #Bed 4 patient 4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_4 <- eventReactive(data_sample$SUBJECT_ID == "23489",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "23489")
      dat2 <- dat$LOS
      dat2
    })
    Pat_4B <- eventReactive(data_sample$SUBJECT_ID == "23489",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "23489")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_4C <- eventReactive(data_sample$SUBJECT_ID == "23489",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "23489")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    
    output$Bed_4 <- renderUI({  
      x<- Pat_4()
      if(x =="Green"){gradientBox("Patient ID:", Pat_4C(),  
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #4:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_4B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_4C(),  
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #4:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_4B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_4C(),
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #4:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_4B())))
      }
      else {
        gradientBox("Patient ID:", Pat_4C(), 
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #4:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_4B())))
      }
    })     
    
    #Bed 5 patient 5~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_5 <- eventReactive(data_sample$SUBJECT_ID == "24687",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "24687")
      dat2 <- dat$LOS
      dat2
    })
    Pat_5B <- eventReactive(data_sample$SUBJECT_ID == "24687",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "24687")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_5C <- eventReactive(data_sample$SUBJECT_ID == "24687",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "24687")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    
    output$Bed_5 <- renderUI({  
      x<- Pat_5()
      if(x =="Green"){gradientBox("Patient ID:", Pat_5C(),
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #5:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_5B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_5C(), 
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #5:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_5B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_5C(),
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #5:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_5B())))
      }
      else {
        gradientBox("Patient ID:", Pat_5C(),
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #5:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_5B())))
      }
    })
    
    #Bed 6 patient 6~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_6 <- eventReactive(data_sample$SUBJECT_ID == "29615",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "29615")
      dat2 <- dat$LOS
      dat2
    })
    Pat_6B <- eventReactive(data_sample$SUBJECT_ID == "29615",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "29615")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_6C <- eventReactive(data_sample$SUBJECT_ID == "29615",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "29615")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    
    output$Bed_6 <- renderUI({  
      x<- Pat_6()
      if(x =="Green"){gradientBox("Patient ID:", Pat_6C(),  
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #6:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_6B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_6C(), 
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #6:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_6B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_6C(), 
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #6:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_6B())))
      }
      else {
        gradientBox("Patient ID:", Pat_6C(), 
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #6:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_6B())))
      }
    })    
    
    #Bed 7 patient 7~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_7 <- eventReactive(data_sample$SUBJECT_ID == "42843",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "42843")
      dat2 <- dat$LOS
      dat2
    })
    Pat_7B <- eventReactive(data_sample$SUBJECT_ID == "42843",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "42843")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_7C <- eventReactive(data_sample$SUBJECT_ID == "42843",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "42843")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    
    output$Bed_7 <- renderUI({  
      x<- Pat_7()
      if(x =="Green"){gradientBox("Patient ID:", Pat_7C(), 
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #7:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_7B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_7C(), 
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #7:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_7B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_7C(), 
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #7:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_7B())))
      }
      else {
        gradientBox("Patient ID:", Pat_7C(), 
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #7:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_7B())))
      }
    })    
    
    #Bed 8 patient 8~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_8 <- eventReactive(data_sample$SUBJECT_ID == "66818",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "66818")
      dat2 <- dat$LOS
      dat2
    })
    Pat_8B <- eventReactive(data_sample$SUBJECT_ID == "66818",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "66818")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_8C <- eventReactive(data_sample$SUBJECT_ID == "66818",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "66818")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    Pat_8D <- eventReactive(data_demo$SUBJECT_ID == "66818",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_demo, data_demo$SUBJECT_ID == "66818")
      dat2 <- dat$GENDER_SEX
      dat2
    })
    
    Pat_8E <- eventReactive(data_demo$SUBJECT_ID == "66818",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_demo, data_demo$SUBJECT_ID == "66818")
      dat2 <- dat$DOB
      dat2
    })
    
    Pat_8F <- eventReactive(data_demo$SUBJECT_ID == "66818",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_demo, data_demo$SUBJECT_ID == "66818")
      dat2 <- dat$ADMISSION_TYPE
      dat2
    })
    
    Pat_8G <- eventReactive(data_demo$SUBJECT_ID == "66818",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_demo, data_demo$SUBJECT_ID == "66818")
      dat2 <- dat$MARITAL_STATUS
      dat2
    })
    
    Pat_8H <- eventReactive(data_demo$SUBJECT_ID == "66818",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_demo, data_demo$SUBJECT_ID == "66818")
      dat2 <- dat$ETHNICITY
      dat2
    })
    
    
    
    output$Bed_8 <- renderUI({  
      x<- Pat_8()
      if(x =="Green"){gradientBox("Patient ID:", Pat_8C(), "LOS (days):", Pat_8B(),
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 16px;","Bed #8:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_8B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_8C(), "LOS (days):", Pat_8B(), 
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 16px;","Bed #8:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_8B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_8C(), "LOS (days):", Pat_8B(),  
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 16px;","Bed #8:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_8B())))
      }
      else {
        gradientBox(HTML(paste("Patient ID:", Pat_8C(), br(),"Latest LOS in days:", Pat_8B())),
                    width = 3,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 16px;","Bed #8:"),
                    footer = HTML(paste("Sex:",Pat_8D(), br(),
                                        "Birthdate:",Pat_8E(), br(),
                                        "Admission Type:",Pat_8F(),
                                        "Martital Status:",Pat_8G(),
                                        "Ethnicity:",Pat_8H())))
      }
    })    
    
    
    #Bed 9 patient 9~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_9 <- eventReactive(data_sample$SUBJECT_ID == "68865",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "68865")
      dat2 <- dat$LOS
      dat2
    })
    Pat_9B <- eventReactive(data_sample$SUBJECT_ID == "68865",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "68865")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_9C <- eventReactive(data_sample$SUBJECT_ID == "68865",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "68865")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    
    output$Bed_9 <- renderUI({  
      x<- Pat_9()
      if(x =="Green"){gradientBox("Patient ID:", Pat_9C(), 
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #9:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_9B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_9C(), 
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #9:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_9B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_9C(), 
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #9:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_9B())))
      }
      else {
        gradientBox("Patient ID:", Pat_9C(), 
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #9:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_9B())))
      }
    })    
    
    #Bed 10 patient 10~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_10 <- eventReactive(data_sample$SUBJECT_ID == "98643",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "98643")
      dat2 <- dat$LOS
      dat2
    })
    Pat_10B <- eventReactive(data_sample$SUBJECT_ID == "98643",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "98643")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_10C <- eventReactive(data_sample$SUBJECT_ID == "98643",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_sample, data_sample$SUBJECT_ID == "98643")
      dat2 <- dat$SUBJECT_ID
      dat2
    })
    
    
    output$Bed_10 <- renderUI({  
      x<- Pat_10()
      if(x =="Green"){gradientBox("Patient ID:", Pat_10C(), 
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #10:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_10B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_10C(), 
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #10"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_10B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_10C(),   
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #10:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_10B())))
      }
      else {
        gradientBox("Patient ID:", Pat_10C(), 
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #10:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_10B())))
      }
    })  

    
    # Run the application 
}

shinyApp(ui = ui, server = server)
    
