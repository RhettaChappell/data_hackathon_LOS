#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD IN REQUIRED LIBRARIES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)

# DATA and VISUALISATIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggrepel)
library(forcats)
library(tidyverse)
library(plotly)
library(data.table)
library(reshape2)
library(lubridate)
library(scales)
library(htmlTable)
# SPATIAL:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(rgdal) 
library(raster)
library(leaflet)
library(sp)
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
#fake data 
data<-read.csv("data/Fake_Data_Patient_LOS.csv")
#check
str(data)
Current_Time <- today()
data_2 <- data %>% 
  #set data types
  mutate(Admit_Date = as.Date(Admit_Date, format = "%d/%m/%Y"),
         Initial_LOS_prediction = as.Date(Initial_LOS_prediction, format = "%d/%m/%Y"),
         Latest_LOS_prediction = as.Date(Latest_LOS_prediction, format = "%d/%m/%Y"),
         Current_Time = Sys.Date(),
         Medication_flag = as.factor(Medication_flag),
         SEX = as.factor(SEX),
         Patient_ID = as.factor(Patient_ID)) %>% 
  #create a variable which stores the actual LOS per patient
  group_by(Patient_ID) %>% 
  mutate(Actual_LOS = Current_Time - Admit_Date,
         Initial_prediction_days = Initial_LOS_prediction - Admit_Date,
         Latest_prediction_days = Latest_LOS_prediction - Initial_LOS_prediction,
         Latest_prediction_days = gsub("0", Initial_prediction_days, Latest_prediction_days)) %>% 
  ungroup() %>% 
  group_by(Patient_ID) %>% 
  #LOGIC - to colour the info boxes
  #Green = Most recent LOS prediction is shorter (in time/ length) than the initial LOS prediction and the actual LOS time is shorter than the most recent prediction
  #Yellow = Most recent LOS prediction is longer (in time/ length) than the initial LOS prediction and the actual LOS time is shorter than the most recent prediction
  #Red = the actual LOS is greater (in time/ length) than the most recent prediction
  #Grey = Most recent LOS prediction is  equal (in time/length) to the initial LOS prediction and the actual LOS time is shorter than the most recent prediction
  mutate(LOS_PRED_STATUS = case_when(
                           #need to set the difftime objects to numeric as we need to compare them as numbers
                           as.numeric(Latest_prediction_days) <= as.numeric(Initial_prediction_days) & as.numeric(Actual_LOS) <= as.numeric(Latest_prediction_days)  ~ "Green",
                           as.numeric(Latest_prediction_days) <= as.numeric(Initial_prediction_days) & as.numeric(Actual_LOS) <= as.numeric(Latest_prediction_days) ~ "Grey",
                           as.numeric(Latest_prediction_days) >= as.numeric(Initial_prediction_days) & as.numeric(Actual_LOS)  <= as.numeric(Latest_prediction_days) ~ "Yellow",
                           as.numeric(Actual_LOS)  > as.numeric(Latest_prediction_days)  ~ "Red"
  ))





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
    
    Pat_1 <- eventReactive(data_2$Patient_ID == "1_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "1_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    
    Pat_1B <- eventReactive(data_2$Patient_ID == "1_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "1_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_1C <- eventReactive(data_2$Patient_ID == "1_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "1_ID")
      dat2 <- dat$Patient_ID
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
    
    Pat_2 <- eventReactive(data_2$Patient_ID == "2_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "2_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_2B <- eventReactive(data_2$Patient_ID == "2_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "2_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_2C <- eventReactive(data_2$Patient_ID == "2_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "2_ID")
      dat2 <- dat$Patient_ID
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
    
    Pat_3 <- eventReactive(data_2$Patient_ID == "3_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "3_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_3B <- eventReactive(data_2$Patient_ID == "3_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "3_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_3C <- eventReactive(data_2$Patient_ID == "3_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "3_ID")
      dat2 <- dat$Patient_ID
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
    
    Pat_4 <- eventReactive(data_2$Patient_ID == "4_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "4_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_4B <- eventReactive(data_2$Patient_ID == "4_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "4_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_4C <- eventReactive(data_2$Patient_ID == "4_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "4_ID")
      dat2 <- dat$Patient_ID
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
    
    Pat_5 <- eventReactive(data_2$Patient_ID == "5_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "5_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_5B <- eventReactive(data_2$Patient_ID == "5_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "5_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_5C <- eventReactive(data_2$Patient_ID == "5_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "5_ID")
      dat2 <- dat$Patient_ID
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
    
    Pat_6 <- eventReactive(data_2$Patient_ID == "6_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "6_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_6B <- eventReactive(data_2$Patient_ID == "6_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "6_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    Pat_6C <- eventReactive(data_2$Patient_ID == "6_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "6_ID")
      dat2 <- dat$Patient_ID
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
    
    Pat_7 <- eventReactive(data_2$Patient_ID == "7_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "7_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_7B <- eventReactive(data_2$Patient_ID == "7_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "7_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_7C <- eventReactive(data_2$Patient_ID == "7_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "7_ID")
      dat2 <- dat$Patient_ID
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
    
    Pat_8 <- eventReactive(data_2$Patient_ID == "8_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "8_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_8B <- eventReactive(data_2$Patient_ID == "8_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "8_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_8C <- eventReactive(data_2$Patient_ID == "8_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "8_ID")
      dat2 <- dat$Patient_ID
      dat2
    })
    
    
    output$Bed_8 <- renderUI({  
      x<- Pat_8()
      if(x =="Green"){gradientBox("Patient ID:", Pat_8C(), 
                                  gradientColor = "maroon",
                                  width = 2,
                                  collapsible = FALSE,
                                  icon = "fa fa-bed",
                                  title = tags$p(style = "font-size: 18px;","Bed #8:"),
                                  footer = HTML(paste("Latest LOS prediction:", Pat_8B())))
      }
      else if ( x == "Yellow"){
        gradientBox("Patient ID:", Pat_8C(), 
                    gradientColor = "yellow",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #8:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_8B())))
      }
      else if ( x == "Red"){
        gradientBox("Patient ID:", Pat_8C(), 
                    gradientColor = "red",
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    title = tags$p(style = "font-size: 18px;","Bed #8:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_8B())))
      }
      else {
        gradientBox("Patient ID:", Pat_8C(), 
                    width = 2,
                    collapsible = FALSE,
                    icon = "fa fa-bed",
                    gradientColor = "purple",
                    title = tags$p(style = "font-size: 18px;","Bed #8:"),
                    footer = HTML(paste("Latest LOS prediction:", Pat_8B())))
      }
    })    
    
    
    #Bed 9 patient 9~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    Pat_9 <- eventReactive(data_2$Patient_ID == "9_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "9_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_9B <- eventReactive(data_2$Patient_ID == "9_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "9_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_9C <- eventReactive(data_2$Patient_ID == "9_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "9_ID")
      dat2 <- dat$Patient_ID
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
    
    Pat_10 <- eventReactive(data_2$Patient_ID == "10_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "10_ID")
      dat2 <- dat$LOS_PRED_STATUS
      dat2
    })
    Pat_10B <- eventReactive(data_2$Patient_ID == "10_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "10_ID")
      dat2 <- dat$Latest_LOS_prediction
      dat2
    })
    
    Pat_10C <- eventReactive(data_2$Patient_ID == "10_ID",  { 
      #store the subsetted data in an object  - we will use this later to return a list of colours
      dat <- subset(data_2, data_2$Patient_ID == "10_ID")
      dat2 <- dat$Patient_ID
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
    
