library(shiny)
library(tidyverse)
library(shinythemes)
require(DT)
require(shiny)
require(shinyWidgets)
require(shinydashboard)

dat1 <- read.csv("Data/2017-11-08 Rnitro.csv",sep=';')
dat2 <- read.csv("Data/2017-09-03 Datos_thevetia .csv",sep=';')

shinyUI(
  
  navbarPage(theme = shinytheme("yeti"),
    img(src='AO.png', align = "left"),
    tabPanel('Seleccione los datos',
             tags$style(type = "text/css", ".navbar-brand {padding-top: 0px!important; padding-left: 0px!important;}"),
             tags$style(type = "text/css", "#normality_icon {text-align: center !important;}"),
             tags$style(type = "text/css", "#trans_normality_icon {text-align: center !important;}"),
             tags$style(type = "text/css", "#homo_icon {text-align: center !important;}"),
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Seleccione los datos", choices=c('dat1','dat2'), 
                             selected = "dat1"),
                 radioButtons("choice","Seleccione una opción", choices=c("Datos" = 1, "Estructura" = 2,
                                                                          "Resumen" = 3 )),
                 
                 hr(),
                 uiOutput("response_selector"),
                 uiOutput("predictors_selector"),
                 hr()
                 
                 
               ),
               mainPanel(
                 
                 conditionalPanel(condition="input.choice==1", DT::dataTableOutput('dat')),
                 conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
                 conditionalPanel(condition="input.choice==3", verbatimTextOutput("summary"))
                 
               )
             )
             ),
        tabPanel('Normalidad',
                 sidebarLayout(
                   sidebarPanel(
                    htmlOutput('test_results_table'),
                     htmlOutput('normality_icon'),
                     hr(),
                    uiOutput('transformation_selector'),
                    uiOutput('denominator_selector'),
                    uiOutput('trans_test_results_table'),
                    htmlOutput('trans_normality_icon'),
                    uiOutput('trans_warning'),
                    
                    hr()
                     
                     
                   ), 
                   mainPanel(
                     plotOutput("QQ_plot"),
                     plotOutput("hist")
                   )
                 )
          
        ), 
    
    
    tabPanel('Homocedasticidad',
             sidebarLayout(
               sidebarPanel(
                 # htmlOutput('test_results_table'),
                 #htmlOutput('normality_icon'),
                 hr()
                 
                 
               ), 
               mainPanel(
                 #plotOutput("QQ_plot")
                 
               )
             )
             
    ),
    
    tabPanel('ANOVA',
             sidebarLayout(
               sidebarPanel(
                 # htmlOutput('test_results_table'),
                 #htmlOutput('normality_icon'),
                 hr()
                 
                 
               ), 
               mainPanel(
                 #plotOutput("QQ_plot")
                 
               )
             )
             
    ), 
    
    tabPanel('Comparaciones múltiples',
             sidebarLayout(
               sidebarPanel(
                 # htmlOutput('test_results_table'),
                 #htmlOutput('normality_icon'),
                 hr()
                 
                 
               ), 
               mainPanel(
                 #plotOutput("QQ_plot")
                 
               )
             )
             
    )
      
    )
    
          
  )
