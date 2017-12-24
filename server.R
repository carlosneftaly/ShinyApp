library(shiny)
library(tidyverse)
library(shinythemes)
require(DT)
require(shiny)
require(shinyWidgets)
require(shinydashboard)

dat1 <- read.csv("Data/2017-11-08 Rnitro.csv",sep=';')
dat2 <- read.csv("Data/2017-09-03 Datos_thevetia .csv",sep=';')

shinyServer(function(input,output)({
  
  
  
  data <- reactive({
    get(input$dataset)
  })
  
  ## to output the dataset
  #output$dat <- renderPrint({
  # data()
  #})
  
  output$dat<-DT::renderDataTable({
    data_table <- DT::datatable(data())
    data_table
  })
  
  
  # to output the structure of the dataset
  output$struct <- renderPrint({
    str(get(input$dataset))
  })
  
  # for summary
  output$summary <- renderPrint({
    summary(get(input$dataset))
  })
  
  output$response_selector <- renderUI({
    
   
      pickerInput(
        inputId = "selected_response", "Seleccion la variable respuesta (Y)",
        choices = colnames(data()),
        selected = NULL,
        multiple = F, options = list('actions-box' = TRUE, title = "Click here", 'deselect-all-text' = "None", 'select-all-text' = "All"),
        width = '80%'
      )
    
    
  })
    
    
    
    output$predictors_selector <- renderUI({
      
      
        pickerInput(
          inputId = "selected_predictors", "Seleccione el factor (X)", 
          choices = colnames(data())[!colnames(data()) %in% input$selected_response],
          selected = NULL,
          multiple = T, options = list('actions-box' = TRUE, title = "Click here", 'deselect-all-text' = "None", 'select-all-text' = "All"),
          width = '80%'
        )
      
      
      
      
    })
    
    # Define predictors
    
    predictors <- reactive({
      
      inPred <- input$selected_predictors
      
     
      
    })
    
    # Define response
    
    response <- reactive({
      
      inResp <- input$selected_response
      
      
      
    })
    
  
    output$QQ_plot <- renderPlot({
      resp_col <- which(colnames(data()) == response())
      
      
        dataPlot <- data()[,resp_col]
      
      
      qqnorm(dataPlot,  main="QQ plot", pch = 16, col = "#6fcb9f60", las = 1, bty = "n", ylim = c(min(dataPlot, na.rm = T), max(dataPlot, na.rm = T)))		
      qqline(dataPlot, col="#ff7f50", lwd=3, lty=2, las = 1)
      
    })
    
    
    # Histogram
    
    
    output$hist <- renderPlot({
      resp_col <- which(colnames(data()) == response())
      
      
        dataPlot <- data()[,resp_col]
      
      
      h <- hist(dataPlot, col = "#6fcb9f80", border = "#6fcb9f", las = 1, 
                xlab = as.character(response()), main = paste0("Histograma of ", as.character(response())))
      d <- density(dataPlot)
      
      xs <- d$x
      ys <- d$y * length(dataPlot) * diff(h$breaks)[1]
      
      lines(x = xs, y = ys, lwd = 4, col = "#ff7f5090")
      polygon(xs, ys, col = "#ff7f5020", border = NA)
      
    })
      
    
    # Test de normalidad
    
    
    test_results <- reactive({
      
      
      
      resp_col <- which(colnames(data()) == response())
      
      shapiro.test(data()[,resp_col])
      
      
    })
    
    output$test_results_table <- renderUI({
      
      
      
      
      HTML(paste0(
        "<p  style='font-size:150%;text-align:center;'><b>", response(),"<br>",
        test_results()$method, '</b></p><br>',
        "<p  style='font-size:100%;text-align:center;'>", names(test_results()$statistic), " statistic: ", formatC(test_results()$statistic), '<br> ',
        "p-value: ", formatC(test_results()$p.value), "</p>"
      ))
      
    })
    
    
    output$normality_icon <- renderUI({
      
      icon(ifelse(test_results()$p.value > 0.05, "thumbs-o-up", "thumbs-o-down"), "fa-3x")
      
    })
    
    
    

  
})
)