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
      
      if(input$selected_transformation == 'none'){
        dataPlot <- data()[,resp_col]
      }else{
        dataPlot <- transformedData()
      }
      
      if (is.null(dataPlot))
        return(NULL)
      
      if(sum(is.na(dataPlot)) > 1)
        return(NULL)
      
      qqnorm(dataPlot,  main="QQ plot", pch = 16, col = "#6fcb9f60", las = 1, bty = "n", ylim = c(min(dataPlot, na.rm = T), max(dataPlot, na.rm = T)))		
      qqline(dataPlot, col="#ff7f50", lwd=3, lty=2, las = 1)
      
    })
    
    
    
    # Histogram
    
    
    output$hist <- renderPlot({
      resp_col <- which(colnames(data()) == response())
      
      if(input$selected_transformation == 'none'){
        dataPlot <- data()[,resp_col]
      }else{
        dataPlot <- transformedData()
      }
      
      if (is.null(dataPlot))
        return(NULL)
      
      if(sum(is.na(dataPlot)) > 1)
        return(NULL)
      
      
      h <- hist(dataPlot, col = "#6fcb9f80", border = "#6fcb9f", las = 1, 
             xlab = as.character(response()), main = paste0("Histograma de ", as.character(response())))
      d <- density(dataPlot)
      
      xs <- d$x
      ys <- d$y * length(dataPlot) * diff(h$breaks)[1]
      
      lines(x = xs, y = ys, lwd = 4, col = "#ff7f5090")
      polygon(xs, ys, col = "#ff7f5020", border = NA)
      
    })
    
    
    # Normality test
    
    
    test_results <- reactive({
      
      if (is.null(data()))
        return(NULL)
      
      resp_col <- which(colnames(data()) == response())
      
      shapiro.test(data()[,resp_col])
      
      
    })
    
    
    
    output$test_results_table <- renderUI({
      
      
      
      
      HTML(paste0(
        "<p  style='font-size:150%;text-align:center;'><b>", response(),"<br>",
        test_results()$method, '</b></p><br>',
        "<p  style='font-size:100%;text-align:center;'>", names(test_results()$statistic), " Estadístico : ", formatC(test_results()$statistic), '<br> ',
        "Valor P: ", formatC(test_results()$p.value), "</p>"
      ))
      
    })
    
    
    output$normality_icon <- renderUI({
      
      icon(ifelse(test_results()$p.value > 0.05, "thumbs-o-up", "thumbs-o-down"), "fa-3x")
      
    })
    
    
    # Transformations
    
    # Choose the transformation
    
    output$transformation_selector <- renderUI({
      
   
      
      pickerInput(
        inputId = "selected_transformation", "Seleccione una transformación",
        choices = list(None = 'none', 'Square root' = 'sqrt', 'Natural log + 1' = 'log'),
        selected = 'none',
        multiple = F, options = list('actions-box' = TRUE, title = "Click aquí"),
        width = '80%'
      )
      
      
    })
    
    
    
    # Calculate the transformed data
    
    transformedData <- reactive({
      resp_col <- which(colnames(data()) == response())
      if(input$selected_transformation == 'none'){
        
        as.numeric(data()[,resp_col])
      }else if(input$selected_transformation == 'sqrt'){
        
        sqrt(as.numeric(data()[,resp_col]))
      }else if(input$selected_transformation == 'log'){
        
        log(as.numeric(data()[,resp_col] + 1))
      }
      
    })
    
    
    # Calculate the transformed data normality test
    
    
    trans_test_results <- reactive({
      
      if (input$selected_transformation == 'none'){
        return(NULL)
      }
      
      if(is.na(transformedData()[1]))
        return(NULL)
      
      shapiro.test(transformedData())
      
    })
    
    
    output$trans_test_results_table <- renderUI({
      
      if(input$selected_transformation == 'none'){
        return(NULL)
      }else{
        HTML(paste0(
          "<p  style='font-size:150%;text-align:center;'><b> Transformed ", response(),"<br>",
          trans_test_results()$method, '</b></p><br>',
          "<p  style='font-size:100%;text-align:center;'>", names(trans_test_results()$statistic), " Estadístico: ", formatC(trans_test_results()$statistic), '<br> ',
          " Valor P: ", formatC(trans_test_results()$p.value), "</p>"
        ))
      }
      
    })
    
    
    output$trans_normality_icon <- renderUI({
      if(input$selected_transformation == ''){
        return(NULL)
      }else{
        icon(ifelse(trans_test_results()$p.value > 0.05, "thumbs-o-up", "thumbs-o-down"), "fa-3x")
      }
    })
    
    output$trans_warning <- renderUI({
      
      if(sum(is.na(transformedData())) > 1){
        
        showModal(modalDialog(
          title = "Warning!",
          HTML("This is probably not an appropriate transformation for your response variable.<br>
           This transformation is recommended only when data are ratios or percentages, and only after selecting an appropriate denominator (e.g. 100 in case of a percentage)"),
          easyClose = TRUE
        ))
        
      }else{
        NULL
      }
      
      
    })
    
    
    
    

  
})
)